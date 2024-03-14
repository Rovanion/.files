local compat    = require 'compat'
local awful     = require 'awful'
local beautiful = require 'beautiful'
local naughty   = require 'naughty'
local wibox     = require 'wibox'
utils = {}

function utils.error(message)
   naughty.notify({preset = naughty.config.presets.critical,
                   text   = message})
end

function utils.recursive_print(s, l, i) -- recursive Print (structure, limit, indent)
   l = (l) or 100; i = i or "";-- default item limit, indent string
   if (l<1) then print "ERROR: Item limit reached."; return l-1 end;
   local ts = type(s);
   if (ts ~= "table") then print (i,ts,s); return l-1 end
   print (i,ts);           -- print "table"
   for k,v in pairs(s) do  -- print "[KEY] VALUE"
      l = utils.recursive_print(v, l, i.."\t["..tostring(k).."]");
      if (l < 0) then break end
   end
   return l
end


--- 'memoize' a function (cache returned value for next call).
-- This is useful if you have a function which is relatively expensive,
-- but you don't know in advance what values will be required, so
-- building a table upfront is wasteful/impossible.
-- @param func a function of at least one argument
-- @return a function with at least one argument, which is used as the key.
function utils.memoize(func)
   local cache = {}
   return function(k)
      local res = cache[k]
      if res == nil then
         res = func(k)
         cache[k] = res
      end
      return res
   end
end

local function _string_lambda(f)
   if f:find '^|' or f:find '@' then
      local args,body = f:match '|([^|]*)|(.+)'
      if f:find '@' then
         args = '__arg__'
         body = f:gsub('@','__arg__')
      else
         if not args then return utils.error 'bad string lambda' end
      end
      local fstr;
      if f:find('=') then
         fstr = 'return function('..args..') '..body..' end'
      else
         fstr = 'return function('..args..') return '..body..' end'
      end
      local fn,err = compat.load(fstr)
      if not fn then return utils.error(err) end
      fn = fn()
      return fn
   else return utils.error(f .. 'is not a string lambda')
   end
end

--- an anonymous function as a string. This string is either of the form
-- '|args| expression' or is a function of one argument, '_'
-- @param lf function as a string
-- @return a function
-- @usage string_lambda '|x|x+1' (2) == 3
-- @usage string_lambda '_+1' (2) == 3
-- @function string_lambda
utils.string_lambda = utils.memoize(_string_lambda)


function utils.run_if_not_running(program, arguments)
  if arguments == nil then
    arguments = ""
  end
   awful.spawn.easy_async(
      "pgrep " .. program,
      function(stdout, stderr, reason, exit_code)
         if exit_code ~= 0 then
           naughty.notify({ text = "Spawned " .. program .. " " .. arguments })
           awful.spawn.easy_async(program .. " " .. arguments,
           function(stdout, stderr, reason, exit_code)
             naughty.notify({ text = "Spawned " .. program .. " " .. arguments " with exitcode " .. exit_code .. " and stdout " .. stdout })
           end)
         end

   end)

end


function utils.create_titlebar(c, titlebar_buttons, titlebar_position, titlebar_size)
  awful.titlebar(c, {font = beautiful.titlebar_font, position = titlebar_position, size = titlebar_size}) :
    setup {
      { buttons = titlebar_buttons,
        layout  = wibox.layout.fixed.horizontal },
      { buttons = titlebar_buttons,
        layout  = wibox.layout.fixed.horizontal },
      { buttons = titlebar_buttons,
        layout = wibox.layout.fixed.horizontal },
      layout = wibox.layout.align.horizontal
    }
end

return utils
