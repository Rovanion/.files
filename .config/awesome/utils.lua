require 'compat'
local naughty = require "naughty"
awful = require("awful")
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
		if f:find '^|' or f:find '_' then
        local args,body = f:match '|([^|]*)|(.+)'
        if f:find '_' then
            args = '_'
            body = f
        else
            if not args then return utils.error 'bad string lambda' end
        end
        local fstr = 'return function('..args..') return '..body..' end'
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

return utils
