-- Standard awesome library
local gears = require("gears")
-- This is is an ugly hack, but: Awful is assigned a global name here
-- for the anonymous functions declared with th fn'' syntax can use it.
awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup").widget
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

-- Utility functions
local utils = require("utils")
local fn = utils.string_lambda
local prn = utils.recursive_print

-- Battery level indicator
if gears.filesystem.dir_readable('/sys/class/power_supply/BAT0') then
  battery = require("battery-widget")(
    {adapter = "BAT1",
     ac_prefix = "🔌",
     battery_prefix = "🔋",
     widget_text = "  ${AC_BAT} ${color_on}${percent}%${color_off} ",
     listen = true,
     limits = {
       {10, "red"   },
       {25, "orange"},
       {50, "white" }}})
else
  battery = {}
end

-- Window management functions
function suspend_machine ()
  awful.util.spawn('physlock')
  awful.util.spawn('dbus-send --print-reply --system --dest=org.freedesktop.login1 /org/freedesktop/login1 org.freedesktop.login1.Manager.Suspend boolean:true')
end

function restore_minimized_window()
   local c = awful.client.restore()
   -- Focus restored client
   if c then
      client.focus = c
      c:raise()
   end
end

function open_lua_prompt()
   awful.prompt.run {
      prompt       = "Run Lua code: ",
      textbox      = awful.screen.focused().mypromptbox.widget,
      exe_callback = awful.util.eval,
      history_path = awful.util.get_cache_dir() .. "/history_eval"
   }
end

function unmaximize_client(c)
   c.maximized            = false
   c.maximized_horizontal = false
   c.maximized_vertical   = false
   c.fullscreen           = false
   c.floating             = false
   c.ontop                = false
   c:lower()
end

function switch_between_two_clients()
  awful.client.focus.history.previous()
  if client.focus then
    client.focus:raise()
  end
end

function toggle_fullscreen(c)
   c.fullscreen = not c.fullscreen
   if c.fullscreen then c:raise() else c:lower() end
end

function take_screenshot()
  awful.spawn("scrot --select Pictures/scrot/%Y-%m-%d %H.%M.%S.png")
end

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init("~/.config/awesome/themes/zenburn/theme.lua")

-- This is used later as the default terminal and editor to run.
terminal = "urxvt"
editor = os.getenv("EDITOR") or "editor"
editor_cmd = terminal .. " -e " .. editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
    awful.layout.suit.floating,
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
    awful.layout.suit.spiral,
    awful.layout.suit.spiral.dwindle,
    awful.layout.suit.max,
    awful.layout.suit.max.fullscreen,
    awful.layout.suit.magnifier,
    awful.layout.suit.corner.nw,
    -- awful.layout.suit.corner.ne,
    -- awful.layout.suit.corner.sw,
    -- awful.layout.suit.corner.se,
}
-- }}}

-- {{{ Helper functions
local function client_menu_toggle_fn()
    local instance = nil

    return function ()
        if instance and instance.wibox.visible then
            instance:hide()
            instance = nil
        else
            instance = awful.menu.clients({ theme = { width = 250 } })
        end
    end
end
-- }}}

-- {{{ Menu
-- Create a launcher widget and a main menu
myawesomemenu = {
   { "hotkeys", function() return false, hotkeys_popup.show_help end},
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", function() awesome.quit() end}
}

local menu_awesome  = { "awesome", myawesomemenu, beautiful.awesome_icon }
local menu_terminal = { "open terminal", terminal }
local menu_dvorak   = { "Dvorak", ",configure-mouse-and-keyboard"}
local menu_qwerty   = { "qwerty", "setxkbmap se"}

mymainmenu = awful.menu({
      items = {
         menu_awesome,
         -- { "Debian", debian.menu.Debian_menu.Debian },
         menu_terminal,
         menu_dvorak,
         menu_qwerty}})



mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

-- {{{ Wibar
-- Create a textclock widget
mytextclock = wibox.widget.textclock(" %Y-%m-%d %H:%M ")

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
                    awful.button({ }, 1, function(t) t:view_only() end),
                    awful.button({ modkey }, 1, function(t)
                                              if client.focus then
                                                  client.focus:move_to_tag(t)
                                              end
                                          end),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, function(t)
                                              if client.focus then
                                                  client.focus:toggle_tag(t)
                                              end
                                          end),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
                )

local tasklist_buttons = gears.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() and c.first_tag then
                                                      c.first_tag:view_only()
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, client_menu_toggle_fn()),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                          end))

local function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.maximized(wallpaper, s, true)
    end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
    -- Wallpaper
    set_wallpaper(s)

    local ls = awful.layout.layouts;
    local layout_map = { ls[2], ls[2], ls[2], ls[2], ls[2], ls[1], ls[2], ls[2], ls[2], ls[2] }
    -- Each screen has its own tag table.
    awful.tag({ "web", "work", "remote", "music", "chat", "float", "pass", "8", "9" }, s, layout_map)

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(gears.table.join(
                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons)

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, tasklist_buttons)

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "bottom", screen = s })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            mylauncher,
            s.mytaglist,
            s.mypromptbox,
        },
        s.mytasklist, -- Middle widget
        { -- Right widgets
           layout = wibox.layout.fixed.horizontal,
           battery.widget,
           mykeyboardlayout,
           wibox.widget.systray(),
           mytextclock,
           s.mylayoutbox,
        },
    }
end)
-- }}}

-- {{{ Mouse bindings
root.buttons(gears.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 4, awful.tag.viewnext),
    awful.button({ }, 5, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = gears.table.join(
    awful.key({ modkey,         }, "Tab",        switch_between_two_clients,                     {description = "go back",                   group = "client"}),
    awful.key({ modkey, "Shift" }, "h",          hotkeys_popup.show_help,                        {description = "show help",                 group = "awesome"}),
    awful.key({ modkey,         }, "Left",       awful.tag.viewprev,                             {description = "previous tag",              group = "tag"}),
    awful.key({ modkey,         }, "Right",      awful.tag.viewnext,                             {description = "previous tag",              group = "tag"}),
    awful.key({ modkey,         }, "Escape",     awful.tag.history.restore,                      {description = "go back",                   group = "tag"}),
    -- Switching window focus
    awful.key({ modkey,         }, "n",          fn'||awful.client.focus.byidx( 1)',             {description = "next window",               group = "client"}),
    awful.key({ modkey,         }, "p",          fn'||awful.client.focus.byidx(-1)',             {description = "previous window",           group = "client"}),
    -- Handle minimized windows
    awful.key({ modkey, "Mod1"  }, "n",          restore_minimized_window,                       {description = "restore minimized",         group = "client"}),
    -- Screenshot
    awful.key({                 }, "Print",      nil, take_screenshot,                           {description = "take screenshot",           group = "launcher"}),
    awful.key({ modkey,         }, "c",          nil, take_screenshot,                           {description = "take screenshot",           group = "launcher"}),
    -- Move window in the stack
    awful.key({ modkey,         }, "aring",      fn'||awful.client.swap.byidx(-1)',              {description = "move window up stack",      group = "client"}),
    awful.key({ modkey,         }, "odiaeresis", fn'||awful.client.swap.byidx( 1)',              {description = "move window down stack",    group = "client"}),
    -- Switch screens
    awful.key({ modkey,         }, "o",          fn'||awful.screen.focus_relative(-1)',          {description = "focus the previous screen", group = "screen"}),
    -- Launching programs
    awful.key({ modkey,         }, "Return",     fn'||awful.spawn(terminal)',                    {description = "open a terminal",           group = "launcher"}),
    awful.key({ modkey, "Shift" }, "f",          fn'||awful.spawn("firefox")',                   {description = "open firefox",              group = "launcher"}),
    awful.key({ modkey, "Shift" }, "t",          fn'||awful.spawn("thunar")',                    {description = "open thunar",               group = "launcher"}),
    awful.key({ modkey, "Shift" }, "n",          fn'||awful.spawn("nautilus")',                  {description = "open nautilus",             group = "launcher"}),
    awful.key({ modkey, "Shift" }, "odiaeresis", fn'||awful.spawn("evince")',                    {description = "open evince",               group = "launcher"}),
    awful.key({ modkey, "Shift" }, "d",          fn'||awful.spawn("dolphin")',                   {description = "open dolphin",              group = "launcher"}),
    -- Window size
    awful.key({ modkey,         }, "a",          fn'||awful.tag.incmwfact(-0.05)',               {description = "increase main width",       group = "layout"}),
    awful.key({ modkey,         }, "e",          fn'||awful.tag.incmwfact( 0.05)',               {description = "decrease main width",       group = "layout"}),
    awful.key({ modkey, "Shift" }, "ä",          fn'||awful.client.incwfact( 0.05)',             {description = "increase window height",    group = "layout"}),
    awful.key({ modkey, "Shift" }, "o",          fn'||awful.client.incwfact(-0.05)',             {description = "decrease window height",    group = "layout"}),
    -- Window juggling
    awful.key({ modkey, "Shift" }, "a",          fn'||awful.tag.incnmaster( 1, nil, true)',      {description = "more main windows",         group = "layout"}),
    awful.key({ modkey, "Shift" }, "e",          fn'||awful.tag.incnmaster(-1, nil, true)',      {description = "fewer main windows",        group = "layout"}),
    awful.key({ modkey, "Mod1"  }, "a",          fn'||awful.tag.incncol( 1, nil, true)',         {description = "more columns",              group = "layout"}),
    awful.key({ modkey, "Mod1"  }, "e",          fn'||awful.tag.incncol(-1, nil, true)',         {description = "fewer columns",             group = "layout"}),
    -- Switch layout
    awful.key({ modkey,         }, "space",      fn'||awful.layout.inc( 1)',                     {description = "select next",               group = "layout"}),
    awful.key({ modkey, "Shift" }, "space",      fn'||awful.layout.inc(-1)',                     {description = "select previous",           group = "layout"}),
    -- Lifecycle
    awful.key({ modkey, "Mod1"  }, "r",          awesome.restart,                                {description = "reload awesome",            group = "awesome"}),
    awful.key({ modkey, "Mod1"  }, "q",          awesome.quit,                                   {description = "quit awesome",              group = "awesome"}),
    -- Power management
    awful.key({                 }, "XF86Launch1",fn'||awful.util.spawn("xset dpms force off")',  {description = "turn off screen",           group = "power"}),
    awful.key({ modkey,         }, "s",          fn'||awful.util.spawn("physlock")',             {description = "lock screen",               group = "power"}),
    awful.key({ modkey, "Shift" }, "s",          suspend_machine,                                {description = "suspend machine",           group = "power"}),
    -- Prompt
    awful.key({ modkey,         }, "r",          fn'||awful.screen.focused().mypromptbox:run()', {description = "run prompt",                group = "launcher"}),
    awful.key({ modkey, "Shift" }, "r",          function() menubar.show() end,                  {description = "show the menubar",          group = "launcher"}),
    awful.key({ modkey,         }, "x",          open_lua_prompt,                                {description = "lua execute prompt",        group = "awesome"}),
    awful.key({ modkey,         }, "w",          fn'||mymainmenu:show()',                        {description = "show main menu",            group = "awesome"}),
    -- Clipboard manipulation
    awful.key({ modkey,         }, "u",          fn'||awful.spawn("swap-copy-buffers")',         {description = "swap primary and clipboard",group = "clipboard"}),
    -- Emacs
    awful.key({ modkey,         }, "j",          fn'||awful.spawn("emacsclient -e \'(emacs-everywhere)\'")', {description = "edit text with emacs",   group = "launcher"})
    )

clientkeys = gears.table.join(
    awful.key({ modkey,           }, ".",        toggle_fullscreen,                              {description = "toggle fullscreen",         group = "client"}),
    awful.key({ modkey,           }, "q",        fn'@:kill()',                                   {description = "close",                     group = "client"}),
    awful.key({ modkey, "Mod1"    }, "space",    awful.client.floating.toggle,                   {description = "toggle floating",           group = "client"}),
    awful.key({ modkey, "Mod1"    }, "Return",   fn'@:swap(awful.client.getmaster())',           {description = "move to master",            group = "client"}),
    awful.key({ modkey,         }, "adiaeresis", fn'@:move_to_screen()',                         {description = "move to next screen",       group = "client"}),
    awful.key({ modkey,           }, "t",        fn'@.ontop = not @.ontop',                      {description = "toggle keep on top",        group = "client"}),
    awful.key({ modkey,           }, "m",        unmaximize_client,                              {description = "unmaximize",                group = "client"}),
    awful.key({ modkey,           }, "z",        fn'@.minimized = true',                         {description = "minimize window",           group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = gears.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ modkey, "Mod1" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Mod1", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

clientbuttons = gears.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
    },

    -- Floating clients.
    { rule_any = {
        instance = {
          "DTA",  -- Firefox addon DownThemAll.
          "copyq",  -- Includes session name in class.
        },
        class = {
          "Arandr",
          "Gpick",
          "Kruler",
          "MessageWin",  -- kalarm.
          "Sxiv",
          "Wpa_gui",
          "pinentry",
          "veromix",
          "xtightvncviewer"},

        name = {
          "Event Tester",  -- xev.
        },
        role = {
          "AlarmWindow",  -- Thunderbird's calendar.
          "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
        }
      }, properties = { floating = true }},

    -- Add titlebars to normal clients and dialogs
    { rule_any = {type = { "normal", "dialog" }},
      properties = { titlebars_enabled = true }
    },

    -- Set Audacious to always map on the tag named "music" on screen 1.
    -- Use xprop to get the class of a window.
    { rule = { class = "Audacious" },    properties = { screen = 1, tag = "music" } },
    { rule = { class = "Signal" },       properties = { screen = 1, tag = "chat" } },
    { rule = { class = "Spotify" },      properties = { screen = 1, tag = "music" } },
    { rule = { class = "Spotube" },      properties = { screen = 1, tag = "music" } },
    { rule = { class = "KeePassXC" },    properties = { screen = 1, tag = "pass" } },
    { rule = { instance = "web-main" },  properties = { screen = 1, tag = "web" } },
    { rule = { instance = "work-main" }, properties = { screen = 1, tag = "work" } },
    { rule = { instance = "remote-main" },properties= { screen = 1, tag = "remote" } },
    { rule = { instance = "music-main" },properties = { screen = 1, tag = "music" } },
    { rule = { instance = "chat-main" }, properties = { screen = 1, tag = "chat" } },
    { rule = { instance = "float-main" },properties = { screen = 1, tag = "float" } },
    { rule = { instance = "pass-main" }, properties = { screen = 1, tag = "pass" } },
    { rule = { instance = "web" },       properties = { tag = "web" } },
    { rule = { instance = "work" },      properties = { tag = "work" } },
    { rule = { instance = "remote" },    properties = { tag = "remote" } },
    { rule = { instance = "music" },     properties = { tag = "music" } },
    { rule = { instance = "chat" },      properties = { tag = "chat" } },
    { rule = { instance = "float" },     properties = { tag = "float" } },
    { rule = { instance = "pass" },      properties = { tag = "pass" } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup and
      not c.size_hints.user_position and
      not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- We don't actually make any title bars, we make fake borders instead.
    utils.create_titlebar(c, nil, "bottom", 1)
    utils.create_titlebar(c, nil, "right",  1)
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
        and awful.client.focus.filter(c) then
        client.focus = c
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

-- If a client, i.e. Spotify, starts without a class, minimize it until it sets a class.
-- Otherwise our tag assignment rules won't work.
client.connect_signal("manage", function (c)
    -- Some applications (like Spotify) does not respect ICCCM rules correctly
    -- and redefine the window class property.
    -- This leads to having window which does *NOT* follow the user rules
    -- defined in the table `awful.rules.rules`.
    c:connect_signal("property::class", awful.rules.apply)

    awful.rules.apply(c)
end)
client.connect_signal("unmanage", function (c)
    c:disconnect_signal("property::class", awful.rules.apply)
end)

-- Run my autostart script
dofile(awful.util.getdir("config") .. "/" .. "autostart.lua")
