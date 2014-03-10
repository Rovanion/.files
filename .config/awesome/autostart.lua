-- {{{ Autostart
--Autostart scripting
function run_once(command, class, tag)
   -- create move callback
   local callback
   callback = function(c)
      if c.class == class then
	 awful.client.movetotag(tag, c)
	 client.remove_signal("manage", callback)
      end
   end
   client.add_signal("manage", callback)
   -- now check if not already running!
   local findme = command
   local firstspace = findme:find(" ")
   if firstspace then
      findme = findme:sub(0, firstspace-1)
   end
   -- finally run it
   awful.util.spawn_with_shell("pgrep -u $USER -x " .. findme .. " > /dev/null || (" .. command .. ")")
end

run_once("bash ~/.config/egenSkit/mouseAndKeyboard.bash")
run_once("volumeicon")
run_once("nm-applet")
run_once("xscreensaver -no-splash")
--run_once("nice -n 19 conky")
run_once("nice -n 19 dropbox start")
run_once("nice -n 19 redshift")
run_once("audacious", "Audacious", tags[1][4])
run_once("evolution", "Evolution", tags[1][3])
run_once("firefox", "Firefox", tags[1][4])
run_once(terminal , "Firefox", tags[1][1])
--}}}
