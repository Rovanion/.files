-- {{{ Autostart
--Autostart scripting
function run_once(prg,arg_string,pname,screen)
    if not prg then
        do return nil end
    end

    if not pname then
       pname = prg
    end

    if not arg_string then
        awful.util.spawn_with_shell("pgrep -f -u $USER -x '" .. pname .. "' || (" .. prg .. ")",screen)
    else
        awful.util.spawn_with_shell("pgrep -f -u $USER -x '" .. pname .. "' || (" .. prg .. " " .. arg_string .. ")",screen)
    end
end

run_once("bash ~/.config/egenSkit/mouseAndKeyboard.bash")
run_once("volumeicon &")
run_once("nm-applet &")
run_once("xscreensaver -no-splash &")
run_once("nice -n 19 conky &")
run_once("nice -n 19 dropbox start &")
run_once("nice -n 19 redshift")

--}}}
