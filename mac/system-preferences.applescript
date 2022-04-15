--  # Shamelessly lifted from https://stackoverflow.com/a/67974108
--
--  Check to see if System Preferences is
--  # running and if yes, then close it.
--  #
--  # This is done so the script will not fail
--  # if it is running and a modal sheet is
--  # showing, hence the use of 'killall'
--  # as 'quit' fails when done so, if it is.
--  #
--  # This is also done to allow default behaviors
--  # to be predictable from a clean occurrence.

if running of application "System Preferences" then
    try
        tell application "System Preferences" to quit
    on error
        do shell script "killall 'System Preferences'"
    end try
    delay 0.1
end if

--  # Make sure System Preferences is not running before
--  # opening it again. Otherwise there can be an issue
--  # when trying to reopen it while it's actually closing.

repeat while running of application "System Preferences" is true
    delay 0.1
end repeat

--  # Open System Preferences to the General
--  # tab of the Security and Privacy pane.

tell application "System Preferences"
    activate
    reveal anchor "General" of ¬
        pane id "com.apple.preference.security"
end tell

tell application "System Events"
    tell application process "System Preferences"
        tell its window 1
            --  # Wait until the 'Require password' check box exists.
            set i to 0
            repeat until exists checkbox 1 of tab group 1
                delay 0.1
                set i to i + 1
                if i ≥ 30 then return
            end repeat
            --  # If the target check box is unchecked then quit.
            if value of checkbox 1 of tab group 1 = 0 then
                tell application "System Preferences" to quit
                return
            end if
            --  # Else click the target check box.
            click checkbox 1 of tab group 1
            --  # Wait until the 'Enter the password' sheet exists.
            set i to 0
            repeat until exists sheet 1
                delay 0.1
                set i to i + 1
                if i ≥ 30 then return
            end repeat
            --  # You can uncomment the 'set value of text field 1' and set the password
            --  # and uncomment 'click button "OK"' to have the rest fully automated, or
            --  # leave them commented, enter your password, press enter and the rest is
            --  # automated. There is also a way to retrieve your password from your
            --  # Keychain, however, that's for you to research and implement.
            tell sheet 1
                -- set value of text field 1 to "PASSWORD"
                -- click button "OK"
                set i to 0
                repeat until exists button "Turn Off Screen Lock"
                    delay 0.5
                    set i to i + 1
                    if i ≥ 30 then return
                end repeat
                click button "Turn Off Screen Lock"
            end tell
        end tell
    end tell
end tell

tell application "System Preferences" to quit
