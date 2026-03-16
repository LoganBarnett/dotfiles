################################################################################
# This module prevents powering down the machine by way of the power switch.
#
# The kids have a nasty habit of powering their machines down when they are done
# using them.  That doesn't mean _I_ am done using them though.  These are
# servers too, and I need them to remain online.
################################################################################
{
  services.logind = {
    # enable = true;
    # You can also have "poweroff", "reboot", "halt", "suspend", "hibernate",
    # and "lock".
    extraConfig = ''
      HandlePowerKey=ignore
      HandleSuspendKey=ignore
      HandleLidSwitch=ignore
    '';
  };
  security.polkit = {
    enable = true;
    extraConfig = ''
      polkit.addRule(function(action, subject) {
        if (
          (action.id == "org.freedesktop.login1.power-off" ||
           action.id == "org.freedesktop.login1.reboot" ||
           action.id == "org.freedesktop.login1.hibernate" ||
           action.id == "org.freedesktop.login1.suspend") &&
          !subject.isInGroup("wheel")
        ) {
          return polkit.Result.NO;
        }
      });
    '';
  };
}
