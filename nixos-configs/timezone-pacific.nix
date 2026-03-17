################################################################################
# Use for hosts residing in the Pacific Standard time zone.
#
# Ideally hosts should be using UTC in many circumstances, but the distinction
# between servers and desktops is somewhat blurred in my infrastructure.  This
# will make the machines show the correct time on their displays.
################################################################################
{
  time.timeZone = "America/Los_Angeles";
}
