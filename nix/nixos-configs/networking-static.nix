################################################################################
# Switch to systemd-network or whatever it is NixOS wants to use.
#
# This is in part because WiFi via `networking.wireless` is mutually exclusive
# to NetworkManager.  NetworkManager is better for most server hosts, but
# probably not great for laptops which need to roam freely.
################################################################################
{ ... }: {
  # Hah!  If you disable networkmanager, you'll basically lose your network
  # stack.  Turn DHCP on to allow routes, IPs, etc to get stood up
  # automatically.
  networking.useDHCP = true;
  networking.networkmanager.enable = false;
}
