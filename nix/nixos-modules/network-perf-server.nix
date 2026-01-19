################################################################################
# Enable network performance testing via iperf on this host.
################################################################################
{ ... }: {
  services.iperf = {
    enable = true;
    openFirewall = true;
    # TODO: Lock down this server with some credentials.
    # rsaPrivateKey = "";
    # authorizedUsersFile = "";
  };
  # TODO: Provide a Prometheus exporter for this functionality.  You can find
  # the exporter here: https://github.com/edgard/iperf3_exporter
  # At time of writing, there is no Prometheus exporter NixOS configuration, but
  # it would be pretty easy to put together I imagine.
}
