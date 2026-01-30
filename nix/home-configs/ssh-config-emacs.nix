{ ... }: {
  programs.ssh = {

    matchBlocks = {
      # Global defaults for all hosts.
      "*" = {
        # For Emacs.
        controlMaster = "auto";
        # Send keepalive packets to prevent the ssh host or
        # firewall/loadbalancer itself from dropping connections.
        serverAliveInterval = 50;
      };

    };

  };
}
