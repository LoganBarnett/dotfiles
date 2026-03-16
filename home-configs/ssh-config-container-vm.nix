{ ... }: {
  programs.ssh = {

    matchBlocks = {
      # Fix issues with podman-machine VM not starting up.  It starts, but not
      # before the connection mechanism gives up.  See:
      # https://github.com/containers/podman/issues/14237
      # https://github.com/containers/podman/issues/17403#issuecomment-1511363735
      # However this only seems to allow the process to hang indefinitely.
      "localhost" = {
        identitiesOnly = true;
      };
    };

  };
}
