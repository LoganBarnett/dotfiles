################################################################################
# Synchronizes my notes (a plain text git repository) to a WebDAV directory.
# This allows mobile clients that support WebDAV to consume the notes, and
# eliminates the need for a third party hosting provider that can see my notes.
#
# Changes coming from these scripts aren't picked up automatically.  There is a
# periodic process that scans files, but I haven't seen it work (circa
# [2025-02-25].  One can force it with:
# sudo -u nextcloud nextcloud-occ files:scan --all
################################################################################
{ config, pkgs, ... }: {
  age.secrets.notes-sync-ssh = {
    rekeyFile = ../secrets/notes-sync-ssh.age;
    generator.script = "ssh-ed25519-with-pub";
    group = "nextcloud";
    mode = "0440";
  };
  systemd.timers.notes-sync = {
    # multi-user.target is how we ensure this timer is started on a
    # `nixos-rebuild switch`.
    wantedBy = [ "multi-user.target" "timers.target" ];
    timerConfig = {
      # This is needed to also start the timer at boot.
      OnBootSec = "15m";
      OnUnitActiveSec = "15m";
    };
  };
  systemd.services.notes-sync = {
    enable = true;
    serviceConfig = {
      ExecStart = let
        # Unfortunately the documentation generators out there are incomplete
        # for this function.  See
        # https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/trivial-builders/default.nix#L175
        # for all of the parameters availble.
        script = pkgs.writeShellApplication {
          name = "notes-sync";
          runtimeInputs = [
            pkgs.git
            pkgs.openssh
          ];
          # excludeShellChecks = [
          #   "SC2154"
          # ];
          text = builtins.readFile ../scripts/notes-sync.sh;
        };
        user-dir = "logan";
        git-url = "git@bitbucket.org:LoganBarnett/notes.git";
        notes-dir =
          "${config.services.nextcloud.home}/data/${user-dir}/files/notes";
      in
        # TODO: Move hostname and domain into facts.nix.
        ''
        ${script}/bin/notes-sync \
          --git-url ${git-url} \
          --ssh-identity ${config.age.secrets.notes-sync-ssh.path} \
          --sync-dir ${notes-dir}
        '';
      # Ensure these get captured.  We were observing stderr not getting logged.
      StandardOutput = "journal";
      StandardError = "journal+console";
      Type = "oneshot";
      # TODO: Make this less sloppy.
      User = "nextcloud";
    };
    wants = [ "run-agenix.d.mount" ];
  };
}
