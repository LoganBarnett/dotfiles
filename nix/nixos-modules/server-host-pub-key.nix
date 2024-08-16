################################################################################
# Bootstrap a host with an SSH key.
#
# Presently doesn't work, and likely isn't possible with existing libraries.
################################################################################
# Theft from:
# https://github.com/oddlama/nix-config/blob/6483bd4f7edb981fbcee829fa1e455fe0b848c36/config/optional/initrd-ssh.nix#L15
{ config, lib, pkgs, ... }: let
  host-id = config.networking.hostName;
in {
  age.secrets.initrd_host_ed25519_key = {
    generator.script = "ssh-ed25519-with-pub";
    # It's important to specify a rekeyFile because if it goes into our
    # generated directory, agenix-rekey will remove "orphaned" files from there.
    # This can be seen with a message like this:
    # Removed 2 orphaned files in generation directories
    # Those files are the generated public keys that we wish to remain intact.
    rekeyFile = ../secrets/${host-id}/initrd_host_ed25519_key.age;
  };
  boot.initrd.network.enable = true;
  boot.initrd.network.ssh = {
    enable = false;
    authorizedKeyFiles = lib.mkForce ["/etc/ssh/authorized_keys.d/%u"];
    port = 4;
    hostKeys = [config.age.secrets.initrd_host_ed25519_key.path];
  };
  boot.initrd.secrets = {
    "/etc/host-key" = config.age.secrets.initrd_host_ed25519_key.path;
  };
  # {
  #   age.identityPaths = [
  #     "/etc/host-key"
  #   ];
  # }
  # Make sure that there is always a valid initrd hostkey available that can
  # be installed into the initrd. When bootstrapping a system (or
  # re-installing), agenix cannot succeed in decrypting whatever is given,
  # since the correct hostkey doesn't even exist yet. We still require a
  # valid hostkey to be available so that the initrd can be generated
  # successfully.  The correct initrd host-key will be installed with the
  # next update after the host is booted for the first time, and the secrets
  # were rekeyed for the the new host identity.
  system.activationScripts.agenixEnsureInitrdHostkey = {
    text = ''
      [[ -e ${config.age.secrets.initrd_host_ed25519_key.path} ]] \
        || ${pkgs.openssh}/bin/ssh-keygen -t ed25519 -N "" -f ${config.age.secrets.initrd_host_ed25519_key.path}
  #   '';
    deps = ["agenixInstall" "users"];
  };
  system.activationScripts = {
    agenixChown.deps = ["agenixEnsureInitrdHostkey"];
    # Just before switching, remove the agenix directory if it exists.  This can
    # happen when a secret is used in the initrd because it will then be copied
    # to the initramfs under the same path. This materializes /run/agenix as a
    # directory which will cause issues when the actual system tries to create a
    # link called /run/agenix. Agenix should probably fail in this case, but
    # doesn't and instead puts the generation link into the existing directory.
    # TODO See https://github.com/ryantm/agenix/pull/187.
    # removeAgenixLink.text = "[[ ! -L /run/agenix ]] && [[ -d /run/agenix ]] && rm -rf /run/agenix";
    # agenixNewGeneration.deps = ["removeAgenixLink"];
  };
}
