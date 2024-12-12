################################################################################
# This is some boilerplate for Raspberry Pi hosts.
#
# We can make some assumptions about building Raspberry Pis since we can create
# the entire bootable image from scratch, and we don't need to deal with UEFI
# nonsense.
################################################################################
{ flake-inputs, host-id }: { ... }: {
  imports = [
   # (import ./secrets.nix {inherit flake-inputs host-id;})
  ];
  age.secrets."${host-id}-pub-key" = {
    generator.script = "ssh-ed25519-with-pub";
    rekeyFile = ../secrets/${host-id}-pub-key.age;
  };
}
