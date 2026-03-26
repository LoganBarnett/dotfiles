################################################################################
# Manage secret generators and declare all secrets here.
#
# This makes heavy use of agenix-rekey to declare secrets that are encrypted per
# host by both a master key and host keys.  These secrets do not wind up in the
# Nix store though they are manage declaratively.
#
# There are some challenges when a secret is managed or needed at _build_ time
# (as opposed to, for example, a service that reads in a secret when it starts).
# I don't have specific steps for getting around that yet.
#
# To decrypt a secret manually, use `rage --decrypt <file>.age`.  You will be
# prompted for the master password's key (which is the 3rd one I've create, "-3"
# as a suffix).  I don't know if the .pub file matters or not.
################################################################################
{
  config,
  flake-inputs,
  host-id,
  lib,
  pkgs,
  system,
  ...
}:
let
in
{
  nixpkgs.overlays = [
    flake-inputs.agenix.overlays.default
    # This lets us include the agenix-rekey package.
    flake-inputs.agenix-rekey.overlays.default
  ];
  # Grants us the "long-passphrase" generator.
  # TODO: Help document pre-existing generators listed here:
  # https://github.com/oddlama/agenix-rekey/blob/85df729446fca1b9f22097b03e0ae2427c3246e2/modules/agenix-rekey.nix#L557
  age.generators.long-passphrase =
    { pkgs, ... }: "${pkgs.xkcdpass}/bin/xkcdpass --numwords=10 --delimiter=' '";
  age.generators.long-passphrase-hashed =
    {
      decrypt,
      deps,
      file,
      name,
      pkgs,
      secret,
      ...
    }:
    ''
      ${decrypt} ${(lib.escapeShellArg (builtins.elemAt deps 0).file)} | \
         ${pkgs.openssl}/bin/openssl passwd -6 -stdin
    '';

  age.rekey = {
    # TODO:  This is the host key, and we should call it that instead of the pub
    # key.  The .pub is the pub key, but we also have a private key and having
    # that called the pub-key doesn't make sense.  Make sure to capture other
    # references, and rename what's already on disk.
    hostPubkey = ../secrets/${host-id}-pub-key.pub;
    masterIdentities = [
      ../secrets/agenix-master-key-3.age
    ];
    # Must be relative to the flake.nix file.
    localStorageDir = ../secrets/rekeyed/${host-id};
    generatedSecretsDir = ../secrets/generated;
    secretsDir = ../secrets;
    # These fields are labeled as missing with:
    #  The option `age.rekey.userFlake' does not exist. Definition values:
    # userFlake = flake-inputs.self;
    # nodes = flake-inputs.self.nixosConfigurations;
    storageMode = "local";
  };

  age.generators.ssh-ed25519-with-pub =
    {
      file,
      lib,
      name,
      pkgs,
      ...
    }:
    ''
      mkdir -p "$(dirname "${file}")"
      (exec 3>&1;
      ${pkgs.openssh}/bin/ssh-keygen \
        -q \
        -t ed25519 \
        -N "" \
        -C ${lib.escapeShellArg "${name}"} \
        -f ${name} \
        <<<y >/dev/null 2>&1;
        cp "${name}.pub" "$(dirname "${file}")"
        echo copied public key ${name}.pub to "$(dirname "${file}")" 1>&2
        cat "${name}"
        rm "${name}"{,.pub}
      true)
    '';

  # TODO: This doesn't quite work as expected.  We need something to sync up
  # with the /etc/ssh/ssh_host_ed25519_key and its .pub sibling.  This will
  # break for new hosts trying to get secrets.  To fix it, overwrite the .pub
  # file in this repository for the host in question with
  # /etc/ssh/ssh_host_ed25519_key.pub and run `agenix rekey -a` to rekey the
  # files.
  # age.secrets."${host-id}-pub-key" = {
  #   generator.script = "ssh-ed25519-with-pub";
  #   rekeyFile = ../secrets/${host-id}-pub-key.age;
  # };

  # This is a catch22.  This is needed at build time but isn't available until
  # activation time.  See bin/nix-host-new in this repository as well as
  # bin/nix-host-key-install for putting this file in the right place.
  # environment.etc."ssh/ssh_host_ed25519_key".file = config.age.secrets."${host-id}-pub-key".file;

  age.secrets.proton-ca = {
    intermediary = true;
    rekeyFile = ../secrets/proton-ca.age;
    settings = {
      tls = {
        domain = "proton";
        subject = {
          country = "US";
          state = "Oregon";
          location = "Portland";
          organization = "Barnett family";
          organizational-unit = "IT Department";
        };
        validity = 365 * 5;
      };
    };
    generator.script = "tls-ca-root";
  };

  age.secrets.builder-key-blue = {
    generator.script = "ssh-ed25519-with-pub";
    rekeyFile = ../secrets/builder-key-blue.age;
    # Don't set a custom path - let agenix manage it in /run/agenix.  Setting a
    # custom path in /etc/nix conflicts with nix-darwin's environment.etc
    # management, which causes nix-darwin to rename the file with a
    # .before-nix-darwin suffix.
    # path = "/etc/nix/builder-agenix-key";
  };

  age.secrets.builder-key-green = {
    generator.script = "ssh-ed25519-with-pub";
  };

  # Ensure agenix secrets are decrypted before NixOS sets up user accounts,
  # so that hashedPasswordFile references are available during activation.
  system.activationScripts.users.deps = [ "agenixInstall" ];

  # If you're here because you can't find /run/agenix, you're probably on
  # darwin/macOS and you don't have any host keys in /etc/ssh.  You will have to
  # generate them for this host (which can be done with `agenix-rekey
  # generate`), and then have them copied to the correct location by running
  # `nix-host-key-install` on the host in question, with this repository in
  # place (which you have to have cloned to make the executable available
  # anyways).  Then you should find a /run/agenix.  Additional notes can be
  # found in the nix-host-key-install script.
  #
  # This value is left to help me find it again when I forget all of this again.
  # I have a checklist executable started in nix-host-new.
  age.secretsDir = "/run/agenix";

  imports = [
    # This should work equally for macOS.  This and the darwinModules version
    # both reference the same file.
    flake-inputs.agenix.nixosModules.default
    # agenix-rekey now sets _class on its modules, so we must select the
    # correct one for the platform to avoid a class mismatch error.  We use
    # `system` rather than `pkgs.stdenv.isDarwin` because `pkgs` is not fully
    # resolved during `imports` evaluation — using it here risks infinite
    # recursion since `pkgs` is shaped by the modules being imported.
    (
      if lib.strings.hasSuffix "-darwin" system then
        flake-inputs.agenix-rekey.darwinModules.default
      else
        flake-inputs.agenix-rekey.nixosModules.default
    )
    # The installSecretFn fork (flake input agenix) makes this unnecessary.
    # Re-enable if the fork is dropped and MAX_ARG_STRLEN problems return.
    # ../nixos-modules/agenix-compact-activation.nix
    ../agenix/agenix-rekey-generator-mosquitto-password-file.nix
    ../agenix/slapd-hashed.nix
    ../agenix/stalwart-dkim-key.nix
    ../agenix/base64-configurable-secret.nix
    ../agenix/environment-file-secret.nix
    ../agenix/htpasswd.nix
    ../agenix/hex-configurable-secret.nix
    ../agenix/openhab-pbkdf2.nix
    ../agenix/secret-template-file.nix
    ../agenix/tls-secret.nix
    ../agenix/wireguard-priv.nix
    ../agenix/yaml-secret.nix
  ];
  environment.systemPackages = [
    # This should remain out because agenix-rekey brings in a replacement
    # agenix.
    # flake-inputs.agenix.packages.${system}.default
    pkgs.agenix-rekey
    # Rage is a Rust based Age that claims a more consistent CLI API.
    pkgs.rage
  ];
  # These files are not actually temporary files, especially if the "-" at the
  # end is included to keep systemd from cleaning up the directory (at some
  # point?).  See
  # https://discourse.nixos.org/t/is-it-possible-to-declare-a-directory-creation-in-the-nixos-configuration/27846/6
  # for Nix usage, and see
  # https://www.freedesktop.org/software/systemd/man/latest/tmpfiles.d.html for
  # the systemd documentation on the topic.
  # Unfortunately, there might be a loading order problem with this and other
  # build-time scripts.  Can I use a variable for the directory so it is
  # resolved before any secrets are used?  Or perhaps just some settings in
  # age.secretsDir to ensure the directory exists.
  # Since this doesn't use a declarative approach, I am opting not to use it.
  # systemd.tmpfiles.rules = [
  #   "d /etc/secrets 0770 nixbld nixbld -"
  # ];
}
