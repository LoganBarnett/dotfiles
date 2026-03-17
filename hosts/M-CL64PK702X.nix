################################################################################
# SECURITY NOTE: The strings assembled in `org-alias` and `work-alias` below
# should NEVER appear unobfuscated in scripts, Nix code, or documentation
# files on disk.  They must always be interrupted (character-by-character
# concatenation) to thwart searches that might be used by those looking for
# attack vectors.  Use these variables or environment variables (ORG_NAME,
# SUBORG_NAME, GP_SERVER) instead of hardcoding the literal strings.
################################################################################
{
  flake-inputs,
  host-id,
  lib,
  system,
  pkgs,
  ...
}:
let
  username = "logan.barnett";
  # Parent organization.
  org-alias = lib.concatStrings [
    # I'm the Riddler.
    "h"
    "m"
    "h"
  ];
  # Sub-organization.
  work-alias = lib.concatStrings [
    # I'm the Riddler.
    "n"
    "w"
    "e"
    "a"
  ];
  work-domain = "${work-alias}.org";
  # Email domain (different from org name).
  org-domain = "${org-alias}co.com";
in
{
  services.garage-queue-worker.settings.capabilities.scalars.vram_mb = 24576;
  system.primaryUser = username;
  # Something required for every macOS host after a nix-darwin migration.  This
  # value will be different per host.  Perhaps hosts stood up after that point
  # won't need it.
  ids.gids.nixbld = 30000;
  imports = [
    (
      { lib, pkgs, ... }:
      {
        allowUnfreePackagePredicates = [
          (
            pkg:
            builtins.elem (lib.getName pkg) [
              "example-unfree-package"
              "unrar"
              "terraform"
              "windsurf"
            ]
          )
        ];
      }
    )
    ../nixos-configs/secrets.nix
    flake-inputs.garage-queue.darwinModules.worker
    flake-inputs.home-manager.darwinModules.home-manager
    # the _module.args idiom is how I can ensure these values get passed via the
    # internal callPackage mechanism for darwinSystem on these modules.  We want
    # callPackage because it does automatic "splicing" of nixpkgs to achieve
    # cross-system compiling.  I don't know that we need to use this at this
    # point, but making it all consistent has value.
    {
      _module.args.git-users = [
        {
          git-email = "logan.barnett@${work-domain}";
          git-name = "Logan Barnett";
          git-signing-key = "85D2D1CE81A7A529FA4ABAE61841B0A4F704B99A";
          host-username = "logan.barnett";
        }
      ];
    }
    ../darwin.nix
    ../darwin-configs/garage-queue-worker.nix
    ../darwin-configs/proc-siding-worker.nix
    ../darwin-configs/goss-ollama-metal-gpu.nix
    ../darwin-configs/ollama.nix
    # M1 Max with 32 GB unified memory: 32 × 0.75 ≈ 24 GB available for
    # model weights when the machine is lightly loaded.
    ../nixos-configs/ollama-models-24gb-vram.nix
    ../users/logan-new-e-ah.nix
    ../nixos-configs/user-can-admin.nix
    ../nixos-configs/workstation.nix
    ../nixos-configs/user-can-develop.nix
    ../headed-host.nix
    (
      {
        config,
        lib,
        pkgs,
        ...
      }:
      {
        age.secrets.llm-coding-agent-ssh = {
          generator.script = "ssh-ed25519-with-pub";
          rekeyFile = ../secrets/llm-coding-agent-ssh.age;
          # Allow the primary user to read this key for SSH client usage.
          mode = "0400";
          owner = config.system.primaryUser;
        };
        home-manager.users."logan.barnett" = {
          imports = [
            ../home-configs/ghostty.nix
            ../home-configs/gh-cli.nix
            ../home-configs/gh-cli-ache-em-ache.nix
            ../home-configs/mcphost.nix
            ../home-configs/copilot.nix
            ../home-configs/copilot-ache-em-ache.nix
            ../home-configs/ssh-config-general.nix
            ../home-configs/ssh-config-container-vm.nix
            flake-inputs.emacs-config.homeModules.ssh-config-emacs
            ../home-configs/ssh-config-new-e-ah.nix
            ../home-configs/ssh-config-proton.nix
            ../home-configs/ssh-llm-coding-agent-new-e-ah.nix
          ];
          home.file.".gemrc".text = (
            pkgs.callPackage ../gemrc.nix {
              extra-gem-sources = [
                (lib.concatStrings [
                  "http://gems.mgmt.${work-alias}colo.pvt:8080/"
                ])
              ];
            }
          );
          # I used to manage .yarnrc the same way, but it has a lastUpdateCheck
          # field that is automatically written to, so it would need some
          # different plumbing.  I have examples of just writing files out that
          # can be written by external processes later, but there are better ways
          # to tackle this.
          # https://github.com/yarnpkg/yarn/issues/4134 outlines this exact issue,
          # and it is now closed because yarn v2 separates the auto-generated
          # stuff.  I don't have any projects with which to test this on yet, so
          # no action is to be taken.  I've just removed the yarnrc file instead.
          # This is the last vestiges of my yarnrc, for reference:
          # "--add.exact" true
          # email logustus@gmail.com
          # lastUpdateCheck
          # username logustus
          home.file.".npmrc".text = (
            pkgs.callPackage ../npmrc.nix {
              extra-npm-registries = [
                (lib.concatStrings [
                  "@${work-alias}:registry="
                  "https://artifacts.americas.${work-alias}.pvt/nexus/content/groups/npm-all/"
                ])
              ];
            }
          );
        };
        environment.etc."krb5.conf".text = ''
          [libdefaults]
            default_realm = IPA.${lib.toUpper work-alias}COLO.PVT

          [realms]
            IPA.${lib.toUpper work-alias}COLO.PVT = {
              kdc = idm01.mgmt.${work-alias}colo.pvt
            }
            AMERICAS.${lib.toUpper work-alias}.PVT = {
            }
        '';
        environment.systemPackages = [
          pkgs.aider-chat
          pkgs.awscli
          # Interact with our internal Bitbucket Data Center server.
          pkgs.bitbucket-cli
          pkgs.confluence-markdown-exporter
          # Command line utility to query, search and tail EL (elasticsearch,
          # logstash) logs.
          pkgs.elktail
          # Use GitHub from the command line.
          pkgs.gh
          # Stop using the cursed GlobalProtect VPN GUI client and use something
          # we can better automate instead.
          pkgs.gpclient
          # Separate authentication tool for GlobalProtect SSO.
          pkgs.gpauth
          # Wrapper script for easy GlobalProtect connection.
          (pkgs.callPackage ../derivations/gp-connect.nix { })
          # Automatic headless authentication for GlobalProtect.
          (pkgs.callPackage ../derivations/gp-connect-auto.nix { })
          # Used for encrypting sensitive information in Hiera.
          pkgs.hiera-eyaml
          pkgs.mktemp
          # Gives us tools like ldapsearch, ldapadd, and ldapmodify which is
          # sometimes used for searching users and figuring out who the managers
          # are of employees.
          pkgs.openldap
          pkgs.openssl
          # Needed for our flavor of Hiera EYAML usage.  See `hiera-eyaml` for
          # more info.
          pkgs.saml2aws
          # Try out terraform changes, including checking on HCP workspaces, which
          # can really speed up coding agent usage.
          pkgs.terraform
          # Let us log in on the blasted VPN UI.  Doesn't work on macOS currently.
          # pkgs.sunshine
          # Let machines write the machine instructions.
          pkgs.windsurf
          # `jq` but for YAML.  The `yq` (no suffix) is a Python app which
          # converts YAML into JSON and then back again if desired.  So it
          # requires `jq` for operations.  This `yq` is standalone and can work
          # with YAML idioms, but isn't as mature as `jq`.
          pkgs.yq-go
        ];
        # Environment variables for scripts that need to avoid hardcoding
        # organization names.
        environment.variables = {
          ORG_NAME = org-alias;
          ORG_DOMAIN = org-domain;
          SUBORG_NAME = work-alias;
          # GlobalProtect VPN server.
          GP_SERVER = "vpn-${org-alias}.gpcloudservice.com";
          # GlobalProtect username (email).
          GP_USERNAME = "${username}@${org-domain}";
        };
        networking.hostName = host-id;
        nixpkgs.hostPlatform = system;
        security.pki.certificateFiles = [
          "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
          ../new-e-ah-certs.pem
          # Yabba dabba do!
          ../ach-em-ach-flintstones-ca.pem
          # Trust proton CA for local services.
          ../secrets/proton-ca.crt
        ];
        security.pki.keychain.certificateFiles = [
          ../new-e-ah-certs.pem
          # Yabba dabba do!
          ../ach-em-ach-flintstones-ca.pem
          # Trust proton CA for local services.
          ../secrets/proton-ca.crt
        ];
        # system.activationScripts.postActivation.text = ''
        # '';
        system.stateVersion = 5;
        system.activationScripts.postActivation.text = ''
          # Grant SSH access.
          dseditgroup -o edit -a logan.barnett -t user com.apple.access_ssh
          # Set the default shell.  While some systems can work fine without this
          # (I was using Terminal.app just fine while it was broken), things like
          # sshd will silently fail.
          # TODO: Make this work for multiple users.
          # TODO: Contribute this back when multiple activation scripts are
          # allowed.  This either doesn't work, or it isn't enough.  See also the
          # next command.
          echo 'Updating user shells...'
          dscl . -create /Users/logan.barnett UserShell /run/current-system/sw/bin/zsh
          # You'd think we'd just use this, but it's forcibly interactive even
          # when run as root.  Or I'm using it wrong.
          # chsh -u logan.barnett -s /run/current-system/sw/bin/zsh
          echo 'User shells updated.'
        '';
      }
    )
    {
      imports = [
        ../darwin-modules/ollama.nix
      ];
      # services.open-webui.enable = true;
    }
    {
      imports = [
        ../darwin-modules/global-protect-persistent.nix
      ];
      services.globalprotect-monitor = {
        # Disabled while debugging routing issues — run gp-connect-auto manually.
        enable = false;
        server = "vpn-${org-alias}.gpcloudservice.com";
        username = "${username}@${org-domain}";
        orgName = org-alias;
        primaryUser = username;
        checkInterval = 60;
      };
    }
  ];
}
