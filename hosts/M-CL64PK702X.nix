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
  services.garage-queue-worker.workers.ollama.settings.capabilities.scalars.vram_mb =
    24576;
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
              "claude-code"
              "example-unfree-package"
              "terraform"
              "unrar"
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
    ../darwin-configs/screen-sharing.nix
    ../darwin-configs/proc-siding-worker.nix
    ../darwin-configs/goss-ollama-metal-gpu.nix
    ../darwin-configs/sonify-health-goss.nix
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
    (
      {
        facts,
        pkgs,
        ...
      }:
      {
        imports = [
          ../darwin-modules/global-protect-persistent.nix
        ];
        services.globalprotect-monitor = {
          enable = true;
          server = "vpn-${org-alias}.gpcloudservice.com";
          username = "${username}@${org-domain}";
          orgName = org-alias;
          primaryUser = username;
          checkInterval = 60;
          dnsmasq.domainForwarding = [
            {
              domain = facts.network.domain;
              server = pkgs.lib.custom.networkDnsIp facts;
            }
          ];
        };
      }
    )
  ];
  networking.monitors = [ "goss" ];
  services.sonify-health = {
    enable = true;
    # Socket stuff doesn't seem to work with macOS the way we have it.  I need
    # to ensure we created the socket first, which is something you have to do
    # on macOS that systemd just handles a little more automatically.  There's
    # also the question of how to access the web UI.
    socket = null;
    port = 3000;
    logLevel = "debug";
    patches = {
      star-trek-ok = {
        amplitude = 0.327;
        attack_ms = 6.0;
        brightness = 1.82;
        chirp_ratio = 1.01;
        crush = 0.0;
        decay_ms = 0.0;
        downsample = 0.0;
        drive = 0.5;
        duration = 0.22;
        echo_delay = 0.32;
        echo_mix = 0.33;
        fm_depth = 0.0;
        fm_ratio = 0.0;
        freq = 4307.0;
        gap = 0.0;
        highpass = 0.0;
        noise_mix = 0.0;
        release_ms = 22.0;
        resonance = 3.72;
        reverb_mix = 0.89;
        saw_ratio = 0.02;
        sine_ratio = 2.37;
        square_ratio = 0.0;
        stereo_pan = -0.42;
        sub_octave = 0.03;
        sustain = 1.0;
        tremolo_depth = 0.11;
        tremolo_rate = 0.0;
        tri_ratio = 1.22;
        vibrato_depth = 0.49;
        vibrato_rate = 0.0;
      };
      star-trek-error = {
        amplitude = 0.54;
        chirp_ratio = 0.8;
        overrides = "star-trek-ok";
      };
      warpdrive-cpu-lo = {
        amplitude = 1.0;
        attack_ms = 5.0;
        brightness = 0.2;
        chirp_ratio = 1.0;
        crush = 0.0;
        decay_ms = 400.0;
        downsample = 0.0;
        drive = 0.01;
        duration = 1.68;
        echo_delay = 0.08;
        echo_mix = 0.15;
        fm_depth = 0.0;
        fm_ratio = 0.0;
        freq = 60.0;
        gap = 0.0;
        highpass = 0.0;
        noise_mix = 1.0;
        release_ms = 200.0;
        resonance = 5.0;
        reverb_mix = 0.25;
        saw_ratio = 0.0;
        sine_ratio = 0.0;
        square_ratio = 0.0;
        stereo_pan = 0.0;
        sub_octave = 0.0;
        sustain = 0.39;
        tremolo_depth = 0.0;
        tremolo_rate = 0.0;
        tri_ratio = 0.0;
        vibrato_depth = 0.0;
        vibrato_rate = 0.0;
      };
      warpdrive-cpu-hi = {
        gap = -1.64;
        overrides = "warpdrive-cpu-lo";
      };
    };
    heartbeats = [
      {
        name = "internal";
        command = "${pkgs.fping}/bin/fping -q -t 4000 -r 1 192.168.254.254 192.168.254.9 silicon.proton";
        resultMode = "exit-code";
        cycleSecs = 15.0;
        playback = "clock";
        cycleOffsetSecs = 11.0;
        notes = [
          {
            transition = {
              type = "discrete";
              states = [
                {
                  threshold = 0.5;
                  patch = "star-trek-ok";
                }
                {
                  threshold = 1.01;
                  patch = "star-trek-error";
                }
              ];
            };
          }
          {
            offset = 0.2;
            transition = {
              type = "discrete";
              states = [
                {
                  threshold = 0.5;
                  patch = "star-trek-ok";
                }
                {
                  threshold = 1.01;
                  patch = "star-trek-error";
                }
              ];
            };
          }
        ];
      }
      {
        name = "external";
        command = "${pkgs.fping}/bin/fping -q -t 4000 -r 1 208.67.222.222 9.9.9.9 resolver1.opendns.com api.anthropic.com";
        resultMode = "exit-code";
        cycleSecs = 15.0;
        playback = "clock";
        cycleOffsetSecs = 8.0;
        notes = [
          {
            transition = {
              type = "discrete";
              states = [
                {
                  threshold = 0.5;
                  patch = "star-trek-ok";
                }
                {
                  threshold = 1.01;
                  patch = "star-trek-error";
                }
              ];
            };
          }
          {
            offset = 0.2;
            transition = {
              type = "discrete";
              states = [
                {
                  threshold = 0.5;
                  patch = "star-trek-ok";
                }
                {
                  threshold = 1.1;
                  patch = "star-trek-error";
                }
              ];
            };
          }
        ];
      }
      {
        name = "vpn";
        command = "${pkgs.fping}/bin/fping -q -t 4000 -r 1 10.210.16.247 10.210.16.191 idm01.mgmt.${work-alias}colo.pvt artifacts.americas.${work-alias}.pvt";
        resultMode = "exit-code";
        cycleSecs = 15.0;
        playback = "clock";
        cycleOffsetSecs = 5.0;
        notes = [
          {
            transition = {
              type = "discrete";
              states = [
                {
                  threshold = 0.5;
                  patch = "star-trek-ok";
                }
                {
                  threshold = 1.01;
                  patch = "star-trek-error";
                }
              ];
            };
          }
        ];
      }
      {
        name = "gpu";
        command = "${
          flake-inputs.metalps.packages.${system}.cli
        }/bin/metalps --json --interval-ms 500 | ${pkgs.jq}/bin/jq '[.processes[].gpu_percent] | add // 0 | . / 100'";
        resultMode = "stdout";
        cycleSecs = 15.0;
        playback = "loop";
        cycleOffsetSecs = 0.0;
        crossfadeMs = 42.0;
        pollIntervalSecs = 5.0;
        notes = [
          {
            volume = 1.0;
            transition = {
              type = "gradient";
              patches = [
                "warpdrive-cpu-lo"
                "warpdrive-cpu-hi"
              ];
              segments = [
                {
                  strategy = "linear";
                  intensity = 5.3;
                }
              ];
            };
          }
        ];
      }
    ];
  };
}
