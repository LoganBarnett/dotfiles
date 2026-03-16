################################################################################
# Displays a URL on a host's head, but doesn't allow interaction.
################################################################################
{
  url,
  browser ? "chromium",
}:
{ config, pkgs, ... }: let
  kioskUser = "kiosk";
  group = "kiosk";
  homepage = "https://example.com"; # Replace with your desired URL
in {
  age.secrets.kiosk-long-passphrase = {
    generator.script = "long-passphrase";
    inherit group;
    mode = "0440";
    rekeyFile = ../secrets/${kioskUser}-password.age;
  };
  age.secrets.kiosk-long-passphrase-hashed = {
    generator = {
      script = "long-passphrase-hashed";
      dependencies = [
        config.age.secrets."${kioskUser}-long-passphrase"
      ];
    };
    inherit group;
    rekeyFile = ../secrets/${kioskUser}-hashed-password.age;
    mode = "0440";
  };
  users.users.${kioskUser} = {
    isNormalUser = true;
    hashedPasswordFile = config.age.secrets.kiosk-long-passphrase-hashed.path;
    # Needed for display access.
    extraGroups = [ "video" "input" ];
  };
  users.groups.${group} = {};
  services.displayManager.enable = true;
  services.displayManager.autoLogin = {
    enable = true;
    user = kioskUser;
  };
  services.xserver = {
    enable = true;
    # Optional: Disable screen blanking.
    serverFlagsSection = ''
      Option "BlankTime" "0"
      Option "StandbyTime" "0"
      Option "SuspendTime" "0"
      Option "OffTime" "0"
    '';
    displayManager = {
      lightdm.enable = true;
      # Optional: Turn off the screen saver and DPMS.
      sessionCommands = ''
        xset s off
        xset -dpms
        xset s noblank
      '';
      session = [
        {
          name = "kiosk";
          manage = "windowManager";
          start = ''
            ${pkgs.openbox}/bin/openbox-session &
            systemctl --user start kiosk.service
            waitPID=$!
          '';
        }
      ];
    };
    windowManager.openbox.enable = true;
  };
  # services.xserver.desktopManager.default = "none"; # prevents conflict
  environment.systemPackages = [
    pkgs.xdotool  # optional: for interaction blocking tweaks
  ];
  systemd.user.services.kiosk = {
    enable = true;
    description = "Kiosk";
    wantedBy = [ "graphical-session.target" ];
    serviceConfig = {
      ExecStart = let
        chromium = pkgs.writeShellApplication {
          name = "kiosk-browser";
          runtimeInputs = [ pkgs.chromium ];
          text = ''
            /bin/chromium \
              --kiosk \
              --noerrdialogs \
              --disable-infobars \
              --disable-session-crashed-bubble \
              --incognito \
              --disable-pinch \
              --overscroll-history-navigation=0 \
              '${url}'
          '';
        };
        firefox = pkgs.writeShellApplication {
          name = "kiosk-browser";
          runtimeInputs = [ pkgs.firefox ];
          text = ''
            firefox --kiosk '${url}'
          '';
        };
        script = if browser == "chromium" then chromium else firefox;
      in ''
        ${script}/bin/kiosk-browser'';
      Restart = "always";
      Environment = "XDG_RUNTIME_DIR=%t";
    };
  };
}
