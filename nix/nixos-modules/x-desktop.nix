################################################################################
# Make this host an X server based desktop.
################################################################################
{ flake-inputs, lib, pkgs, ... }: {
  # Hint to Electron apps to use Wayland.
  environment.sessionVariables.NIXOS_OZONE_WL = "1";
  environment.systemPackages = [
    # Full screen, hardware accelerated screenshots.  Works for Wayland.
    pkgs.grim
    pkgs.i3status
    pkgs.i3blocks
    pkgs.i3lock-color
    # Full screen, hardware accelerated screenshots.  Works for X11.
    pkgs.scrot
    pkgs.wmctrl
    pkgs.wmname
  ];
  imports = [
    ./unfree-predicates.nix
  ];
  # Required for touchpad support, and some managers such as hyprland require
  # it.
  services.libinput = {
    enable = true;
  };
  # Strangely, this is required to enable Gnome.  I haven't been able to find
  # any tickets on this and the only documentation I can find about Gnome
  # (https://nixos.wiki/wiki/GNOME) carries no mention of it.
  allowUnfreePackagePredicates = [
    (pkg: builtins.elem (lib.getName pkg) [
      "vpnc"
    ])
  ];
  services.xserver = {
    enable = true;
    desktopManager.gnome.enable = true;
    displayManager.startx.enable = true;
    displayManager.gdm = {
      enable = true;
      wayland = false;
    };
    windowManager = {
      qtile.enable = true;
      bspwm.enable = true;
      i3.enable = true;
      dwm.enable = true;
    };
    # Keyboard settings.
    xkb = {
      layout = "us";
      variant = "";
    };
  };
  # TODO: Move this to separate module.
  # Enable sound with pipewire.
  # Actually this is broken per: https://github.com/NixOS/nixpkgs/issues/319809
  # sound.enable = true;
  hardware.pulseaudio = {
    enable = false;
    support32Bit = false;
  };
  # Enable RealtimeKit, which allows real time scheduling priority for user
  # processes.  This allows the PulseAudio server to do its work in real time.
  security.rtkit.enable = true;
  # A security policy manager that should interact somehow with rtkit...
  # TODO: This doesn't belong here, I think.  I'm not even sure what
  # setting this accomplished.  It was part of a desperate debugging
  # effort that is now over.
  security.polkit.enable = true;
  # TODO: This doesn't belong here, I think.  I'm not even sure what
  # setting this accomplished.  It was part of a desperate debugging
  # effort that is now over.
  hardware.nvidia.forceFullCompositionPipeline = false;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
    # Use the example session manager (no others are packaged yet so this is
    # enabled by default, no need to redefine it in your config for now).
    # media-session.enable = true;
  };
}
