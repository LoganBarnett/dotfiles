{ config, lib, ... }: {
  imports = [
    (import ../nixos-modules/desktop-dark.nix { user = "logan"; })
  ];
  services.keyd.enable = true;
  services.keyd.keyboards = {
    default = {
      ids = [ "*" ];
      # The docs say to use "man keyd" but they aren't installed on NixOS (yet,
      # need to file a ticket).  Until then see:
      # https://man.archlinux.org/man/extra/keyd/keyd.1.en
      settings = {
        main = {
          capslock = "layer(control)";
        };
        alt = {
          # Cut, copy, paste.
          x = "C-x";
          c = "C-c";
          v = "C-v";
          # Select all.
          a = "C-a";
        };
        # Read line bindings!
        control = {
          a = "home";
          e = "end";
          f = "right";
          b = "left";
          n = "down";
          p = "up";
          d = "delete";
          # Doesn't seem to work.  Perhaps there's another?
          k = "clear";
        };
        # Sometimes holding fn for arrows doesn't work well.  Toggle into it.
        "control+alt" = {
          a = "toggle(vim-like-arrows)";
        };
        vim-like-arrows = {
          h = "left";
          j = "down";
          k = "up";
          l = "right";
          # Always provide a means to get back.
          "control+alt+shift+a" = "swap(main)";
        };
      };
    };
  };
  home-manager.users.logan = {
    home.stateVersion = config.system.stateVersion;
    dconf.settings = {
      "org/gnome/desktop/interface" = {
        clock-format = "24h";
      };
      "org/gnome/desktop/peripherals/mouse" = {
        natural-scroll = true;
      };
      "org/gnome/desktop/peripherals/touchpad" = {
        natural-scroll = true;
      };
      # "org/gnome/desktop/wm/keybindings" = {
      #   # TODO: Switch these keys.
      #   switch-input-source = [ "<Alt>Tab" ];
      #   switch-input-source-backward = [ "<Shift><Alt>Tab" ];
      # };
      # "org/gnome/desktop/input-sources" = {
      #   sources = [
      #     (lib.gvariant.mkTuple [
      #       "xkb"
      #       "us"
      #     ])
      #     (lib.gvariant.mkTuple [
      #       "xkb"
      #       "ru"
      #     ])
      #   ];
      # };
    };
  };
}
