################################################################################
#
################################################################################
{ config, host-id, ... }: {
  imports = [
    ../nixos-modules/tigervnc.nix
  ];
  age.secrets."${host-id}-vnc-password" = {
    generator.script = "long-passphrase";
    mode = "0600";
    owner = "logan";
    rekeyFile = ../secrets/${host-id}-vnc-password.age;
  };
  environment.etc."X11/xinit/Xsession" = {
    mode = "0755";
    text = ''
      #!/bin/sh
      exec gnome-session
    '';
  };
  services.tigervnc = {
    # enable = true;
    # example: open on display :1 -> port 5901
    # But don't use :1 on a desktop where you expect an X session to already be
    # active.
    displays."2" = {
      user = "logan";          # change to your user
      geometry = "1920x1080";
      depth = 24;
      # securityTypes = [ "TLSVnc" ];  # enforce TLS encryption
      passwordFile = config.age.secrets."${host-id}-vnc-password".path;
      openFirewall = true;
      # xstartup = "/etc/vnc-xstartup";
    };
  };
}
