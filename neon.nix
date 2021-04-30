# { pkgs ? import <nixpkgs> { overlays = [ openconnect-sso ]; } }:
{ pkgs, ... }:

# let vpnc = pkgs.fetchgit {
#   url = "git://git.infradead.org/users/dwmw2/vpnc-scripts.git";
#   rev = "c0122e891f7e033f35f047dad963702199d5cb9e";
#   sha256 = "11b1ls012mb704jphqxjmqrfbbhkdjb64j2q4k8wb5jmja8jnd14";
# };
# in
let
  # openconnectOverlay = import "${builtins.fetchTarball https://github.com/vlaci/openconnect-sso/archive/master.tar.gz}/overlay.nix";
  # pkgs = import <nixpkgs> { overlays = [ openconnectOverlay ]; };
  in
{
  # nixpkgs.overlays = [
  #   openconnect-sso
  # ];


  # What you want.
  # users.users.${config.settings.username}.shell = pkgs.zsh;
  # What you get.
  users.users.logan.shell = pkgs.zsh;

  # let customOhMyZsh = oh-my-zsh: with oh-my-zsh {
  #   enable = true;
  #   customPkgs = with pkgs; [
  #     noreallyjustfuckingstopalready
  #     zsh-git-prompt
  #   ];
  #   plugins = [
  #     "nix"
  #   ];
  #   theme = "robbyrussell";
  # };
  # namei = singleBinary "namei" {
  #   linux = pkgs.glibc.bin;
  #   darwin = pkgs.darwin.system_cmds;
  # };
  packageOverrides = pkgs: with pkgs;
    let
 in
    {
    # The pkgs part here is important, even though pkgs is in the lexical scope.
    # At the moment this is broken due to "perl532" being missing. Strangely
    # omitting the overlays declaration here makes the issue go away. The
    # problem manifests even if overlays is empty.
    # pkgs.overlays = [
      # Could be inline like this:
      #
      # (self: super: {
      #     freecad = super.callPackage ~/dev/nixpkgs/pkgs/applications/graphics/freecad/default.nix {};
      # })
      # import ./nix/overlays.nix
    # ];

# This records as a flat dependency just called 'shell-packages'. I need to find
# a way to unpack this.
    shellPackages = pkgs.buildEnv {
      extraOutputsToInstall = [ "man" "doc" ];
# "my-packages" isn't very helpful. We should make this profile "default" or
# "shell".
      name = "shell-packages";
      paths = [
        # openconnectOverlay.openconnect-sso
        # pkgs.openconnect-sso
        # nixpkgs.overlays.openconnect-sso
        openconnect-sso

      ];
      pathsToLink = [ "/Applications" "/bin" "/etc" "/share" ];
      # Get man pages synced.
      postBuild = ''
        if [ -x $out/bin/install-info -a -w $out/share/info ]; then
          shopt -s nullglob
          for i in $out/share/info/*.info $out/share/info/*.info.gz; do
              $out/bin/install-info $i $out/share/info/dir
          done
        fi
      '';
    };
  };
}
