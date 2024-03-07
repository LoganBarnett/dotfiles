# The ultimate editor.
{ pkgs, ... }: {

  environment.systemPackages = [
    (pkgs.emacsWithPackages (epkgs: [
      epkgs.mu4e
    ]))
    # Other forms left for reference.
    # aarch64 (arm) is lacking on the mainline build of emacs/emacsMacPort. This
    # branch builds. See https://github.com/NixOS/nixpkgs/pull/138424 for
    # progress on it getting merged.
    # pkgs.emacsMacport
    # ((pkgs.emacsPackagesFor pkgs.emacs-unstable).emacsWithPackages (
    # # package = ((pkgs.emacsPackagesFor pkgs.emacs).emacsWithPackages (
    #   epkgs: [
    #     epkgs.mu4e
    #   ]
    # ))
    # pkgs.emacs
  ];
}
