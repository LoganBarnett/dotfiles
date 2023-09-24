self: super: {
  wine = {
    imports = let
      wine-wow64 = fetchTarball
      "https://github.com/nixos/nixpkgs/archive/51e69b6cafaea692e3b788269a8bb262d1537a5b.tar.gz";
  in ["${wine-wow64}/pkgs/applications/emulators/wine/packages.nix"];
    # wineWow64 = callPackage ./base.nix {
    #   pname = "wine-wow64";
    #   inherit src version supportFlags patches moltenvk;
    #   pkgArches = [ pkgs ];
    #   mingwGccs = with pkgsCross; [ mingw32.buildPackages.gcc mingwW64.buildPackages.gcc ];
    #   geckos = [ gecko64 ];
    #   monos =  [ mono ];
    #   configureFlags = [ "--enable-archs=x86_64,i386" ];
    #   platforms = [ "x86_64-linux" "x86_64-darwin" ];
    #   mainProgram = "wine";
    # };
    # platforms = [ "i686-linux" "x86_64-linux" "aarch64-darwin" ];
    # wine32 = {
    #   platforms = [ "i686-linux" "x86_64-linux" "aarch64-darwin" ];
    # };
    # wine64 = {
    #   platforms = [ "i686-linux" "x86_64-linux" "aarch64-darwin" ];
    # };
    # wineWow = {
    #   platforms = [ "i686-linux" "x86_64-linux" "aarch64-darwin" ];
    # };
    # wineWow64 = {
    #   platforms = [ "i686-linux" "x86_64-linux" "aarch64-darwin" ];
    # };
  };
}
