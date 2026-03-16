{ pkgs, ... }:
{
  programs.lutris = {
    enable = true;
    # Lutris wants to download its own Wine packages, which will have linking
    # problems on Nix.  Providing packages here creates symlinks into
    # ~/.local/share/lutris/runners/ so Lutris detects them without
    # downloading anything.  Note: the download UI buttons remain visible but
    # non-functional — there's no option to hide them.
    winePackages = [
      # stagingFull is the most inclusive option: it combines all optional
      # features (gstreamer, GTK, VA-API, OpenCL, gecko, mono, etc.) with the
      # staging patch set, which carries upstream-pending compatibility and
      # performance fixes.
      pkgs.wineWowPackages.stagingFull
    ];
    # GE-Proton is a community build with additional patches for game
    # compatibility, media codec support, and fixes not yet upstream.
    protonPackages = [
      pkgs.proton-ge-bin
    ];
    defaultWinePackage = pkgs.wineWowPackages.stagingFull;
    # Extra libraries injected into Lutris' FHS environment.  These cover
    # common missing .so files that cause artifacts or crashes under Wine.
    extraPackages = with pkgs; [
      gamemode
      mangohud
      vkd3d-proton
      # Media/audio
      openal
      faudio
      gst_all_1.gst-plugins-good
      gst_all_1.gst-plugins-bad
      gst_all_1.gst-plugins-ugly
      gst_all_1.gst-libav
    ];
  };
}
