{
  lib,
  config,
  clangStdenv,
  fetchFromGitHub,
  autoconf,
  automake,
  libblocksruntime,
  libtool,
  intltool,
  pkg-config,
  jansson,
  stdenv,
  swift-corelibs-libdispatch,
  # deadbeef can use either gtk2 or gtk3
  gtk2Support ? false,
  gtk2,
  gtk3Support ? true,
  gtk3,
  gsettings-desktop-schemas,
  wrapGAppsHook3,
  # input plugins
  vorbisSupport ? true,
  libvorbis,
  mp123Support ? true,
  libmad,
  flacSupport ? true,
  flac,
  wavSupport ? true,
  libsndfile,
  cdaSupport ? true,
  libcdio,
  libcddb,
  aacSupport ? true,
  faad2,
  opusSupport ? true,
  opusfile,
  wavpackSupport ? false,
  wavpack,
  ffmpegSupport ? false,
  ffmpeg,
  apeSupport ? true,
  yasm,
  # misc plugins
  zipSupport ? true,
  libzip,
  artworkSupport ? true,
  imlib2,
  hotkeysSupport ? true,
  libX11,
  osdSupport ? true,
  dbus,
  # output plugins
  alsaSupport ? stdenv.isLinux,
  alsa-lib,
  pulseSupport ? config.pulseaudio or stdenv.isLinux,
  libpulseaudio,
  pipewireSupport ? stdenv.isLinux,
  pipewire,
  # effect plugins
  resamplerSupport ? true,
  libsamplerate,
  overloadSupport ? true,
  zlib,
  # transports
  remoteSupport ? true,
  curl,
}:

assert gtk2Support || gtk3Support;

let
  inherit (lib) optionals;

  version = "1.10.0";
in
clangStdenv.mkDerivation {
  pname = "deadbeef";
  inherit version;

  src = fetchFromGitHub {
    owner = "DeaDBeeF-Player";
    repo = "deadbeef";
    fetchSubmodules = true;
    rev = version;
    hash = "sha256-qa0ULmE15lV2vkyXPNW9kSISQZEANrjwJwykTiifk5Q=";
  };

  buildInputs = [
    jansson
    swift-corelibs-libdispatch
  ]
  ++ optionals gtk2Support [
    gtk2
  ]
  ++ optionals gtk3Support [
    gtk3
    gsettings-desktop-schemas
  ]
  ++ optionals vorbisSupport [
    libvorbis
  ]
  ++ optionals mp123Support [
    libmad
  ]
  ++ optionals flacSupport [
    flac
  ]
  ++ optionals wavSupport [
    libsndfile
  ]
  ++ optionals cdaSupport [
    libcdio
    libcddb
  ]
  ++ optionals aacSupport [
    faad2
  ]
  ++ optionals opusSupport [
    opusfile
  ]
  ++ optionals zipSupport [
    libzip
  ]
  ++ optionals ffmpegSupport [
    ffmpeg
  ]
  ++ optionals apeSupport [
    yasm
  ]
  ++ optionals artworkSupport [
    imlib2
  ]
  ++ optionals hotkeysSupport [
    libX11
  ]
  ++ optionals osdSupport [
    dbus
  ]
  ++ optionals alsaSupport [
    alsa-lib
  ]
  ++ optionals pulseSupport [
    libpulseaudio
  ]
  ++ optionals pipewireSupport [
    pipewire
  ]
  ++ optionals resamplerSupport [
    libsamplerate
  ]
  ++ optionals overloadSupport [
    zlib
  ]
  ++ optionals wavpackSupport [
    wavpack
  ]
  ++ optionals remoteSupport [
    curl
  ]
  ++ optionals stdenv.isDarwin [
    libblocksruntime
  ]
  ;

  nativeBuildInputs = [
    autoconf
    automake
    intltool
    libtool
    pkg-config
  ]
  ++ optionals gtk3Support [
    wrapGAppsHook3
  ];

  enableParallelBuilding = true;

  preConfigure = ''
    ./autogen.sh
  '';

  configureFlags = lib.optionals stdenv.isDarwin [
    "--disable-lfm"
  ];

  postPatch = ''
    # Fix the build on c++17 compiler:
    #   https://github.com/DeaDBeeF-Player/deadbeef/issues/3012
    # TODO: remove after 1.9.5 release.
    substituteInPlace plugins/adplug/Makefile.am --replace 'adplug_la_CXXFLAGS = ' 'adplug_la_CXXFLAGS = -std=c++11 '

    # Drop x86-only -msse3 in libretro DSP plugin so non-x86 (e.g. arm64-darwin)
    # builds work.
    substituteInPlace external/ddb_dsp_libretro/Makefile.am \
      --replace-quiet "-msse3 " ""
  ''
    + lib.optionalString stdenv.isDarwin ''
      # Darwin: libdispatch is part of libSystem; there is no separate
      # -ldispatch. Remove -ldispatch from all Makefiles so plugins like lastfm
      # link.
      grep --recursive --files-with-matches -- '-ldispatch' \
        | while read -r f; do
          substituteInPlace "$f" --replace-warn '-ldispatch' ""
        done


      substituteInPlace src/streamer.c --replace-fail 'coreaudio.h' "plugins/coreaudio/coreaudio.h"
    ''


#       substituteInPlace src/main.c \
#         --replace-fail '#include "conf.h"' \
# '#include "conf.h"
# #ifdef __APPLE__
# void cocoautil_get_cache_path(char *buf, size_t size);
# void cocoautil_get_application_support_path(char *buf, size_t size);
# #endif
# '
#     + lib.optionalString stdenv.isDarwin ''
#       # Make main.c see the CoreAudio plugin headers
#       substituteInPlace src/main.c \
#         --replace-fail '#ifdef __APPLE__' '#ifdef __APPLE__
# #include "../plugins/coreaudio/coreaudio.h"
# '
#    ''
    # + lib.optionalString stdenv.isDarwin ''
    #   # Provide a stub for cocoautil_get_cache_path, which is referenced
    #   # from src/main.c in macOS builds but not declared/defined in this
    #   # autotools build.
    #   cat > cocoautil_stub.h <<'EOF'
    #   #include <stddef.h>
    #   #include <limits.h>
    #   #include <stdlib.h>
    #   #include <string.h>
    #   #include <stdio.h>

    #   static inline void cocoautil_get_cache_path(char *buf, size_t bufsize) {
    #       const char *home = getenv("HOME");
    #       if (!home || !*home) {
    #           const char *fallback = "/tmp";
    #           if (bufsize > 0) {
    #               strncpy(buf, fallback, bufsize - 1);
    #               buf[bufsize - 1] = '\0';
    #           }
    #           return;
    #       }
    #       if (bufsize > 0) {
    #           int n = snprintf(buf, bufsize, "%s/Library/Caches/DeaDBeeF", home);
    #           if (n < 0 || (size_t)n >= bufsize) {
    #               buf[bufsize - 1] = '\0';
    #           }
    #       }
    #   }
    #   EOF
    # ''
  ;

  # Make sure our stub we made in the postPatch for Darwin is used.
  # NIX_CFLAGS_COMPILE = lib.optionalString stdenv.isDarwin "-include cocoautil_stub.h";

  NIX_CFLAGS_COMPILE = lib.concatStringsSep " " [
    # Existing dispatch bits you already added, keep those:
    # "-I${swift-corelibs-libdispatch.dev}/include"
    # "-I${swift-corelibs-libdispatch}/include"
    # "-I${swift-corelibs-libdispatch}/lib/swift"
    # Make the local coreaudio header visible to any TU that wants it.
    "-Iplugins/coreaudio"
  ];

  meta = with lib; {
    description = "Ultimate Music Player for GNU/Linux";
    mainProgram = "deadbeef";
    homepage = "http://deadbeef.sourceforge.net/";
    downloadPage = "https://github.com/DeaDBeeF-Player/deadbeef";
    license = licenses.gpl2;
    platforms = [
      "aarch64-darwin"
      "x86_64-darwin"
      "x86_64-linux"
      "i686-linux"
    ];
    maintainers = [ maintainers.abbradar ];
  };
}
