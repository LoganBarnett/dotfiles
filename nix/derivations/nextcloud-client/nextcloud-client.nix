# TODO: I get some build errors here with regards to KDE (via karchive), but I
# think that's a misdirected error.  I suspect what's happening is we're not
# supplying enough to the Darwin build.  Some things it's missing:
# 1. A dependency for the in-repo swift application: swift-argument-parser.
# 2. zsh
# 3. Python, not confirmed but by virtue of the docs.
# 4. Possibly many more.
# The build programs could be attempting to execute and interpreting _any_ error
# as a configuration setting or some unavailability of certain libraries.
# In addition, the build could be looking inversely at the availability of
# certain libraries and then making assumptions that it's Linux instead of
# Darwin.  So for example, it might find some QT packages and say "yep, that's
# Linux", and abandoning any macOS settings before we even got to see if they
# work or not.
# More investigation is required, and it might be helpful to open an issue on
# nextcloud itself since I'm very unfamiliar with how macOS desktop applications
# are built.
{
  bzip2,
  lib,
  gitUpdater,
  fetchFromGitHub,
  qt6Packages,
  stdenv,
  cmake,
  extra-cmake-modules,
  inkscape,
  inotify-tools,
  kdePackages,
  libcloudproviders,
  libp11,
  librsvg,
  libsecret,
  openssl,
  pcre,
  pkg-config,
  sphinx,
  sqlite,
  swift,
  xdg-utils,
  libsysprof-capture,
}:

stdenv.mkDerivation (lib.optionalAttrs stdenv.isDarwin {
  buildPhase = ''
    cd admin/osx/mac-crafter
    ${swift}/bin/swift run mac-crafter \
      --disable-autoupdater
  '';
} // rec {
  pname = "nextcloud-client";
  version = "3.16.4";

  outputs = [
    "out"
    "dev"
  ];

  src = fetchFromGitHub {
    owner = "nextcloud-releases";
    repo = "desktop";
    tag = "v${version}";
    hash = "sha256-8P73YitjuU9SGDBNimqJsvSfGOE9lNCVUNN3f4KXWSY=";
  };

  patches = [
    ./0001-When-creating-the-autostart-entry-do-not-use-an-abso.patch
  ];

  postPatch = if stdenv.isLinux then ''
    for file in src/libsync/vfs/*/CMakeLists.txt; do
      substituteInPlace $file \
        --replace-fail "PLUGINDIR" "KDE_INSTALL_PLUGINDIR"
    done
  '' else "";

  nativeBuildInputs = [
    bzip2
    bzip2.dev
    pkg-config
    cmake
    extra-cmake-modules
    librsvg
    sphinx
    qt6Packages.wrapQtAppsHook
  ];

  buildInputs = [
    bzip2
    bzip2.dev
    (kdePackages.karchive.overrideAttrs (old: {
      cmakeFlags = (old.cmakeFlags ++ [
        "-DBZIP2_INCLUDE_DIR=${bzip2.dev}/include"
        "-DBZIP2_LIBRARY=${bzip2.out}/lib/libbz2.dylib"
        "-DBZIP2_LIBRARIES=${bzip2.out}/lib"
      ]);
      postPatch = ''
        substituteInPlace src/kbzip2filter.cpp \
          --replace 'bzDecompress' 'BZ2_bzDecompress'
        substituteInPlace src/kbzip2filter.cpp \
          --replace 'bzCompress' 'BZ2_bzCompress'
        substituteInPlace CMakeLists.txt \
          --replace 'add_subdirectory(desktop)' ""
      '';
    }))
    libcloudproviders
    libp11
    libsecret
    openssl
    pcre
    qt6Packages.qt5compat
    qt6Packages.qtbase
    qt6Packages.qtkeychain
    qt6Packages.qtsvg
    qt6Packages.qttools
    qt6Packages.qtwebengine
    qt6Packages.qtwebsockets
    sqlite
    libsysprof-capture
  ] ++ (lib.optional stdenv.isLinux [
    inotify-tools
    kdePackages.kio
    qt6Packages.qtwayland
  ])
  ++ (lib.optional stdenv.isDarwin [
    inkscape
  ])
  ;

  qtWrapperArgs = [
    "--prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath [ libsecret bzip2 bzip2.dev ]}"
    # make xdg-open overridable at runtime
    "--suffix PATH : ${lib.makeBinPath [ xdg-utils ]}"
  ];

  cmakeFlags = [
    "-DCMAKE_PREFIX_PATH=${bzip2.dev}"
    "-DBUILD_UPDATER=off"
    "-DCMAKE_INSTALL_LIBDIR=lib" # expected to be prefix-relative by build code setting RPATH
    "-DMIRALL_VERSION_SUFFIX=" # remove git suffix from version
  ];



  postBuild = ''
    make doc-man
  '';

  passthru.updateScript = gitUpdater { rev-prefix = "v"; };

  meta = {
    changelog = "https://github.com/nextcloud/desktop/releases/tag/v${version}";
    description = "Desktop sync client for Nextcloud";
    homepage = "https://nextcloud.com";
    license = lib.licenses.gpl2Plus;
    maintainers = with lib.maintainers; [
      kranzes
      SuperSandro2000
    ];
    platforms = lib.platforms.unix;
    mainProgram = "nextcloud";
  };
})
