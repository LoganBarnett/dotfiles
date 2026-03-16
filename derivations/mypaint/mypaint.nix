################################################################################
# Copy of
# https://github.com/NixOS/nixpkgs/blob/b599843bad24621dcaa5ab60dac98f9b0eb1cabe/pkgs/by-name/my/mypaint/package.nix
# in hopes of getting macOS/Darwin support.
#
# The program builds just fine.  The problem is the runtime.  Here is a sample
# invocation with a large dump full of things that need addressing:
# $ mypaint
# INFO: mypaint: Installation layout: conventional POSIX-like structure with prefix '/nix/store/daf1yb7gkhlp68ldhsp87jc786byjvis-mypaint-2.0.1'
# WARNING: gui.userconfig: Failed to load settings file: /Users/logan/.config/mypaint/settings.json
# WARNING: gui.userconfig: Failed to load settings: using defaults
# INFO: gui.main: No locale setting found, using system locale
# ERROR: lib.i18n: OSX: failed to import AppKit.NSLocale
# Traceback (most recent call last):
#   File "/nix/store/daf1yb7gkhlp68ldhsp87jc786byjvis-mypaint-2.0.1/lib/mypaint/lib/i18n.py", line 143, in set_i18n_envvars
#     from AppKit import NSLocale
# ModuleNotFoundError: No module named 'AppKit'
# WARNING: lib.i18n: OSX: falling back to POSIX mechanisms.
# INFO: lib.i18n: OSX: LANG='en_US.UTF-8'
# INFO: lib.i18n: OSX: LANGUAGE=None
# WARNING: /nix/store/daf1yb7gkhlp68ldhsp87jc786byjvis-mypaint-2.0.1/lib/mypaint/lib/gettext_setup.py: No bindtextdomain builtins found in module 'locale'.
# INFO: /nix/store/daf1yb7gkhlp68ldhsp87jc786byjvis-mypaint-2.0.1/lib/mypaint/lib/gettext_setup.py: Trying platform-specific fallback hacks to find bindtextdomain funcs.
# ERROR: /nix/store/daf1yb7gkhlp68ldhsp87jc786byjvis-mypaint-2.0.1/lib/mypaint/lib/gettext_setup.py: No platform-specific fallback for locating bindtextdomain is known for 'darwin'
# INFO: gui.application: Created basedir '/Users/logan/.config/mypaint'
# INFO: gui.application: Created basedir '/Users/logan/.local/share/mypaint'
# INFO: gui.application: Created data subdir '/Users/logan/.local/share/mypaint/backgrounds'
# INFO: gui.application: Created data subdir '/Users/logan/.local/share/mypaint/brushes'
# INFO: gui.application: Created data subdir '/Users/logan/.local/share/mypaint/scratchpads'
#
# (.mypaint-wrapped:16203): Gtk-WARNING **: 09:44:53.006: Could not find the icon 'org.mypaint.MyPaint'. The 'hicolor' theme
# was not found either, perhaps you need to install it.
# You can get a copy from:
# 	http://icon-theme.freedesktop.org/releases
# ERROR: gui.application: Missing icon 'org.mypaint.MyPaint': check that mypaint icons have been installed into /nix/store/daf1yb7gkhlp68ldhsp87jc786byjvis-mypaint-2.0.1/share/icons
# Traceback (most recent call last):
#   File "/nix/store/daf1yb7gkhlp68ldhsp87jc786byjvis-mypaint-2.0.1/lib/mypaint/gui/application.py", line 135, in _init_icons
#     icon_theme.load_icon(icon_name, 32, 0)
# gi.repository.GLib.GError: gtk-icon-theme-error-quark: Icon 'org.mypaint.MyPaint' not present in theme Adwaita (0)
# ERROR: gui.application: Missing icon 'mypaint-brush-symbolic': check that librsvg is installed, and update loaders.cache
# Traceback (most recent call last):
#   File "/nix/store/daf1yb7gkhlp68ldhsp87jc786byjvis-mypaint-2.0.1/lib/mypaint/gui/application.py", line 135, in _init_icons
#     icon_theme.load_icon(icon_name, 32, 0)
# gi.repository.GLib.GError: gtk-icon-theme-error-quark: Icon 'mypaint-brush-symbolic' not present in theme Adwaita (0)
# CRITICAL: gui.application: Required icon(s) missing
# ERROR: gui.application: Icon search path: ['/Users/logan/.local/share/icons', '/Users/logan/.icons', '/nix/store/daf1yb7gkhlp68ldhsp87jc786byjvis-mypaint-2.0.1/share/icons', '/nix/store/7hp7awp9s2kc8ng879bhmivv0y09r2vq-gsettings-desktop-schemas-48.0/share/gsettings-schemas/gsettings-desktop-schemas-48.0/icons', '/nix/store/8vibmbcp6vhhv6kp8z57i5kb6w34rw1c-gtk+3-3.24.49/share/gsettings-schemas/gtk+3-3.24.49/icons', '/Users/logan/.nix-profile/share/icons', '/etc/profiles/per-user/logan/share/icons', '/run/current-system/sw/share/icons', '/nix/var/nix/profiles/default/share/icons', '/Users/logan/.nix-profile/share/icons', '/nix/var/nix/profiles/default/share/icons', '/nix/store/daf1yb7gkhlp68ldhsp87jc786byjvis-mypaint-2.0.1/share/pixmaps', '/nix/store/7hp7awp9s2kc8ng879bhmivv0y09r2vq-gsettings-desktop-schemas-48.0/share/gsettings-schemas/gsettings-desktop-schemas-48.0/pixmaps', '/nix/store/8vibmbcp6vhhv6kp8z57i5kb6w34rw1c-gtk+3-3.24.49/share/gsettings-schemas/gtk+3-3.24.49/pixmaps', '/Users/logan/.nix-profile/share/pixmaps', '/etc/profiles/per-user/logan/share/pixmaps', '/run/current-system/sw/share/pixmaps', '/nix/var/nix/profiles/default/share/pixmaps', '/Users/logan/.nix-profile/share/pixmaps', '/nix/var/nix/profiles/default/share/pixmaps', '/nix/store/daf1yb7gkhlp68ldhsp87jc786byjvis-mypaint-2.0.1/share/icons']
# ERROR: gui.application: MyPaint can't run sensibly without its icons; please check your installation. See https://github.com/mypaint/mypaint/wiki/FAQ-Missing-icons for possible solutions.
################################################################################
{
  adwaita-icon-theme,
  lib,
  fetchFromGitHub,
  fetchpatch,
  gtk3,
  gettext,
  json_c,
  lcms2,
  libpng,
  librsvg,
  gobject-introspection,
  libmypaint,
  hicolor-icon-theme,
  mypaint-brushes,
  gdk-pixbuf,
  pkg-config,
  python3,
  swig,
  stdenv,
  wrapGAppsHook3,
}:

let
  inherit (python3.pkgs)
    pycairo
    pygobject3
    pyobjc-core
    pyobjc-framework-Cocoa
    numpy
    buildPythonApplication
    ;
in
buildPythonApplication rec {
  pname = "mypaint";
  version = "2.0.1";
  format = "other";

  src = fetchFromGitHub {
    owner = "mypaint";
    repo = "mypaint";
    tag = "v${version}";
    hash = "sha256-rVKcxzWZRLcuxK8xRyRgvitXAh4uOEyqHswLeTdA2Mk=";
    fetchSubmodules = true;
  };

  patches = [
    # Fix build due to setuptools issue.
    # https://github.com/mypaint/mypaint/pull/1183
    (fetchpatch {
      url = "https://github.com/mypaint/mypaint/commit/423950bec96d6057eac70442de577364d784a847.patch";
      hash = "sha256-OxJJOi20bFMRibL59zx6svtMrkgeMYyEvbdSXbZHqpc=";
    })
    # https://github.com/mypaint/mypaint/pull/1193
    (fetchpatch {
      name = "python-3.11-compatibility.patch";
      url = "https://github.com/mypaint/mypaint/commit/032a155b72f2b021f66a994050d83f07342d04af.patch";
      hash = "sha256-EI4WJbpZrCtFMKd6QdXlWpRpIHi37gJffDjclzTLaLc=";
    })
    # Fix drag-n-drop file opening
    (fetchpatch {
      url = "https://github.com/mypaint/mypaint/commit/66b2ba98bd953afa73d0d6ac71040b14a4ea266b.patch";
      hash = "sha256-4AWXD/JMpNA5otl2ad1ZLVPW49pycuOXGcgfzvj0XEE=";
    })
    # Fix crash with locked layer
    (fetchpatch {
      url = "https://github.com/mypaint/mypaint/commit/0b720f8867f18acccc8e6ec770a9cc494aa81dcf.patch";
      hash = "sha256-ahYeERiMLA8yKIXQota6/ApAbOW0XwsHO2JkEEMm1Ow=";
    })
    # Refactoring for the following patch to apply.
    (fetchpatch {
      url = "https://github.com/mypaint/mypaint/commit/d7d2496401a112a178d5fa2e491f0cc7537d24cd.patch";
      hash = "sha256-dIW6qWqY96+bsUDQQtGtjENvypnh//Ep3xW+wooCJ14=";
      includes = [
        "gui/colors/hcywheel.py"
      ];
    })
    # Fix crash with hcy wheel masking
    (fetchpatch {
      url = "https://github.com/mypaint/mypaint/commit/5496b1cd1113fcd46230d87760b7e6b51cc747bc.patch";
      hash = "sha256-h+sE1LW04xDU2rofH5KqXsY1M0jacfBNBC+Zb0i6y1w=";
    })
    # Darwin gettext support
    ./mypaint-darwin-gettext.patch
  ];

  nativeBuildInputs = [
    adwaita-icon-theme
    gdk-pixbuf
    gettext
    pkg-config
    swig
    wrapGAppsHook3
    gobject-introspection # for setup hook
    hicolor-icon-theme # fór setup hook
    python3.pkgs.setuptools
  ];

  buildInputs = [
    adwaita-icon-theme
    # Required to be present by Darwin.
    gettext
    gtk3
    gdk-pixbuf
    libmypaint
    mypaint-brushes
    json_c
    lcms2
    libpng
    librsvg
    pycairo
    pygobject3

    # Mypaint checks for a presence of this theme scaffold and crashes when not present.
    hicolor-icon-theme
  ];

  propagatedBuildInputs = [
    numpy
    pycairo
    pygobject3
  ]
  ++ lib.optional stdenv.isDarwin [ pyobjc-core pyobjc-framework-Cocoa ]
  ;

  nativeCheckInputs = [
    gtk3
  ];

  buildPhase = ''
    runHook preBuild

    ${python3.interpreter} setup.py build

    runHook postBuild
  '';

  preFixup = ''
    # Create a private loaders dir and symlink both sets of loaders into it.
    loaders="$out/lib/gdk-pixbuf-2.0/2.10.0/loaders"
    mkdir -p "$loaders"
    for d in \
        "${gdk-pixbuf}/lib/gdk-pixbuf-2.0/2.10.0/loaders" \
        "${librsvg}/lib/gdk-pixbuf-2.0/2.10.0/loaders"
    do
      if [ -d "$d" ]; then
        for lib in "$d"/*.so "$d"/*.dylib; do
          [ -e "$lib" ] || continue
          ln -sf "$lib" "$loaders/$(basename "$lib")"
        done
      fi
    done

    # Generate a combined loaders.cache for macOS.
    # Use gdk-pixbuf's existing cache as a base, then append librsvg's loader.
    cat "${gdk-pixbuf}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache" > "$loaders.cache"
    # Extract just the loader line from librsvg's cache
    if [ -f "${librsvg}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache" ]; then
      grep "^\"" "${librsvg}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache" | \
        sed "s|${librsvg}|$out|g" >> "$loaders.cache"
    fi

    # Wrap so GTK sees our icons and uses our private loader dir (incl. SVG).
    gappsWrapperArgs+=(
      --prefix XDG_DATA_DIRS : "$out/share"
      --prefix XDG_DATA_DIRS : "${hicolor-icon-theme}/share"
      --prefix XDG_DATA_DIRS : "${adwaita-icon-theme}/share"
      --unset GDK_PIXBUF_MODULE_FILE
      --set GDK_PIXBUF_MODULE_FILE "$loaders.cache"
      --prefix PATH : "${gettext}/bin"
      --prefix DYLD_LIBRARY_PATH : "${gettext}/lib"
      # Optional while debugging:
      # --set GTK_ICON_THEME Adwaita
    )
  '';

  installPhase = ''
    runHook preInstall

    ${python3.interpreter} setup.py managed_install --prefix=$out

    # For Darwin - Put MyPaint's icons into hicolor theme (fallback).
    # Don't put them in Adwaita because GTK stops at the first Adwaita it finds.
    install -D -m0644 desktop/icons/hicolor/scalable/apps/org.mypaint.MyPaint.svg \
      "$out/share/icons/hicolor/scalable/apps/org.mypaint.MyPaint.svg"
    install -D -m0644 "$out/share/icons/hicolor/scalable/apps/org.mypaint.MyPaint.svg" \
      "$out/share/icons/hicolor/symbolic/apps/mypaint-brush-symbolic.svg"

    # Provide theme index in _this_ prefix so GTK recognizes hicolor here.
    install -D -m0644 "${hicolor-icon-theme}/share/icons/hicolor/index.theme" \
      "$out/share/icons/hicolor/index.theme"

    # For Darwin - Build the theme cache (some GTK builds need it present).
    ${gtk3}/bin/gtk-update-icon-cache --force --ignore-theme-index \
      "$out/share/icons/hicolor" || true
    runHook postInstall
  '';

  # tests require unmaintained and removed nose, it should switch to pytest
  # https://github.com/mypaint/mypaint/issues/1191
  doCheck = false;

  checkPhase = ''
    runHook preCheck

    HOME=$TEMPDIR ${python3.interpreter} setup.py test

    runHook postCheck
  '';

  meta = {
    description = "Graphics application for digital painters";
    homepage = "http://mypaint.org/";
    license = lib.licenses.gpl2Plus;
    maintainers = with lib.maintainers; [ jtojnar ];
  };
}
