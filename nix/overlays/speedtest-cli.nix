# I added speedtest-cli as an overlay because 1: it's kind of useful and 2: it's
# fairly simple as far as Python packages go. It uses setup.py. I managed to
# find a decent working sample in this location:
# https://github.com/NixOS/nixpkgs/blob/8284fc30c84ea47e63209d1a892aca1dfcd6bdf3/pkgs/applications/networking/instant-messengers/salut-a-toi/default.nix
# And a more complicated one here:
# https://github.com/NixOS/nixpkgs/blob/8284fc30c84ea47e63209d1a892aca1dfcd6bdf3/pkgs/servers/tautulli/default.nix
#
# This is a working Python application constructed in an overlay. In the
# consuming nix file it is simply presented as "pkgs.speedtest-cli". I might
# like to keep this around as an example.
self: super: {
  speedtest-cli = super.python39.pkgs.buildPythonApplication rec {
    pname = "speedtest-cli";
    version = "2.1.3";
    format = "other";

    pythonPath = [ super.python39.pkgs.setuptools ];
    nativeBuildInputs = [
      super.python39.pkgs.wrapPython
      super.makeWrapper
    ];

    src = super.python39.pkgs.fetchPypi {
      inherit pname version;
      sha256 = "1w4h7m0isbvfy4zx6m5j4594p5y4pjbpzsr0h4yzmdgd7hip69sy";
    };
    buildPhase = ''
      ${super.python39.interpreter} setup.py build
    '';

    installPhase = ''
      ${super.python39.interpreter} setup.py install --prefix="$out"
      for i in "$out/bin"/*; do
      head -n 1 "$i" | grep -E '[/ ]python( |$)' && {
        wrapProgram "$i" --prefix PYTHONPATH : "$PYTHONPATH:$out/${super.python39.sitePackages}"
      } || true
      done
    '';

    doCheck = false;

    meta  = with super.lib; {
      description = "Command line interface for testing internet bandwidth using speedtest.net";
      homepage = "https://github.com/sivel/speedtest-cli";
      # TODO: Change to apache 2.0.
      license = licenses.mit;
      platforms = super.lib.platforms.linux ++ super.lib.platforms.darwin;
      # What should I put here? I don't maintain this, do I?
      # maintainers = with maintainers; [ somename ];
    };
  };
}
