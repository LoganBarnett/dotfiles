# I added percol as an overlay because 1: it's kind of useful and 2: it's fairly
# simple as far as Python packages go. It uses setup.py. I managed to find a
# decent working sample in this location:
# https://github.com/NixOS/nixpkgs/blob/8284fc30c84ea47e63209d1a892aca1dfcd6bdf3/pkgs/applications/networking/instant-messengers/salut-a-toi/default.nix
# And a more complicated one here:
# https://github.com/NixOS/nixpkgs/blob/8284fc30c84ea47e63209d1a892aca1dfcd6bdf3/pkgs/servers/tautulli/default.nix
#
# This is a working Python application constructed in an overlay. In the
# consuming nix file it is simply presented as "pkgs.percol". I might like to
# keep this around as an example.
self: super: {
  percol = super.python3.pkgs.buildPythonApplication rec {
    pname = "percol";
    version = "0.1.0";
    format = "other";

    pythonPath = [ super.python3.pkgs.setuptools ];
    nativeBuildInputs = [
      super.python3.pkgs.wrapPython
      super.makeWrapper
    ];

    src = super.python3.pkgs.fetchPypi {
      inherit pname version;
      sha256 = "1bchvqf4prdmfm1cg6y2i76kcd3jwmzz5wmlx1zhi7f3asgksjf8";
    };
    buildPhase = ''
      ${super.python3.interpreter} setup.py build
    '';

    installPhase = ''
      ${super.python3.interpreter} setup.py install --prefix="$out"
      for i in "$out/bin"/*; do
      head -n 1 "$i" | grep -E '[/ ]python( |$)' && {
        wrapProgram "$i" --prefix PYTHONPATH : "$PYTHONPATH:$out/${super.python3.sitePackages}"
      } || true
      done
    '';

    doCheck = false;

    meta  = with super.lib; {
      description = "Adds flavor of interactive selection to the traditional pipe concept on UNIX";
      homepage = "https://github.com/mooz/percol/";
      license = licenses.mit;
      platforms = super.lib.platforms.linux ++ super.lib.platforms.darwin;
      # What should I put here? I don't maintain this, do I?
      # maintainers = with maintainers; [ somename ];
    };
  };
}
