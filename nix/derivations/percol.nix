{ lib, pkgs, ... }: pkgs.python3.pkgs.buildPythonApplication rec {
  pname = "percol";
  version = "0.1.0";
  format = "other";

  pythonPath = [ pkgs.python3.pkgs.setuptools ];
  nativeBuildInputs = [
    pkgs.cacert
    pkgs.python3.pkgs.wrapPython
    pkgs.makeWrapper
  ];

  src = pkgs.python3.pkgs.fetchPypi {
    inherit pname version;
    sha256 = "1bchvqf4prdmfm1cg6y2i76kcd3jwmzz5wmlx1zhi7f3asgksjf8";
  };
  buildPhase = ''
      ${pkgs.python3.interpreter} setup.py build
    '';

  installPhase = ''
      ${pkgs.python3.interpreter} setup.py install --prefix="$out"
      for i in "$out/bin"/*; do
      head -n 1 "$i" | grep -E '[/ ]python( |$)' && {
        wrapProgram "$i" --prefix PYTHONPATH : "$PYTHONPATH:$out/${pkgs.python3.sitePackages}"
      } || true
      done
    '';

  doCheck = false;

  meta = {
    description = "Adds flavor of interactive selection to the traditional pipe concept on UNIX";
    homepage = "https://github.com/mooz/percol/";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux ++ lib.platforms.darwin;
    # What should I put here? I don't maintain this, do I?
    # maintainers = with maintainers; [ somename ];
  };
}
