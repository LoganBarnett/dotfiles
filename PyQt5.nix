self: super: {
  # super.python39.pkgs.PyQt5 = self.buildPythonPackage rec {
  PyQt5 = self.buildPythonPackage rec {

    pname = "PyQt5";
    version = "5.15.4";
    # disabled = isPy27;

    src = self.fetchPypi {
      inherit pname version;
      sha256 = "1gp5jz71nmg58zsm1h4vzhcphf36rbz37qgsfnzal76i1mz5js9a";
    };

    # propagatedBuildInputs = [ pytz requests freezegun ];
    doCheck = false;
    # checkInputs = [ pytest ];
    # checkPhase = ''
    #   py.test -m "not webtest"
    # '';

    meta = with self.lib; {
      description = "PyQt is a set of Python bindings for The Qt Company's Qt application framework.";
      homepage = "https://www.riverbankcomputing.com/software/pyqt/";
      # license = licenses.asl20;
      # maintainers = with maintainers; [ flokli ];
    };
  };
}
