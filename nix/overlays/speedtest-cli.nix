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
final: prev: let
  # Speedtest specifically supports Python 3.10 as its most recent version.
  python = prev.python310;
  pname = "speedtest-cli";
  version = "2.1.3";
in {
  speedtest-cli = python.pkgs.buildPythonApplication {
    inherit pname version;

    pyproject = true;

    nativeBuildInputs = [
      prev.cacert
    ];

    src = python.pkgs.fetchPypi {
      inherit pname version;
      hash = "sha256-XidzIzzttfo9gSDrf5e8xJdLUiGyVNM/8W4vHUE9kPA=";
    };

    build-system = [
      python.pkgs.setuptools
    ];

    dependencies = [];

    meta = with prev.lib; {
      description = "Command line interface for testing internet bandwidth using speedtest.net.";
      homepage = "https://github.com/sivel/speedtest-cli";
      license = licenses.asl20;
      platforms = prev.lib.platforms.linux ++ prev.lib.platforms.darwin;
      # What should I put here? I don't maintain this, do I?
      # maintainers = with maintainers; [ somename ];
    };
  };
}
