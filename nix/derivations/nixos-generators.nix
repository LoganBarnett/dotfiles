{ stdenv, lib, fetchFromGitHub, makeWrapper, coreutils, jq, findutils, nix, bash }:

stdenv.mkDerivation rec {
  pname = "nixos-generators";
  version = "unstable-2024-05-20";
  src = fetchFromGitHub {
    owner = "nix-community";
    repo = "nixos-generators";
    rev = "d14b286322c7f4f897ca4b1726ce38cb68596c94";
    sha256 = "sha256-iqQa3omRcHGpWb1ds75jS9ruA5R39FTmAkeR3J+ve1w=";
  };
  strictDeps = true;
  nativeBuildInputs = [ makeWrapper ];
  buildInputs = [ bash ];
  installFlags = [ "PREFIX=$(out)" ];
  postFixup = ''
    wrapProgram $out/bin/nixos-generate \
      --prefix PATH : ${lib.makeBinPath [ jq coreutils findutils nix ] }
  '';

  meta = with lib; {
    description = "Collection of image builders";
    homepage    = "https://github.com/nix-community/nixos-generators";
    license     = licenses.mit;
    maintainers = with maintainers; [ lassulus ];
    mainProgram = "nixos-generate";
    platforms   = platforms.unix;
  };
}
