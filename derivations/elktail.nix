{
  buildGoModule,
  fetchFromGitHub,
  lib,
  ...
}: let
  pname = "elktail";
  version = "unstable-2023-01-27";
in buildGoModule {
  inherit pname version;

  src = fetchFromGitHub {
    # Note that this is the most active and current fork.  Notably it uses
    # go.mod et. al, which is needed to build this in Nix.  It uses a more
    # recent CLI library, and most importantly has Elasticsearch v7 support.
    owner = "solidgate-tech";
    repo = "elktail";
    # There are no releases on this fork yet.
    rev = "7f8c378";
    hash = "sha256-6vGq/Lsy+EtGKKZOAdur4nO3MIAkGtvAyk+4vCX1CEo=";
  };

  vendorHash = "sha256-u4xxBgIOxr2wI3Or6Q7ps7l7Z6/476r2SRisqkG8WM8=";

  meta = {
    description = ''
      Command line utility to query, search and tail EL (elasticsearch, logstash) logs.
      Fork with Elasticsearch v7 support and modernized dependencies.
    '';
    homepage = "https://github.com/solidgate-tech/elktail";
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
  };
}
