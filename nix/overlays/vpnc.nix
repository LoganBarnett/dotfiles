# This still needs some work. I get:
#
# > clang -O3 -g -W -Wall -Wmissing-declarations -Wwrite-strings -I/nix/store/rjw53jq2nmksjfirvg00hswzhi48pl76-libgcrypt-1.10.2-dev/include -DOPENSSL_GPL_VIOLATION -DCRYPTO_OPENSSL -DVERSION=\"0.5.3\" -DSCRIPT_PATH=\"/nix/store/vpwqhg0z5nwqldrzfqi94vhc32c000aq-vpnc-scripts-unstable-2023-01-03/bin/vpnc-script\"  -c -o src/vpnc.o src/vpnc.c
# > clang -g -o /private/tmp/nix-build-vpnc-unstable-2021-11-04.drv-0/source/bin/vpnc src/sysdep.o src/vpnc-debug.o src/isakmp-pkt.o src/tunip.o src/config.o src/dh.o src/math_group.o src/supp.o src/decrypt-utils.o src/crypto.o src/crypto-openssl.o src/vpnc.o -L/nix/store/cqbgzgqaiadkxzgyw84gwbvnjlvhza7c-libgcrypt-1.10.2/lib -lgcrypt -lcrypto
# > Undefined symbols for architecture arm64:
# >   "_get_current_dir_name", referenced from:
# >       _vpnc_doit in tunip.o
# > ld: symbol(s) not found for architecture arm64
# > clang-11: error: linker command failed with exit code 1 (use -v to see invocation)
# > make: *** [Makefile:96: vpnc] Error 1
#
# This might be related: https://github.com/NixOS/nixpkgs/issues/19098
# There presently is no issue for arm64 specifically, or anything about darwin.
# What I have below is a derivation converted to overlay form that is copied
# directly from:
# https://github.com/NixOS/nixpkgs/blob/0c7ffbc66e6d78c50c38e717ec91a2a14e0622fb/pkgs/tools/networking/vpnc/default.nix
# I should make a ticket perhaps.
final: prev:
let
  opensslSupport = true;
in
{
  # { lib, stdenv, fetchFromGitHub, fetchpatch
  # , makeWrapper, pkg-config, perl
  # , gawk, gnutls, libgcrypt, openresolv, vpnc-scripts
  # , opensslSupport ? false, openssl # Distributing this is a GPL violation.
  # }:

  # stdenv.mkDerivation {
  vpnc = prev.stdenv.mkDerivation {
    pname = "vpnc";
    version = "unstable-2021-11-04";

    src = prev.fetchFromGitHub {
      owner = "streambinder";
      repo = "vpnc";
      rev = "c8bb5371b881f8853f191c495e762f834c9def5d";
      sha256 = "1j1p83nfc2fpwczjcggsby0b44hk97ky0s6vns6md3awlbpgdn57";
      fetchSubmodules = true;
    };

    nativeBuildInputs = [ prev.makeWrapper prev.pkg-config];
                        # ++ prev.lib.optional (!opensslSupport) prev.pkg-config;
    buildInputs = [ prev.libgcrypt prev.perl ]
                  ++ (if opensslSupport then [ prev.openssl ] else [ prev.gnutls ]);

    makeFlags = [
      "PREFIX=$(out)"
      "ETCDIR=$(out)/etc/vpnc"
      "SCRIPT_PATH=${prev.vpnc-scripts}/bin/vpnc-script"
    ] ++ prev.lib.optional opensslSupport "OPENSSL_GPL_VIOLATION=yes";

    postPatch = ''
    patchShebangs src/makeman.pl
  '';

    enableParallelBuilding = true;
    # Missing install depends:
    #   install: target '...-vpnc-unstable-2021-11-04/share/doc/vpnc': No such file or directory
    #   make: *** [Makefile:149: install-doc] Error 1
    enableParallelInstalling = false;

    meta = with prev.lib; {
      homepage = "https://davidepucci.it/doc/vpnc/";
      description = "Virtual private network (VPN) client for Cisco's VPN concentrators";
      license = if opensslSupport then licenses.unfree else licenses.gpl2Plus;
      platforms = prev.lib.platforms.linux ++ prev.lib.platforms.darwin;
    };
  };
}
