################################################################################
# Some custom functions that get used in a lot of places, but need access to
# the NixOS module system.
#
# TODO: Move these to ../lib.nix.
################################################################################
{ config, lib, ... }: {
  # This becomes available as `pkgs.lib`.  How it can just be "lib" remains a
  # mystery.
  nixpkgs.overlays = [(final: prev: {
    lib = prev.lib // {
      custom = {

        monitor-to-exporter-name = monitor: {
          blackbox-ping = "blackbox";
          dns-smart-block = "dns-smart-block-exporter";
        }.${monitor} or monitor;

        pemToCertificates = pemData: (let
          lines = final.lib.strings.splitString "\n" pemData;
          certs-acc = final.lib.lists.foldl'
          (acc: line:
            if acc.in-cert
            then (
              if line == "-----END CERTIFICATE-----"
              then {
                in-cert = false;
                cert-lines = [];
                certs = acc.certs ++ [
                  (final.lib.strings.concatLines acc.cert-lines)
                ];
              }
              else acc // {
                cert-lines = acc.cert-lines ++ [ line ];
              }
            )
            else (
              if line == "-----BEGIN CERTIFICATE-----"
              then acc // { in-cert = true; cert-lines = []; }
              # A comment line.  Skip it.
              else acc
            )
          )
          {
            in-cert = false;
            certs = [];
            cert-lines = [];
          }
          lines;
        in
          certs-acc.certs
        );

        # Lifted from:
        # https://gist.github.com/manveru/74eb41d850bc146b7e78c4cb059507e2
        toBase64 = text: let
          lib = final.lib;
          inherit (lib) sublist mod stringToCharacters concatMapStrings;
          inherit (lib.strings) charToInt;
          inherit (builtins) substring foldl' genList elemAt length concatStringsSep stringLength;
          lookup = stringToCharacters "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
          sliceN = size: list: n: sublist (n * size) size list;
          pows = [(64 * 64 * 64) (64 * 64) 64 1];
          intSextets = i: map (j: mod (i / j) 64) pows;
          compose = f: g: x: f (g x);
          intToChar = elemAt lookup;
          convertTripletInt = sliceInt: concatMapStrings intToChar (intSextets sliceInt);
          sliceToInt = foldl' (acc: val: acc * 256 + val) 0;
          convertTriplet = compose convertTripletInt sliceToInt;
          join = concatStringsSep "";
          convertLastSlice = slice: let
            len = length slice;
          in
            if len == 1
            then (substring 0 2 (convertTripletInt ((sliceToInt slice) * 256 * 256))) + "=="
            else if len == 2
            then (substring 0 3 (convertTripletInt ((sliceToInt slice) * 256))) + "="
            else "";
          len = stringLength text;
          nFullSlices = len / 3;
          bytes = map charToInt (stringToCharacters text);
          tripletAt = sliceN 3 bytes;
          head = genList (compose convertTriplet tripletAt) nFullSlices;
          tail = convertLastSlice (tripletAt nFullSlices);
        in
          join (head ++ [tail]);
      };


    };
  })];
}
