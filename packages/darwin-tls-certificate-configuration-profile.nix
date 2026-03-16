################################################################################
# A useless set of code.
#
# Attempting to install a profile via `profile install` will result in this
# wonderful message:
# profiles tool no longer supports installs.  Use System Settings Profiles to
# add configuration profiles.
#
# This means you can produce a configuration profile / mobileconfig all you
# like, but you can't do anything with it unattended / automatically.
#
# I believe configuration profiles can be used via an MDM.  So maybe there's
# hope that this can find use there.
################################################################################
{ certs, lib, libossp_uuid, name, pkgs, writeTextFile, ... }: let
  # This should be part of nixpkgs lib.  Setting it here should make it easy to
  # swap out later.
  toBase64 = pkgs.lib.custom.toBase64;
in writeTextFile {
  name = "darwin-tls-certificate-configuration-profile.mobileconfig";
  text = let
    # This is generated beforehand, and just needs to be an arbitrary UUID.
    namespace-uuid = "f4786efa-2076-11f0-a4c4-c70c30625675";
    uuid = name: ''
      ${libossp_uuid}/bin/uuid --version 3 ${namespace-uuid} ${name}
    '';
    xmlEscape = x:
      if builtins.isString x
      then builtins.replaceStrings
        ["&"   "<"   ">"   "\""  "'"]
        ["&amp;" "&lt;" "&gt;" "&quot;" "&apos;"]
        x
      else toString x
    ;
    # TODO: Support all type conversions.
    plist-data-type = value:
      if builtins.isString value then "string"
        else if builtins.isInt value then "integer"
        else if builtins.isList value then "list"
        else if builtins.isAttrs value then
          (if value ? __as_data__ then "data" else "dict")
          else "unknown-type"
    ;
    list-to-array = value: ''
      ${builtins.concatStringsSep "\n" (builtins.map plist-data-value value)}
    '';
    plist-data-value = value: let
      tagged = value: let
        type = plist-data-type value;
      in ''
        <${type}>${value}</${type}>
      '';
    in
      tagged (
        # Each one of these should carefully choose if it should be escaped.
        # Nesting and escaping don't go well together.
        if builtins.isAttrs value
        then (if value ? __as_data__
              then (xmlEscape value.__as_data__)
              else attrs-to-dict value)
        else if builtins.isList value then
          (list-to-array value)
        else xmlEscape value
      )
    ;
    attrs-to-dict = attrs: ''
      <dict>
        ${lib.strings.concatLines (lib.attrsets.mapAttrsToList
          (name: value: ''
            <key>${xmlEscape name}</key>
            ${plist-data-value value}
          '')
          attrs
        )}
      </dict>
    '';
    cert-bodies = cert-names-by-cert-data: lib.attrsets.mapAttrs
      (name: pkgs.lib.custom.pemToCertificates)
      cert-names-by-cert-data
    ;
    certs-xml = certs: lib.lists.flatten (lib.attrsets.mapAttrsToList
      (name: certs: lib.lists.imap0 (i: cert-data: (let
        indexed-name = "${name}-${toString i}";
      in {
        PayloadCertificateFileName = "${indexed-name}.cer";
        PayloadContent.__as_data__ = cert-data;
        PayloadDescription = ''
          Installs ${xmlEscape indexed-name} as a trusted root TLS certificate.
        '';
        PayloadIdentifier = "org.nixos.profile.certs.${indexed-name}";
        PayloadType = "com.apple.security.root";
        PayloadUUID = uuid indexed-name;
        PayloadVersion = 1;
      })) certs)
      (cert-bodies (lib.attrsets.mapAttrs (_: builtins.readFile) certs))
    );
    profile = {
      PayloadContent = certs-xml certs;
      PayloadDisplayName = "Root CA Trust Profile";
      PayloadIdentifier = "org.nixos.profile.certs";
      PayloadUUID = uuid "nix-darwin-all-tls-certs";
      PayloadVersion = 1;
      PayloadType = "Configuration";
    };
  in ''
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
    "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
${attrs-to-dict profile}
</plist>
  '';
}
