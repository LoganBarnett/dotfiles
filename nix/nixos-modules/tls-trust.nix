################################################################################
# We should trust our own certificate chain!
################################################################################
{ pkgs, ... }: {

  security.pki.certificateFiles = [
    "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
    ../secrets/proton-ca.crt
  ];

}
