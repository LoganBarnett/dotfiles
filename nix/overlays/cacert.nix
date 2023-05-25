self: super: {
  cacert = (super.cacert or {}) // {
    extraCertificatesFiles = [ ../../../dotfiles-private/new-e-ah-certs.pem ];
  };
}
