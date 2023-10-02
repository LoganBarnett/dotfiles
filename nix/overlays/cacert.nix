final: prev: {
  cacert = (prev.cacert or {}) // {
    extraCertificatesFiles = [ ../../../dotfiles-private/new-e-ah-certs.pem ];
  };
}
