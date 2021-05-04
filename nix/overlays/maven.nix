self: super: {
  maven = (super.maven or {}) // {
    jdk = super.jdk;
  };
}
