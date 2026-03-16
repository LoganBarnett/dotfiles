final: prev: {
  maven = (prev.maven or {}) // {
    jdk = prev.jdk;
  };
}
