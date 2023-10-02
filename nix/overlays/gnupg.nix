final: prev: {
  gnupg = (prev.gnupg or {}) // {
    agent = {
      pinentryFlavor = "emacs";
    };
  };
}
