self: super: {
  gnupg = (super.gnupg or {}) // {
    agent = {
      pinentryFlavor = "emacs";
    };
  };
}
