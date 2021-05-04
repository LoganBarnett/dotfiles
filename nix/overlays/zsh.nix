self: super: {
  zsh = (super.zsh or {}) // {
    enable = true;
    ohMyZsh = {
      enable = true;
      customPkgs = with super.zsh.pkgs; [
        noreallyjustfuckingstopalready
        zsh-git-prompt
      ];
      plugins = [
        "nix"
      ];
    };
  };
}
