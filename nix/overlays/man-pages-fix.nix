(final: prev: {
  man-pages = prev.man-pages.overrideAttrs (old: rec {
    version = "unstable-2024-05-08";
    # buildInputs = [ prev.locale ];
    src = builtins.fetchGit {
      # url = "mirror://kernel/linux/docs/man-pages/${old.pname}-${version}.tar.xz";
      url = "git://git.kernel.org/pub/scm/docs/man-pages/man-pages.git";
      rev = "db038e4c4626ca2d1ac8b5ded82152762c9832e8";
      # sha256 = "000000000000000000000000000000000000000000000000000";
    };
  });
})
