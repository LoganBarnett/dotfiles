self: super:
{
  freecad = super.callPackage ./freecad.nix {
    inherit self;
    # perl = self.perl532;
  };
}
