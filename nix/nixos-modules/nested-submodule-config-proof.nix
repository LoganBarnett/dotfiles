################################################################################
# In getting with an argument with ChatGPT (5) it has insisted that submodules
# can provide both an options and config keys, and when doing so, the config
# value is spread as the module config appropriately.  I have not seen this in
# the wild, and the documentation doesn't back it.  There are stale copies of
# the NixOS manual floating around that _suggest_ this is the case, but nothing
# confirms it.  While such behavior has a nice consistency to it, it doesn't
# actually work that way today.  This module proves that.
#
# To invoke the test, include this module and then use:
# llm-arguments.proof.submodule-configs.items = [
#   { foo = "bar"; }
# ];
# Apply your configuration and then, on the host in question, `cat
# /etc/proof-bar`.
################################################################################
{ lib, config, ... }:
let
  inherit (lib) mkOption types;
in {
  options.llm-arguments.submodule-configs.items = mkOption {
    type = types.attrsOf (types.submodule ({ name, config, lib, ... }: {
      options = {
        foo = mkOption {
          type = types.str;
          default = "unset";
          description = "Value to prove submodule config access.";
        };
      };

      # PROOF: using `config.foo` inside the submodule’s own `config`
      config = {
        environment.etc."proof-${name}".text = "foo=${config.foo}\n";
      };
    }));
    default = {};
    description = "Attrset of proof items (submodules).";
  };
}
