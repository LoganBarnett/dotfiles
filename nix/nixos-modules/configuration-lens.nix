################################################################################
# Use functional lenses for managing configuration.
#
# Nix can easily emit entire configuration files but sometimes Nix cannot
# entirely own a particular file and so putting the file into the Nix store is
# both impractical and the individual settings are not very meaningful.
#
# To work around this, we can have lenses (a lens is a functional programming
# term) that ensure a particular value is present in a particular location.  The
# augeas tool is our primary candidate for handling this, but conceivably we
# could use any other tool we wanted for the job.
################################################################################
{ config, lib, pkgs, ... }: {
  options.fileLenses = lib.mkOption (let
    lensTypes = [ "augeas" ];
    augeasLensType = lib.types.submodule {
      options = {
        filePath = lib.mkOption {
          # We could enum this.  There is a finite list, but it is rather
          # large (see `man augtool` under the COMMANDS section).
          type = lib.types.nonEmptyStr;
        };
        operation = lib.mkOption {
          type = lib.types.enum [
            "set"
          ];
        };
        documentPath = lib.mkOption {
          # We could enum this.  There is a finite list, but it is rather
          # large (see `man augtool` under the COMMANDS section).
          type = lib.types.nonEmptyStr;
        };
        value = lib.mkOption {
          type = lib.types.nullOr lib.types.nonEmptyStr;
        };
        transform = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
        };
        lensType = lib.mkOption {
          type = lib.types.enum lensTypes;
          internal = true;
          default = "augeas";
        };
      };
    };
  in {
    type = lib.types.attrsOf augeasLensType;
    default = {};
    example = "";
    description = "";
  });
  config.system = let
    join = (sep:
      (xs: lib.strings.concatStrings (lib.strings.intersperse sep xs))
    );
    join-lines = join "\n";
    augeasLensInvocation = opt: (''
      stat ${lib.escapeShellArg opt.filePath}
      ${pkgs.augeas}/bin/augtool \
        --autosave \
        ${if opt.transform == null then
          ""
        else
          "--transform ${lib.escapeShellArg opt.transform} incl ${lib.escapeShellArg opt.filePath}"
        } \
        ${lib.escapeShellArg opt.operation} \
        /files/${lib.escapeShellArg opt.filePath}${lib.escapeShellArg opt.documentPath} \
        ${lib.escapeShellArg opt.value}
    '');
    lensInvocation = opt: (
      # We expect there to be more lens transformation tools available.
      (if opt.lensType == "augeas" then
        augeasLensInvocation
        else
          null
      )
      opt
    );
  in {
    activationScripts.fileLenses.text = join-lines (builtins.map
      lensInvocation
      (lib.attrsets.attrValues config.fileLenses)
    );
  };
}
