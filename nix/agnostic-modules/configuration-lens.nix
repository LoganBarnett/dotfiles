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
#
# This module is agnostic and works in both system (NixOS/Darwin) and
# home-manager contexts.
################################################################################
{ config, lib, pkgs, options, ... }: let
  inherit (lib.strings) concatLines;
  # Detect whether we're in home-manager or system context.
  isHomeManager = options ? home;

  yqLensInvocation = name: opt: ''
    echo "Running yq lens: ${name}"
    # Ensure parent directory exists.
    mkdir -p "$(dirname ${lib.escapeShellArg opt.filePath})"
    # Create file if it doesn't exist.
    if [ ! -f ${lib.escapeShellArg opt.filePath} ]; then
      echo "{}" > ${lib.escapeShellArg opt.filePath}
    fi
    # Set the value using yq.
    if ! ${pkgs.yq-go}/bin/yq eval --inplace \
      ${lib.escapeShellArg "${opt.documentPath} = ${opt.value}"} \
      ${lib.escapeShellArg opt.filePath}; then
      echo "ERROR: yq lens '${name}' failed"
      exit 1
    fi
  '';

  augeasLensInvocation = name: opt: let
    # Escape spaces in file path for augeas.
    escapedPath = lib.escape [" "] opt.filePath;
    script = pkgs.writeText "configuration-lens-${name}.aug" ''
      set /files${escapedPath}${opt.documentPath} ${opt.value}
    '';
    augeasCmd =
      "${pkgs.augeas}/bin/augtool" +
      " --noload" +
      " --noautoload" +
      " --include ${pkgs.augeas}/share/augeas/lenses/dist" +
      " --transform '${opt.transform} incl ${escapedPath}'";
  in ''
    echo "Running augeas lens: ${name}"
    if ! ${augeasCmd} --autosave --file ${script}; then
      echo "ERROR: Augeas lens '${name}' failed, checking for errors..."
      echo "Original command: ${augeasCmd}"
      ${augeasCmd} <<EOF
errors
quit
EOF
      exit 1
    fi
  '';

  lensInvocation = name: opt: (
    # Detect YAML and use yq instead of augeas.
    if opt.transform != null && lib.hasInfix "yaml" opt.transform then
      yqLensInvocation name opt
    else if opt.lensType == "augeas" then
      augeasLensInvocation name opt
    else
      null
  );

  activationScript = concatLines (
    lib.attrsets.mapAttrsToList
      lensInvocation
      config.fileLenses
  );
in {
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
          type = lib.types.nullOr lib.types.str;
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

  config = if isHomeManager then {
    # Home-manager activation.
    home.activation.fileLenses =
      lib.hm.dag.entryAfter
        ["writeBoundary"]
        activationScript;
  } else {
    # System activation (NixOS/Darwin).
    system.activationScripts.fileLenses.text = activationScript;
  };
}
