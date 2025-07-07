################################################################################
# A series of helpers for controlling secrets via a YAML file.
# Keep a list here of systems that use this.  For now I have:
# 1. Home Assistant.
# The format is simply a flat series of key value pairs.
################################################################################
{ lib, ... }: {
  # Generate a series of key-value pairs in a YAML file.  The fields we take
  # from the names of the secret.  The values are the secrets themselves.  In
  # this way we can utilize generators from agenix-rekey to generate individual
  # secrets.
  #
  # Normally we'd use something like builtins.toJSON or builtins.toYAML (which
  # is just an alias for builtins.toJSON).  However the document needs to
  # contain actual secrets, but must be emitted by a shells script.  The
  # many-quotes problem of JSON coupled with a lack of quick escaping for raw
  # JSON values makes this tricky, so we're just going to emit YAML line by line
  # instead.  The file is expected to be flat keys and values anyways.
  #
  # I haven't done any work on escaping the value yet.  That is left for another
  # day.
  age.generators.yaml-secret-file = {
    decrypt,
    deps,
    file,
    name,
    pkgs,
    secret,
    ...
  }:
    lib.strings.concatLines
      (builtins.map
        (dep: ''
          echo '${dep.name}: '"$(${decrypt} ${lib.escapeShellArg dep.file})"
        '')
        deps
      )
  ;
}
