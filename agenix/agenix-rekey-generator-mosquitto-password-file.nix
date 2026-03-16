{ pkgs, lib, ... }: {
  age.generators.mosquitto-password-file = {
    decrypt,
    deps,
    file,
    name,
    pkgs,
    secret,
    ...
  }:
    lib.traceVal (lib.strings.concatLines
      (builtins.map
        # Use `${pkgs.mqttpassworder}/bin/mqttpassworder` when we figure out how
        # to properly integrate overlays with `agenix-rekey generate`.
        (dep: let
          name = lib.escapeShellArg (dep.settings.username or dep.name);
        in ''
          mqttpassworder \
            -creds "${name}:$(${decrypt} ${lib.escapeShellArg dep.file})"
        '')
        deps
      )
    );
  # Unfortunately there doesn't seem to be a way to pass overlay information to
  # `agenix-rekey generate`.  Instead we can just add this to our PATH.
  environment.systemPackages = [
    pkgs.mqttpassworder
  ];
}
