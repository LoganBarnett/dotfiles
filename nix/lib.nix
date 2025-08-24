let
  toConstantCase = lib: s:
    let
      isLower = c: lib.strings.match "[a-z]" c != null;
      isUpper = c: lib.strings.match "[A-Z]" c != null;
      isDigit = c: lib.strings.match "[0-9]" c != null;
      s1 = lib.strings.replaceStrings [ "-" ] [ "_" ] s;
      # Insert underscores between lower→upper boundaries: fooBar →
      # foo_Bar.
      s2 = builtins.replaceStrings
        # regex not available in nix, so use string iteration trick:
        # Instead, split on capital letters.
        # We'll implement below via manual scan.
        [] [] s1;
      # A helper to split camel/PascalCase into words
      splitCamel = str:
        let
          chars = lib.stringToCharacters str;
          step = { acc, word, }:
            c:
            if isLower c || isDigit c then
              { acc = acc; word = word + c; }
            else
              # Uppercase boundary.
              { acc = acc ++ [word]; word = c; };
          final = builtins.foldl' step { acc = []; word = ""; } chars;
        in final.acc ++ [ final.word ];
      words = builtins.filter (w: w != "") (splitCamel s2);
    in
      lib.strings.toUpper (lib.concatStringsSep "_" words);


in {

  inherit toConstantCase;

  ##
  # Creating an environment file is kind of a pain, so include all of the
  # machinery required to create one.  It comes up quite a bit.
  # This function will:
  # 1. Aggregate all provided secrets into a secret environment file.
  # 2. Provide that environment file to the service using LoadCredential.
  # This means no fussing with permissions/groups, and works with
  # DynamicUser=true in your unit file.
  # 3. Ensures that the agenix service starts before your service does, so
  # the secrets needed are actually available.
  #
  # Just make sure to use lib.mkMerge with this results of this function
  # and your other settings for your service.
  # TODO: Document/clean up arguments.  The dependency injection must take place
  # here, unfortunately.  The order could be exchanged though.  It might be
  # better that way.
  environment-file-for-service = { lib, config, pkgs, ... }:
    { secrets, service, ... }: {
      imports = [

        # Breaking these up into submodules helps prevent infinite recursion
        # when evaluating `config`.
        {
          age.secrets = (
            # Vanilla secrets.
            lib.attrsets.mapAttrs (name: value: value)
              secrets
          );
        }

        {
          age.secrets = (
            # Environment variables.
            lib.attrsets.mapAttrs' (name: value: {
              name = "${name}-environment-variable";
              value = {
                generator = {
                  script = "environment-variable";
                  dependencies = [
                    config.age.secrets.${name}
                  ];
                };
                settings.field = toConstantCase lib name;
              };
            })
              secrets
          );
        }

        {
          age.secrets."${service}-environment-file" = {
            # The environment file itself.
            generator = {
              script = "environment-file";
              dependencies = lib.attrsets.mapAttrsToList
                (name: value: config.age.secrets."${name}-environment-variable")
                secrets
              ;
            };
          };
        }

        {
          services.${service} = {
            # TODO: Only optionally set this?  It's not always available.
            # It likely isn't even necessary, but other NixOS modules may
            # trigger important logic on its presence.
            # environmentFile = config
            #   .age
            #   .secrets
            #   ."${service}-environment-file"
            #   .path
            # ;
          };
          systemd.services.${service} = let
            after = [ "run-agenix.d.mount" ];
          in {
            inherit after;
            requires = after;
            serviceConfig = {
              ImportCredential = [
                "secrets.env"
              ];
              LoadCredential = [
                "secrets.env:${
                  config
                    .age
                    .secrets
                    ."${service}-environment-file"
                    .path
                }"
              ];
            };
          };
        }
      ];
    };

}
