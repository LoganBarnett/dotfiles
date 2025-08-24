################################################################################
#
################################################################################
{ config, facts, lib, ... }: let
  inherit (lib) optionalAttrs pipe;
  inherit (lib.attrsets) mapAttrsToList;
  # A helper to create the fancy !Find references in YAML documents.
  Find = (model: key: value: { __ref__ = [ model [key value] ]; });
  blueprints = {
    "00-groups" = {
      version = 1;
      entries = mapAttrsToList
        (name: data: {
          model = "authentik_core.group";
          state = "present";
          identifiers.name = name;
          attributes.name = name;
        })
        facts.network.groups
        ;
      # entries = [
      #   # App access group, where "app" is some arbitrary application.
      #   {
      #     model = "authentik_core.group";
      #     state = "present";
      #     identifiers.name = "app-users";
      #     attributes.name = "app-users";
      #   }
      # ];
    };
    "10-users.yaml" = {
      version = 1;
      entries = mapAttrsToList
        (name: data: {
          model = "authentik_core.user";
          state = "present";
          identifiers.username = name;
          attributes = {
            username = name;
            name = data.full-name;
            email = data.email;
            is_active = true;
            is_superuser = false;
          } // (optionalAttrs (data.type == "service") {
            # TODO: Look up password format for env lookups.
            password = "!Env ${"foo"}";
          });
        })
        facts.network.users
        ;
    };


    "20-membership" = {
      version = 1;
      entries = pipe facts.network.users [
        (mapAttrsToList
          (name: data:
            builtins.map (member: {
              group = name;
              inherit member;
            })
              data.members
          )
        )
        (builtins.map (membership: {
          model = "authentik_core.membership";
          state = "present";
          identifiers = {
            user__username = "alice";
            group__name = "app-users";
          };
        }))
      ];
    };

    "30-provider" = {
      version = 1;
      entries = [
        # Not currently required.
        # {
        #   model = "authentik_providers_oidc.oidcprovider";
        #   state = "present";
        #   identifiers = { name = "demo-oidc"; };
        #   attributes = {
        #     name = "demo-oidc";
        #     # Public client_id; secret via env below
        #     client_id = "demo-client-id";
        #     client_secret = "!Env AK_DEMO_CLIENT_SECRET";
        #     # Update to your app’s URLs:
        #     redirect_uris = [
        #       "https://app.example.proton/oauth/callback"
        #       "https://app.example.proton/auth/callback"
        #     ];
        #     # Tweak as needed:
        #     response_types = [ "code" ];
        #     signing_key = null;         # auto-generate/manage server-side
        #     subject_mode = "pairwise";  # or "public"
        #     access_token_validity = 3600;
        #     refresh_token_validity = 1209600;
        #     property_mappings = [];     # default mappings are fine for many apps
        #     scopes = [ "openid" "email" "profile" "offline_access" ];
        #   };
        # }
      ];
    };

    "40-application" = {
      version = 1;
      entries = [
        # Application linked to provider
        {
          model = "authentik_core.application";
          state = "present";
          identifiers.slug = "openhab";
          attributes = {
            name = "OpenHAB";
            slug = "openhab";
            # Link to the provider created above:
            provider = Find
              "authentik_providers_oidc.oidcprovider"
              "name"
              "demo-oidc"
            ;
            open_in_new_tab = false;
            meta_icon = "";  # optional icon URL
          };
        }
      ];
    };

    "50-policy-allow-app-users" = {
      version = 1;
      entries = [
        # Expression policy: allow if user in "app-users"
        # TODO: Develop a model for "apps" so we can create app+group
        # associations and then build this policy list automatically.
        {
          model = "authentik_policies_expression.expressionpolicy";
          state = "present";
          identifiers.name = "allow-openhab-users";
          attributes = {
            name = "allow-openhab-users";
            # Ugh I was given list comprehensions by ChatGPT.
            expression = ''
            # "user" is provided in context
            return user is not None and "openhab-users" in [g.name for g in user.groups.all()]
          '';
          };
        }
      ];
    };

    "60-bind-policy-to-app" = {
      version = 1;
      entries = [
        # Bind the expression policy to the Application (enforcement)
        {
          model = "authentik_policies.policybinding";
          state = "present";
          identifiers = {
            target = Find "authentik_core.application" "slug" "openhab";
            order = 0;
            policy = Find
              "authentik_policies_expression.expressionpolicy"
              "name"
              "allow-openhab-users"
            ;
          };
        }
      ];
    };
  };
in {
  age.secrets = {
    authentik-environment-file = {
      generator = {
        script = "environment-file-aggregate";
        dependencies = [

        ];
      };
    };
  };
  services.https.fqdns."authentik.proton" = {
    enable = true;
    internalPort = config.services.authentik.port;
  };
  services.authentik = {
    enable = true;
    environmentFile = config.age.secrets.authentik-environment-file.path;
    postgresql = {
      createLocally = true;
      host = "/run/postgresql";
    };
  };
  environment.etc = lib.attrsets.mapAttrs' (name: value: {
    "authentik/blueprints/default/${name}.yaml".text = builtins.toYAML {

    };
  }) blueprints;
  # Bind-mount the /etc path into the path Authentik expects (/blueprints).
  # Worker is the component that reads/applies blueprints.
  systemd.services.authentik-worker.serviceConfig.BindReadOnlyPaths = [
    "/etc/authentik/blueprints:/blueprints"
  ];
  # Also bind for server (usually not needed, but harmless).
  systemd.services.authentik-server.serviceConfig.BindReadOnlyPaths = [
    "/etc/authentik/blueprints:/blueprints"
  ];
}
