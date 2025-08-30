################################################################################
#
################################################################################
{ config, facts, flake-inputs, lib, ... }: let
  inherit (lib) optionalAttrs pipe;
  inherit (lib.attrsets) filterAttrs mapAttrs mapAttrsToList mapAttrs';
  inherit (lib.generators) toYAML;
  inherit (lib.strings) replaceStrings;
  # A helper to create the fancy !Find references in YAML documents.
  Find = (model: key: value: { __ref__ = [ model [key value] ]; });
  snake-case = s: replaceStrings ["-"] ["_"] s;
  service-user-password-env-var = name: "authentik_${snake-case name}_password";
  service-user-secret-password = name: "authentik-${name}-password";
  service-users = filterAttrs
    (user: data: data.type == "service")
    facts.network.users
  ;
  oidc-client-secret-name = name: "authentik-${name}-client-secret";
  oidc-client-env-var = name: "authentik_${snake-case name}_client_secret";
  oidc-clients = filterAttrs
    (user: data: data.authentication == "oidc")
    facts.network.services
  ;

  blueprints = {

    # The akadmin user is a requirement for Authentik, but we don't have to use
    # it or have it enabled.  Its presence makes me suspicious, but I've been
    # assured that it only exists in a vestigial sense when users enter the
    # enlightened era of declarative configuration.
    "00-disable-akadmin" = {
      version = 1;
      metadata = {
        name = "disable-akadmin";
      };
      entries = [
        {
          model = "authentik_core.user";
          id = "akadmin_disable";
          identifiers = {
            username = "akadmin";
          };
          state = "present";
          attrs = {
            is_active = false;
            is_superuser = false;
          };
        }
      ];
    };

    "10-groups" = {
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

    "20-users.yaml" = {
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
            password = "!Env ${service-user-password-env-var name}";
          });
        })
        facts.network.users
        ;
    };

    "30-membership" = {
      version = 1;
      entries = pipe facts.network.groups [
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
            user__username = membership;
            group__name = "app-users";
          };
        }))
      ];
    };

    # "50-application" = {
    #   version = 1;
    #   entries = [
    #     # Application linked to provider
    #     {
    #       model = "authentik_core.application";
    #       state = "present";
    #       identifiers.slug = "openhab";
    #       attributes = {
    #         name = "OpenHAB";
    #         slug = "openhab";
    #         # Link to the provider created above:
    #         provider = Find
    #           "authentik_providers_oidc.oidcprovider"
    #           "name"
    #           "demo-oidc"
    #         ;
    #         open_in_new_tab = false;
    #         meta_icon = "";  # optional icon URL
    #       };
    #     }
    #   ];
    # };

    # Why are we using YAML templates here instead of serializing Nix
    # expressions?  Sit down for a moment, and let us tell you a tale of sadness
    # and treachery.  Authentik makes use of a YAML feature called "tags".  This
    # is a special form of a string / scalar that YAML uses to create references
    # to itself in the document.  Some parses know how to understand this
    # instead of treating the entire document as inert data.  Such YAML
    # documents cannot round trip through serializers.  They require this
    # because their structures want to refer to primary, auto incremented keys
    # in a database.  So to keep those auto generated keys (which aren't
    # remotely portable) out of the config files, they chose to rely on tags
    # (instead of just making the slug or id unique and referencing that for
    # some reason...).  Is Authentik to blame for using such a criminal feature,
    # or is YAML for introducing such a feature?  An exercise for the reader as
    # their tear their hair out trying to serialize a Nix expression into the
    # YAML that Authentik wants to see.  This is why we use a YAML template.

    "40-provider".text = ''
      version: 1
      entries:
      ${lib.concatStringsSep "\n" (lib.mapAttrsToList (name: data: ''
        - model: authentik_providers_oidc.oidcprovider
          id: prov_${name}
          state: present
          attrs:
            name: "${name}-oidc"
            client_type: confidential
            client_id: "${name}-client-id"
            client_secret: !Env ${oidc-client-env-var name}
            redirect_uris:${
              lib.concatStringsSep
                ""
                # Thanks YAML...
                (map (u: "\n        - ${u}") data.redirectUris)
            }
            response_types: [code]
            subject_mode: pairwise
            access_token_validity: 3600
            refresh_token_validity: 1209600
            scopes: [openid, email, profile, offline_access]
            # Built-in consent/authorization flow
            authorization_flow: !Find
            - authentik_flows.flow
            - [slug, default-provider-authorization-implicit-consent]
            # Provider's server-side signing keypair (tokens RS256 by default)
            signing_key: !Find
            - authentik_crypto.certificatekeypair
            - [name, "ak-default"]
        - model: authentik_core.application
          id: app_${name}
          state: present
          attrs:
            name: "${name}"
            slug: "${name}"
            # Link the app to the OIDC provider above
            protocol_provider: !KeyOf prov_${name}
        - model: authentik_policies.policybinding
          state: present
          identifiers:
            target: !Find
            - authentik_core.application
            - [slug, ${name}]]
            order: 0
            policy: !Find
            - authentik_policies_expression.expressionpolicy
            - [name, allow-${name}-users]]
        # Expression policy: allow if user in "app-users".
        # TODO: Develop a model for "apps" so we can create app+group
        # associations and then build this policy list automatically.
        - model: authentik_policies_expression.expressionpolicy
          state: present
          identifiers:
            name: allow-${name}-users
          attributes:
            name: allow-${name}-users
            expression: |
              return (
                user is not None and "openhab-users"
                in [g.name for g in user.groups.all()]
              )
      '') facts.network.services)}
      '';

    "90-password-reset-on-empty-password".text = ''
      version: 1
      metadata:
        name: authn-with-initial-password-enrollment
      entries:
        # Flow
        - model: authentik_flows.flow
          id: flow_authn
          identifiers:
            slug: default-authentication
          attrs:
            designation: authentication
            name: Default AuthN

        # Stages
        - model: authentik_stages_identification.identificationstage
          id: stg_ident
          attrs:
            name: Identify
            user_fields: [username, email]

        - model: authentik_stages_password.passwordstage
          id: stg_password
          attrs:
            name: Password
            backends: [default]

        - model: authentik_stages_password.passwordchangestage
          id: stg_password_change
          attrs:
            name: Set/Change Password

        # Stage bindings (order matters)
        - model: authentik_flows.stagebinding
          id: bind_ident
          attrs:
            target: !Find [authentik_flows.flow, [slug, default-authentication]]
            stage:  !Find
            - authentik_stages_identification.identificationstage
            - [name, Identify]
            order: 10

        - model: authentik_flows.stagebinding
          id: bind_password
          attrs:
            target: !Find
            - authentik_flows.flow
            - [slug, default-authentication]
            stage:  !Find
            - authentik_stages_password.passwordstage
            - [name, Password]
            order: 20

        - model: authentik_flows.stagebinding
          id: bind_password_change
          attrs:
            target: !Find [authentik_flows.flow, [slug, default-authentication]]
            stage:  !Find
            - authentik_stages_password.passwordchangestage
            - [name, "Set/Change Password"]
            order: 25

        # Expression policy: include pw-change stage only if no usable password
        # exists.
        - model: authentik_policies_expression.expressionpolicy
          id: pol_needs_pw
          attrs:
            name: Needs password enrollment
            expression: |
              # Include the stage if the user has no usable password
              return request.user.has_usable_password is False

        # Bind the policy to the *password-change* stage binding.
        - model: authentik_policies.policybinding
          id: bind_pol_to_pw_change
          attrs:
            target: !Find
            - authentik_flows.stagebinding
            - [id, bind_password_change]
            policy: !Find
            - authentik_policies_expression.expressionpolicy
            - [name, "Needs password enrollment"]
            order: 1
      '';

  };

  environment-file-service = {
    enable = true;
    secrets = (mapAttrs'
      (name: data: {
        name = service-user-secret-password name;
        value.environmentVariable = service-user-password-env-var name;
      })
      service-users
    ) // (mapAttrs'
      (name: data: {
        name = oidc-client-secret-name name;
        value.environmentVariable = oidc-client-env-var name;
      })
      oidc-clients
    ) // {
      authentik-secret-key.environmentVariable = "AUTHENTIK_SECRET_KEY";
    };
  };

in {
  imports = [
    flake-inputs.authentik-nix.nixosModules.default
  ];

  age.secrets = (mapAttrs'
    (name: data: {
      name = service-user-secret-password name;
      value = {
        generator.script = "long-passphrase";
        rekeyFile = ../secrets/authentik-${name}-password.age;
      };
    })
    service-users
  ) // (mapAttrs'
    (name: data: {
      name = oidc-client-secret-name name;
      value = {
        generator.script = "hex";
        settings.length = 60;
      };
    })
    oidc-clients
  ) // {
    authentik-secret-key = {
      generator.script = "hex";
      settings.length = 60;
    };
  };

  services.environment-file-secrets.services.authentik = environment-file-service;
  services.environment-file-secrets.services.authentik-migrate = environment-file-service;
  services.environment-file-secrets.services.authentik-worker = environment-file-service;

  services.https.fqdns."authentik.proton" = {
    enable = true;
    # It doesn't look like they expose the port config field yet.
    internalPort = config.services.authentik.port or 9000;
  };
  services.authentik = {
    enable = true;
    # environmentFile = config.age.secrets.authentik-environment-file.path;
    # We have this exposed as "secrets.env" and loaded via LoadCredential, but
    # the authentik-flake project has some special stuff wired in where this
    # expect this file, they expect it to be owned by root, and there are
    # multiple systemd services stood up that all share it.  So there's a lot of
    # plumbing and instead of fighting it, we'll just give it that file we
    # generated from our environment-file-secrets.nix module.
    environmentFile = "/run/agenix/authentik-environment-file";
    # postgresql = {
    #   createLocally = true;
    #   host = "/run/postgresql";
    # };
  };
  environment.etc = lib.attrsets.mapAttrs' (name: blueprint: {
    name = "authentik/blueprints/default/${name}.yaml";
    value.text = toYAML {} blueprint;
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
