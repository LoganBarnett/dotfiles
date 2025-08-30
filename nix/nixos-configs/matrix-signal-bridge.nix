################################################################################
# Signal's Desktop client requires a completely new, black box client every two
# weeks or so.  Sounds like an interesting attack vector to me.  If it really is
# "for security" then I claim that critical security fixes every two weeks is
# indicative if an insanely insecure platform.  I suspect the bad kind of
# laziness.
#
# Fortunately, I can just setup a bridge.
#
# Where this stands currently:
#
# From my reading, I want to get a fully configured double_puppet configuration
# going.  This configuration is necessary to make it so that the bridge sends
# messages through Signal as the user which is mapped to Signal, as opposed to
# going through a relay bot (which might look scammy to many users, and also
# just isn't as cool).
#
# In order to complete this, I want some kind of JWT hand-off.  I could use a
# shared secret, but it's not as secure.  Perhaps I could just get the shared
# secret working until I can get more figured out.
#
# Authelia doesn't handle the JWT.  I need a fully provisioned identity provider
# such as Authentik.  At the moment, such identity providers are quite a ways
# away from completion.  Even then, Matrix requires a special JWT format and
# there's a mapping policy that must be configured to make this happen.  Once I
# get it though, it gets the nice "do you want this app to have these
# permissions" kinds of promptings.
#
# There's also some misc. configuration I've yet to address as well.
################################################################################
{ config, ... }: {
  imports = [
    ./matrix-tokens.nix
  ];
  age.secrets.mautrix-signal-appservice-token-environment = {
    generator = {
      script = "environment-variable";
      settings.field = "appservice_token";
      dependencies = [
        config.age.secrets.mautrix-signal-appservice-token
      ];
    };
    rekeyFile = ../secrets/mautrix-signal-homeserver-token-environment.age;
  };
  age.secrets.mautrix-signal-homeserver-token-environment = {
    generator = {
      script = "environment-variable";
      settings.field = "homeserver_token";
      dependencies = [
        config.age.secrets.mautrix-signal-homeserver-token
      ];
    };
    rekeyFile = ../secrets/mautrix-signal-homeserver-token-environment.age;
  };
  age.secrets.mautrix-signal-environment-file-aggregate = {
    generator = {
      script = "environment-file";
      dependencies = [
        config.age.secrets.mautrix-signal-appservice-token-environment
        config.age.secrets.mautrix-signal-homeserver-token-environment
      ];
    };
    rekeyFile =
      ../secrets/mautrix-signal-homeserver-token-environment-aggregate.age;
  };
  services.mautrix-signal = {
    enable = true;
    environmentFile =
      config.age.secrets.mautrix-signal-environment-file-aggregate.path;
    settings = {
      appservice = {
        id = "signal";
        as_token = "$''{appservice_token}";
        bot = {
          displayname = "Signal Bridge Bot";
          username = "signalbot";
        };
        ephemeral_events = false;
      };
      backfill = {
        enable = true;
      };
      bridge = {
        command_prefix = "!signal";
        mute_only_on_create = false;
        permissions = {
          "*" = "relay";
        };
        relay = {
          enabled = true;
        };
        private_chat_portal_meta = true;
      };
      database = {
        type = "postgresql";
        uri = "postgresql:///mautrix_signal?host=/run/postgresql";
      };
      encryption = {
        allow = true;
        default = true;
        pickle_key = "$ENCRYPTION_PICKLE_KEY";
        require = true;
      };
      homeserver = {
        address = "http://localhost:8448";
      };
      logging = {
        min_level = "info";
        writers = [
          {
            format = "pretty-colored";
            time_format = " ";
            type = "stdout";
          }
        ];
      };
      matrix = {
        message_status_events = true;
      };
      network = {
        displayname_template = "{{or .ProfileName .PhoneNumber \"Unknown user\"}}";
      };
      direct_media = {
        server_key = "";
      };
      double_puppet = {
        secrets = { };
        servers = { };
      };
      provisioning = {
        shared_secret = "";
      };
      public_media = {
        signing_key = "";
      };
    };
  };
}
