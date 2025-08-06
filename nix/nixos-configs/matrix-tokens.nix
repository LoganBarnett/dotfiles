################################################################################
# Defines a series of tokens used by Matrix and non-service-account systems that
# need to speak directly to Matrix, sans the typical LDAP authentication used.
# This is typical for bridges.
#
# In Matrix (or Synapse) vernacular, there is the notion of an "appservice" and
# "homeserver".
#
# The appservice is the bridge service.  The homeserver is the Matrix service
# itself.
#
# The Synapse service will need to provide the appservice token when
# communicating with the appservice.
#
# The bridge service will need to provide the homeserver token when
# communicating with the appservice.
################################################################################
{ ... }: {
  age.secrets.mautrix-signal-bridge-appservice-token = {
    generator = {
      script = "hex";
      settings.length = 32;
    };
    rekeyFile = ../secrets/mautrix-signal-bridge-appservice-token.age;
  };
  age.secrets.mautrix-signal-bridge-homeserver-token = {
    generator = {
      script = "hex";
      settings.length = 32;
    };
    rekeyFile = ../secrets/mautrix-signal-bridge-homeserver-token.age;
  };
}
