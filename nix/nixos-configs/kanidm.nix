################################################################################
# An untested KanIDM (Kanidm?).
#
# Kanidm showed much promise to me in the way of a declarative IDP/SSO system,
# but as more details emerged it didn't work for me for a few reasons.
# 1. Kanidm has a hard delineation between users and service accounts, which
#    kind of makes sense when you understand how OIDC works.  I vehemently
#    oppose this stance, because it means you must have "apps" that are blessed
#    by an administrator or someone of sufficient access.   I believe it is a
#    fundamental right to automate.
# 2. Still on the apps part, but another aspect to it:  API tokens.  API tokens
#    in Kanidm are _not_ declaratively managed.  This means if you have critical
#    infrastructure and you're going nuclear across one or more hosts, you are
#    resorting to a Kanidm playbook where you hand resuscitate your API tokens
#    _like cavemen did in ages past_.  Until then nothing works.  It also means
#    you can't have agenix truly manage the API tokens.  You have to ask Kanidm
#    nicely to make one for you on the _working_ Kanidm host.  I hope those of
#    you NixOS lovers out there understand why this is a no go, but for the
#    uninitiated this means you can't just come back up to working with nothing
#    by a valid NixOS configuration, a willing NixOS host, and dreams of working
#    infrastructure.
#
# This configuration is untested but I'm going to keep a record of it somewhere
# just in case I need to come back to it, or I start thinking maybe it'll be
# good to revisit.
################################################################################
{ config, lib, pkgs, facts, ... }:

let
  inherit (lib)
    attrNames concatMap filterAttrs getAttrFromPath hasAttr mapAttrs mapAttrs' mkIf
    optional optionals pipe unique;

  F = facts.network;

  # Persons only (ignore service + oidc-client “users” for Kanidm persons).
  persons =
    filterAttrs (_: u: (u.type or "person") == "person") F.users;

  # If group members include service accounts, drop them for now (module can’t create them).
  personNames = attrNames persons;

  # Group -> attr with description; Kanidm membership is driven from persons’ “groups” field.
  groups =
    mapAttrs (_g: g: {
      present = true;
      description = g.description or "";
    }) F.groups;

  # Compute groups per person based on facts.groups.*.members
  groupsByPerson =
    let
      pairs = concatMap
        (g: let mems = (F.groups.${g}.members or []); in
             map (m: { inherit g m; }) mems)
        (attrNames F.groups);
      byPerson = lib.groupBy (p: p.m) pairs;
    in
      mapAttrs (_m: lst: unique (map (x: x.g) lst)) byPerson;

  # Persons payload for Kanidm:
  provisionPersons =
    mapAttrs (name: u: {
      displayName   = u."full-name" or name;
      legalName     = u."full-name" or null;
      mailAddresses = optional (u ? email) u.email;
      # Only include groups that exist, and only if this member is a person.
      groups        = optionals (hasAttr name groupsByPerson)
                       (groupsByPerson.${name});
    }) persons;

  # Optional OpenHAB OIDC client from your `facts.network.services.openhab`.
  haveOpenhab =
    hasAttr "services" F
    && hasAttr "openhab" F.services
    && (F.services.openhab.authentication or "") == "oidc";

  openhabClientName = "openhab";
  openhabDisplay    = "OpenHAB";
  openhabRedirects  = F.services.openhab.redirectUris or [];

in
{
  #### Kanidm server (basic, ready for reverse proxy / your TLS helper)
  services.kanidm = {
    enableServer = true;
    enableClient = true;

    clientSettings.uri = config.services.kanidm.serverSettings.origin;

    serverSettings = {
      domain             = F.domain;                        # e.g. "proton"
      origin             = "https://sso.${F.domain}";       # behind nginx via your https module
      trust_x_forward_for = true;
      bindaddress        = "127.0.0.1:8443";                # reverse-proxied
      # If you prefer Kanidm’s own TLS instead of nginx termination, set tls_* paths here.
    };

    #### Declarative provisioning: persons, groups, oauth2 systems
    provision = {
      enable = true;
      autoRemove = true;
      instanceUrl = config.services.kanidm.serverSettings.origin;

      # File-based secrets (supported by the module; verified).
      adminPasswordFile    = config.age.secrets.kanidm-admin-password.path;
      idmAdminPasswordFile = config.age.secrets.kanidm-idm-admin-password.path;

      groups  = groups;
      persons = provisionPersons;

      systems.oauth2 = mkIf haveOpenhab {
        "${openhabClientName}" = {
          displayName = openhabDisplay;
          # Kanidm’s option name is `originUrl`; it accepts a string or list for redirects.
          # We pass your facts’ redirectUris straight through.
          originUrl   = openhabRedirects;
          # You can add claim maps or scope mappings later if you want group-based gating.
          # claimMaps."<custom>".valuesByGroup."<group>" = [ "email" "openid" "profile" "groups" ];
        };
      };
    };
  };

  age.secrets = {
    kanidm-admin-password = {
      generator.script = "long-passphrase";
    };
    kanidm-idm-admin-password = {
      generator.script = "long-passphrase";
    };
  };

  services.https.fqdns."sso.${F.domain}" = {
    enable = true;
    internalPort = config.services.kanidm.port or 8443;
  };
}
