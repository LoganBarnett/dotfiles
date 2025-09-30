################################################################################
# Sets up GitHub migration to be ready to use.
#
# Any place you see type-of-source, that's the "slug" for the source system.
# For example, Stash/BitBucket Server/Bitbucket DataCenter is "bbs".
#
# Requires the following secrets via pass:
# 1. ${type-of-source}-ghe-pat - Setting this up is a bit of work, but it's kind
# of "standard" permissions granting in the GitHub regime.
# 2. ${murica} (see var) - this is the password for 'Murica.
# 3. gh-migration-s3-key-id - This is the S3 bucket's access key ID.
# 4. gh-migration-s3-secrey-key - This is the S3 bucket's secret key.
#
# Once this is applied, source your ~/.config/gh/${type-of-source}-env.  This
# will give you a gh-migrate-${type-of-source} function, which takes a project
# name and a repo name.  As an optional third argument, you can pass "true" for
# a debug mode, where it prints the command it runs.
#
# To add a new migration, add a new attrset to the migrations list below, and
# fill out the fields to the best of your ability.  The `type` must be
# ${type-of-source}.
#
# Much of this assumes using the Bitbucket Server migration tool.  Hopefully the
# other tools are close, but feel free to butcher as needed to produce more
# useful function.s
################################################################################
{ config, pkgs, lib, ... }: let
  # I'm the Riddler!
  murica = lib.strings.concatStrings [
    "a"
    "m"
    "e"
    "r"
    "i"
    "c"
    "a"
    "s"
  ];
  fromOrg = lib.strings.concatStrings [
    "n"
    "w"
    "e"
    "a"
  ];
  toOrg = lib.strings.concatStrings [
    "h"
    "m"
    "h"
  ];
  ghHost = "${toOrg}.ghe.com";
  migrations = [
    {
      bucketName = lib.strings.concatStrings (
        lib.lists.reverseList
          (lib.stringToCharacters
            "tekcub-ehg-ot-hsats-ttenrab-nagol-nitram-samoht"
          )
      );
      # This is computed from the adjacent id_rsa, but in PEM form and sans
      # passphrase, since the automation cannot handle a passphrase in the SSH
      # key.
      sourceServerPrivateKey = "$HOME/.ssh/id_rsa_bbs2gh.pem";
      type = "bbs";
      sourceServerSshUser = "atlbitbucket";
      sourceServerHome = "/stash/shared";
      sourceServerApiUrl = "https://stash.${murica}.${fromOrg}.pvt";
      sourceServerSshHost = "git01.mgmt.${fromOrg}colo.pvt";
    }
  ];

  # For GHEC on *.ghe.com the API lives at https://api.<host>.  If you’re on
  # self-hosted GHES (on-prem), override this to your "/api/v3" URL.
  targetApiUrl =
    if lib.hasInfix ".ghe.com" ghHost
    then "https://api.${ghHost}"
    else "https://api.github.com";
in {
  # One small shell file that *computes* env vars at runtime (no secrets in
  # store).
  home.file = builtins.listToAttrs (builtins.map
    (settings: let
      typeUpper = lib.strings.toUpper settings.type;
    in {
      name = ".config/gh/${settings.type}-env";
      value.text = ''
        export GH_PAT="$(pass ${toOrg}-ghe-pat)"
        export GH_TARGET_API_URL="${targetApiUrl}"
        export GH_ORG="${lib.strings.toUpper fromOrg}"
        export ${typeUpper}_USERNAME="$USER"
        export ${typeUpper}_PASSWORD="$(pass ${murica})"

        # S3 bucket settings.
        export AWS_ACCESS_KEY_ID="$(pass gh-migration-s3-key-id)"
        export AWS_REGION="us-west-2"
        export AWS_SECRET_ACCESS_KEY="$(pass gh-migration-s3-secret-key)"
        export BUCKET_NAME="${settings.bucketName}"

        export BBS_BASE_URL="${settings.sourceServerApiUrl}"
      '';
    })
    migrations
  );

  home.packages = map (settings: pkgs.writeShellApplication {
    name = "gh-migrate-${settings.type}";
    text = ''
      project="$1"
      repo="$2"
      if [[ "''${3:-}" == 'true' ]]; then
        set -x
      fi
      gh \
        ${settings.type}2gh \
        migrate-repo \
        --github-org "''$GH_ORG" \
        --github-repo "$repo" \
        --${settings.type}-project "$project" \
        --${settings.type}-repo "$repo" \
        --${settings.type}-server-url "''$BBS_BASE_URL" \
        --target-api-url "$GH_TARGET_API_URL" \
        --verbose \
        --ssh-user "${settings.sourceServerSshUser}" \
        --ssh-private-key "${settings.sourceServerPrivateKey}" \
        --archive-download-host "${settings.sourceServerSshHost}" \
        --bbs-shared-home "${settings.sourceServerHome}" \
        --github-pat "$GH_PAT" \
        --aws-region 'us-west-2' \
        --aws-bucket-name "$BUCKET_NAME"
    '';
  }
  ) migrations;

}
