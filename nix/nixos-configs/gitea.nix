################################################################################
# Gitea (pronounced "git-tee") is a git collaboration server.  Much like GitHub,
# it offers pull requests, diff viewing, build triggers, and merge checks.
#
# Gitea is favorable to me over GitLab because it is lighter weight and more
# dedicated to the singular role of being a git collaboration server, whereas
# GitLab requires many more resources and revolves around a kind of DevOps
# management landscape.
################################################################################
{ config, host-id, pkgs, ... }: {
  services.gitea = {
    enable = true;
    database = {
      type = "postgres";
    };
    settings = {
      service = {
        DISABLE_REGISTRATION = true;
        DOMAIN = "${host-id}.proton";
        # This defaults to using HTTP.  Probably because the defaults don't
        # expect an easy HTTPS reverse proxy configuration.
        ROOT_URL = "https://${config.services.gitea.settings.service.DOMAIN}/";
      };
    };
  };
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_16;
    ensureDatabases = [ "gitea" ];
    ensureUsers = [
      {
        # This uses peer authentication by default, which means only users of
        # matching Unix user names can connect as this user.
        name = config.services.gitea.database.user;
        ensureDBOwnership = true;
      }
    ];
  };
}
