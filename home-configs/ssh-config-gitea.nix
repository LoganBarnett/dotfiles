################################################################################
# SSH configuration for Gitea server.
#
# This allows using simple git clone URLs like:
#   git clone git@gitea.proton:logan/repo.git
#
# Instead of requiring the explicit port format:
#   git clone ssh://git@gitea.proton:2222/logan/repo.git
################################################################################
{ ... }: {
  programs.ssh = {
    matchBlocks = {
      "gitea.proton" = {
        port = 2222;
        user = "git";
      };
    };
  };
}
