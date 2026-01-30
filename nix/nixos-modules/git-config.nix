##
# This module enables a host for doing local development.
{ lib, pkgs, git-users, ... }: let
  git-home-manager-user = (user: {
    ${user.host-username} = {
      programs.git = {
        enable = true;
        settings = {
          alias = {
            branchg="!git branch -avv | grep";
            fast-amend = "commit --amend --no-edit";
            # Removes branches that have been merged already.
            tidy = ''
          !(git branch --merged | sed /^\\*/d | xargs git branch -d ); \
          git remote prune origin
        '';
            tip = "log -1";
            # Removes remote branches that have been merged already. Guards
            # against some special branches that should probably be removed
            # manually.
            tidy-remote = ''
          !git remote prune origin; git branch -r --merged \
            | grep -v -E \
              '^.*?/('"$(git branch --show-current)"'$|master$|develop$|main$|release)' \
            | sed -E 's@origin/@@' \
            | xargs -I{} git push origin :{}
        '';
            wdiff = "diff --color-words";
          };
          color = {
            status = "auto";
            branch = "auto";
            diff = "auto";
          };
          commit = {
            # Case _might_ matter for Magit detecting this, even though Git
            # itself doesn't seem to care.
            gpgSign = true;
          };
          diff = {
            # "patience" is an algorithm that is smarter and has greater context
            # awareness when performing a diff. It takes a little longer, but
            # diffs are pretty fast as it is.
            # "histogram" has even better results than "patience", where I have
            # seen histogram properly identify moved code during a diff.
            algorithm = "histogram";
            # Allow seeing decrypted text for encrypted files during a diff.
            # One cannot stage hunks this way though.
            gpg = {
              textconv = "gpg --no-tty --decrypt";
            };
          };
          # Filters can be helpful to exclude changes from showing up in the
          # diff, thus never being staged, committed, nor conflicting.  This can
          # be helpful if a file must be checked in yet contains data that
          # changes meaninglessly over time with regards to the repository (like
          # an update timestamp).  This can also be used to mask sensitive
          # values, such as authentication tokens.
          #
          # In prior setups, I needed this.  I don't expect much usage here
          # between agenix-rekey and Nix itself.
          filter = {};
          # This is not an official Git configuration value but something that
          # Emacs' Forge package demands.  This may mean overriding it in other
          # areas, assuming Forge can even do that.
          github = {
            user = "LoganBarnett";
          };
          init = {
            defaultBranch = "main";
          };
          merge = {
            ff-only = "true";
          };
          core = {
            # By default git uses vi, which isn't always what we want. Set it to
            # vim instead. See:
            # http://www.freyskeyd.fr/fixing-there-was-a-problem-with-the-editor-vi-for-git/
            editor = "vim";
            # TODO: Migrate these to Nix as well.
            # attributesfile = ~/dev/dotfiles/gitattributes_global;
            # Now defaults to ~/.config/git/ignore
            # excludesfile = ~/dev/dotfiles/gitignore_global;
            # This doesn't allow for a Path so we have to lay down the file via
            # `environment.etc` and then reference it thusly.
            excludesfile = "/etc/gitignore";
            # Requires git 2.19, defaults to ~/.config/git/hooks
            hooksPath = "/etc/git-hooks";
            # Unsure why this was commented.
            # autocrlf = true;
          };
          pull = {
            ff-only = true;
            rebase = true;
          };
          push = {
            # Prevent obnoxious guards that prevent pushing to a newly created
            # branch to its corresponding remote branch.
            default = "current";
            followTags = true;
          };
          rebase = {
            autosquash = true;
            autoStash = true;
          };
          tag = {
            gpgSign = true;
          };
          user = {
            # GitHub uses the email address to associate to the account.  Other
            # services may do this too.  See
            # https://docs.github.com/en/account-and-profile/setting-up-and-managing-your-personal-account-on-github/managing-email-preferences/setting-your-commit-email-address#about-commit-email-addresses
            # for reference.  This means the user name can be anything (not to
            # be confused with a username).  So in most cases, it will be either
            # an alias or my proper name.
            email = user.git-email;
            name = user.git-name;
            # The signing keys set here are actually fingerprints, not a
            # "private key", which is typically what "key" means.  GPG indeed
            # even shows the fingerprints when using --list-secret-keys even
            # though nothing in its output is "secret".  See
            # https://unix.stackexchange.com/a/613909 for a decent answer on this.
            signingkey = user.git-signing-key;
          };
          # includeIf."gitdir:~/dev/nwea/" = {};
        };
      };
    };
  });
in {
  imports = [
    # We no longer need this, but maybe handy to add to nix-darwin proper.
    # ./git-program.nix
  ];
  environment.etc = {
    gitignore.source = ../git/gitignore_global;
  } // (
    lib.attrsets.mapAttrs'
      (name: value: {
        name = "git-hooks/${name}";
        value = { source = ../git/hooks/${name}; };
      })
      (builtins.readDir ../git/hooks)
  );
  nixpkgs.overlays = [
    # TODO: Contribute this back to nixpkgs, and update readme to _not_ use the
    # global configuration because some packages' tests can be fouled by the
    # system configuration (such as needing a GPG key).
    # (final: prev: {
    # git = prev.git.overrideAttrs (old: {
    # The original git Nix package doesn't set this if it's Darwin, but
    # doesn't mention why.  It instead sets NO_APPLE_COMMON_CRYPTO=1.  It
    # isn't apparent why this is an exclusive setting.  This is introduced
    # in nixpkgs#641a1e433a20d0748775a3c9fd1c633f141ff279 but there is no
    # mention as to why.  During this commit, the global configuration of
    # =sysconfdir= is moved there.  I suspect this is because macOS just
    # isn't getting enough locomotion here.  This isn't surprising since git
    # configuration management isn't yet supported on macOS via NixOS nor
    # nix-darwin.
    #
    # Unfortunately this setting triggers a full rebuild of git, and git
    # takes a while to build.
    # makeFlags = old.makeFlags ++ [ "sysconfdir=/etc" ];
    #   });
    # })
  ];
  home-manager.users = (lib.lists.foldr
    (a: b: a // b)
    {}
    (builtins.map git-home-manager-user git-users)
  );
}
