################################################################################
# This is an effort to restore the removal of zsh-git-prompt via
# https://github.com/NixOS/nixpkgs/pull/437745.  It kind of slipped through and
# these things happen.  There is mention in the issue of using more recent
# versions, which this derivation attempts to use so it will be suitable for a
# PR to nixpkgs.
################################################################################
# zsh-git-prompt -- Informative git prompt for zsh
#
# Usage: to enable this plugin for all users, you could
# add it to configuration.nix like this:
#
#   programs.zsh.interactiveShellInit = ''
#     source ${pkgs.zsh-git-prompt}/share/zsh-git-prompt/zshrc.sh
#   '';
#
# Or you can install it globally but only enable it in individual
# users' ~/.zshrc files:
#
#   source /run/current-system/sw/share/zsh-git-prompt/zshrc.sh
#
# Or if installed locally:
#
#   source ~/.nix-profile/share/zsh-git-prompt/zshrc.sh
#
# Either way, you then have to set a prompt that incorporates
# git_super_status, for example:
#
#   PROMPT='%B%m%~%b$(git_super_status) %# '
#
# More details are in share/doc/zsh-git-prompt/README.md, once
# installed.
#
{
  fetchFromGitHub,
  git,
  lib,
  python3,
  stdenv,
}: stdenv.mkDerivation (let
  pname = "zsh-git-prompt";
    version = "0.6";
in {
  inherit pname version;
  src = fetchFromGitHub {
    owner = "zsh-git-prompt";
    repo = "zsh-git-prompt";
    rev = "v${version}";
    hash = "sha256-F9tREogSVMfIcu0cFpWzF187C9pkVX+7jMVXI+uj28A=";
  };
  # TODO: Build the gitstatus Haskell package that's included in this
  # repository, and perform the necessary substitution to use it here.  But this
  # should just use the normal `git status` just fine, so maybe there's nothing
  # to do here really.
  prePatch = ''
    substituteInPlace zshrc.sh \
      --replace-warn ':-"python"' '${python3.interpreter} ' \
      --replace-warn 'git ' '${git}/bin/git '
  '';
  preCompileBuildDriver = "cd src";
  installPhase = ''
    runHook preInstall
    mkdir --parents "$out/share/${pname}"
    cp --recursive --no-preserve=mode,ownership,timestamps \
      zshrc.sh haskell python shell utils README.md LICENSE.md \
      "$out/share/${pname}/"
    runHook postInstall
  '';
  meta = {
    homepage = "https://github.com/zsh-git-prompt/zsh-git-prompt";
    description = "Informative git prompt for zsh";
    license = lib.licenses.mit;
    maintainers = [ lib.maintainers.league ];
  };
})
