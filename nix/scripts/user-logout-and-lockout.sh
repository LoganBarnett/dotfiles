#!/usr/bin/env bash
################################################################################
# "Log out" a user by killing all of their processes and prevent them from
# logging back in.
# Usage:
#   $0 user1 user2 user3...
################################################################################

# "log" is taken, so this is slog - script log. Use it for logging in scripts.
# It prints the name of the script in a decorative means, and as a bonus prints
# the log to stderr. This helps reserve stdout for parseable output.
function slog {
  printf "\e[93m[\e[96m%s\e[93m]\e[0m " "$(basename "$0")" 1>&2
  echo -e "$*" 1>&2
}

failed=false
for user in "$@" ; do
  slog "Logging out $user..."
  # Regardless on whether or not the login false, we should also still lock the
  # user.
  usermod --expiredate 1 "$user"
  timeout 60s pkill --euid "$user"
  rc="$?"
  # 124 is if the process timed out.
  if [ "$?" -eq 124 ]; then
    slog "Logging out of $user timed out.  Forcing logout of $user..."
    pkill --euid "$user"
    rc="$?"
    if [ "$rc" -gt 0 ]; then
      slog "Force logout of $user failed!"
      failed=true
    fi
  elif [ "$rc" -gt 0 ]; then
      slog "Logout of $user failed!"
    failed=true
  fi
done

if [[ "$failed" == true ]]; then
  slog "Could not log out one or more users.  See logs for details."
  exit 1
else
  slog "All users logged out successfully."
  exit 0
fi
