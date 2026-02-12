#!/usr/bin/env bash

# Migrates repos from GitHub (LoganBarnett) to Gitea (gitea.proton).
# This script:
# - Finds all git repos in ~/dev
# - Identifies those with LoganBarnett GitHub remotes
# - Creates matching repos on gitea.proton
# - Pushes all branches and tags to gitea
# - Renames 'origin' remote to 'github'
# - Adds gitea as new 'origin'
#
# Usage: ./migrate-repos-to-gitea.sh [REPO_NAME]
#   REPO_NAME: Optional. If specified, only process this specific repo.
#
# Does NOT modify working directory or create commits.

set -euo pipefail

readonly GITEA_HOST="gitea.proton"
readonly GITEA_SSH_PORT="2222"
readonly GITHUB_USER="LoganBarnett"
readonly GITEA_USER="logan"
readonly DEV_DIR="$HOME/dev"
readonly DRY_RUN="${DRY_RUN:-1}"
readonly FAILED_REPOS_FILE="/tmp/gitea-migration-failed-repos.txt"
readonly REPO_DELAY=10  # Seconds to wait between repos

# Get Gitea token from tea config.
readonly TEA_CONFIG="$HOME/Library/Application Support/tea/config.yml"
readonly GITEA_TOKEN=$(grep -A 10 "name: gitea" "$TEA_CONFIG" | \
    grep "token:" | awk '{print $2}')

log() {
    echo "[$(date +'%Y-%m-%d %H:%M:%S')] $*" >&2
}

error() {
    log "ERROR: $*"
    exit 1
}

# Verify Gitea token was found.
if [ -z "$GITEA_TOKEN" ]; then
    error "Gitea token not found. Run 'tea logins add' first."
fi

# Get all git repos in ~/dev (shallow).
find_repos() {
    find "$DEV_DIR" -maxdepth 2 -name .git -type d | sed 's|/.git$||'
}

# Check if repo has LoganBarnett GitHub remote.
is_loganbarnett_repo() {
    local repo_dir=$1
    git -C "$repo_dir" remote get-url origin 2>/dev/null | \
        grep -q "github.com[:/]${GITHUB_USER}/"
}

# Extract repo name from directory.
get_repo_name() {
    local repo_dir=$1
    basename "$repo_dir"
}

# Create repo on Gitea.
create_gitea_repo() {
    local repo_name=$1
    log "Creating repo on Gitea: $repo_name"

    if [ "$DRY_RUN" = "1" ]; then
        log "[DRY RUN] Would run: tea repo create --name $repo_name --private"
        return 0
    fi

    # Check if repo already exists by looking for the name in the table output.
    # tea repo list outputs a formatted table, so we check if the repo name
    # appears in the NAME column.
    if tea repo list | grep -q "│ ${repo_name} "; then
        log "Repo $repo_name already exists on Gitea, skipping creation"
        return 0
    fi

    # Create the repo, but handle the case where it was created between our
    # check and this command.
    if ! tea repo create --name "$repo_name" --private 2>&1; then
        # Check if error is about repo already existing.
        if tea repo list | grep -q "│ ${repo_name} "; then
            log "Repo $repo_name was created by another process, continuing"
            return 0
        else
            # Some other error, fail.
            log "ERROR: Failed to create repo $repo_name"
            return 1
        fi
    fi
}

# Check if error message indicates a network problem.
is_network_error() {
    local error_msg=$1
    # Common network-related error patterns.
    if echo "$error_msg" | grep -qi \
        -e "connection timed out" \
        -e "could not resolve host" \
        -e "network is unreachable" \
        -e "connection refused" \
        -e "failed to connect" \
        -e "unable to access" \
        -e "operation timed out" \
        -e "temporary failure in name resolution" \
        -e "ssh_exchange_identification"; then
        return 0
    fi
    return 1
}

# Push all branches and tags to gitea.
push_to_gitea() {
    local repo_dir=$1
    local repo_name=$2
    log "Pushing $repo_name to Gitea"

    if [ "$DRY_RUN" = "1" ]; then
        log "[DRY RUN] Would push all branches and tags"
        return 0
    fi

    # Determine which remote to use for pushing.
    # If origin already points to gitea (migration complete), use origin.
    # Otherwise, use gitea remote (add if needed).
    local remote_to_use="gitea"
    if git -C "$repo_dir" remote get-url origin &>/dev/null; then
        local origin_url=$(git -C "$repo_dir" remote get-url origin)
        if [[ "$origin_url" == *"${GITEA_HOST}"* ]]; then
            log "Remote 'origin' already points to Gitea, using it for push"
            remote_to_use="origin"
        fi
    fi

    # Add gitea remote if we're using it and it doesn't exist.
    # Use ssh:// format to specify non-standard port.
    if [ "$remote_to_use" = "gitea" ]; then
        if ! git -C "$repo_dir" remote get-url gitea &>/dev/null; then
            log "Adding gitea remote"
            git -C "$repo_dir" remote add gitea \
                "ssh://git@${GITEA_HOST}:${GITEA_SSH_PORT}/${GITEA_USER}/${repo_name}.git"
        fi
    fi

    # Check if repo has any branches to push.
    local branches
    branches=$(git -C "$repo_dir" branch --format='%(refname:short)' 2>/dev/null || true)
    if [ -z "$branches" ]; then
        log "WARNING: No branches found in repo, skipping push"
        return 0
    fi

    # Push all branches and tags with retry logic for network errors only.
    log "Pushing all branches and tags to ${remote_to_use}"
    local max_retries=3
    local retry_delay=10

    for attempt in $(seq 1 $max_retries); do
        local error_output
        error_output=$(mktemp)
        local push_success=true

        # Push branches explicitly to handle empty remote repos.
        # Use --force-with-lease for safety, but allow initial push to empty repo.
        if ! git -C "$repo_dir" for-each-ref --format='%(refname:short)' refs/heads/ | \
            while read -r branch; do
                git -C "$repo_dir" push "$remote_to_use" "$branch" 2>>"$error_output" || exit 1
            done; then
            push_success=false
        fi

        # Push tags if branches succeeded.
        if [ "$push_success" = true ]; then
            if ! git -C "$repo_dir" push "$remote_to_use" --tags 2>>"$error_output"; then
                push_success=false
            fi
        fi

        if [ "$push_success" = true ]; then
            rm -f "$error_output"
            log "Successfully pushed to ${remote_to_use}"
            return 0
        else
            local errors=$(cat "$error_output")
            rm -f "$error_output"

            # Check if this is a network error.
            if is_network_error "$errors"; then
                if [ $attempt -lt $max_retries ]; then
                    log "Network error detected (attempt $attempt/$max_retries), retrying in ${retry_delay}s..."
                    log "Error: $errors"
                    sleep $retry_delay
                else
                    log "ERROR: Network error persists after $max_retries attempts"
                    log "Error: $errors"
                    return 1
                fi
            else
                # Non-network error, fail immediately.
                log "ERROR: Non-network error encountered, not retrying"
                log "Error: $errors"
                return 1
            fi
        fi
    done
}

# Migrate remotes: origin -> github, gitea -> origin.
migrate_remotes() {
    local repo_dir=$1
    local repo_name=$2
    log "Migrating remotes for $repo_name"

    if [ "$DRY_RUN" = "1" ]; then
        log "[DRY RUN] Would rename origin -> github"
        log "[DRY RUN] Would rename gitea -> origin"
        return 0
    fi

    # Rename origin to github if origin exists and github doesn't.
    if git -C "$repo_dir" remote get-url origin &>/dev/null; then
        if git -C "$repo_dir" remote get-url github &>/dev/null; then
            log "Remote 'github' already exists, skipping origin -> github rename"
        else
            log "Renaming origin -> github"
            git -C "$repo_dir" remote rename origin github
        fi
    else
        log "Remote 'origin' doesn't exist (might be already migrated)"
    fi

    # Rename gitea to origin if gitea exists and origin doesn't.
    if git -C "$repo_dir" remote get-url gitea &>/dev/null; then
        if git -C "$repo_dir" remote get-url origin &>/dev/null; then
            log "Remote 'origin' already exists, skipping gitea -> origin rename"
        else
            log "Renaming gitea -> origin"
            git -C "$repo_dir" remote rename gitea origin
        fi
    else
        log "Remote 'gitea' doesn't exist (might be already renamed to origin)"
    fi
}

# Process a single repo.
process_repo() {
    local repo_dir=$1
    local repo_name
    repo_name=$(get_repo_name "$repo_dir")

    log "Processing: $repo_name"

    # Create on Gitea (continue on error).
    if ! create_gitea_repo "$repo_name"; then
        log "WARNING: Repo creation failed for $repo_name, continuing anyway"
    fi

    # Push to Gitea (continue on error).
    if ! push_to_gitea "$repo_dir" "$repo_name"; then
        log "WARNING: Push failed for $repo_name, skipping remote migration"
        echo "$repo_name" >> "$FAILED_REPOS_FILE"
        echo ""
        return 1
    fi

    # Migrate remotes (continue on error).
    if ! migrate_remotes "$repo_dir" "$repo_name"; then
        log "WARNING: Remote migration failed for $repo_name"
        echo "$repo_name" >> "$FAILED_REPOS_FILE"
        echo ""
        return 1
    fi

    log "Completed: $repo_name"
    echo ""
    return 0
}

# Main execution.
main() {
    local target_repo="${1:-}"

    if [ -n "$target_repo" ]; then
        log "Starting migration for specific repo: $target_repo (DRY_RUN=$DRY_RUN)"
    else
        log "Starting migration (DRY_RUN=$DRY_RUN)"
    fi
    log "Looking for repos in: $DEV_DIR"

    # Clear failed repos file.
    > "$FAILED_REPOS_FILE"

    local count=0
    local processed=0
    local succeeded=0
    local failed=0

    while IFS= read -r repo_dir; do
        ((count++)) || true

        local repo_name
        repo_name=$(get_repo_name "$repo_dir")

        # Skip if target repo specified and this isn't it.
        if [ -n "$target_repo" ] && [ "$repo_name" != "$target_repo" ]; then
            continue
        fi

        if is_loganbarnett_repo "$repo_dir"; then
            ((processed++)) || true

            if process_repo "$repo_dir"; then
                ((succeeded++)) || true
            else
                ((failed++)) || true
            fi

            # Rate limit: delay between repos to avoid overwhelming Gitea.
            if [ "$DRY_RUN" != "1" ] && [ $processed -lt $count ]; then
                sleep $REPO_DELAY
            fi
        fi
    done < <(find_repos)

    log ""
    log "============================================"
    log "Migration Summary"
    log "============================================"
    log "Found $count repos total"
    log "Processed $processed LoganBarnett repos"
    log "Succeeded: $succeeded"
    log "Failed: $failed"

    if [ $failed -gt 0 ] && [ -s "$FAILED_REPOS_FILE" ]; then
        log ""
        log "Failed repos:"
        cat "$FAILED_REPOS_FILE" | while read -r repo; do
            log "  - $repo"
        done
        log ""
        log "To retry failed repos, run the script again."
        log "It will skip already-migrated repos."
    fi

    if [ "$DRY_RUN" = "1" ]; then
        log ""
        log "This was a DRY RUN. To execute, run:"
        log "  DRY_RUN=0 $0"
    fi
}

main "$@"
