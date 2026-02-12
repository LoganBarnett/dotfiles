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
# Does NOT modify working directory or create commits.

set -euo pipefail

readonly GITEA_HOST="gitea.proton"
readonly GITHUB_USER="LoganBarnett"
readonly GITEA_USER="logan"
readonly DEV_DIR="$HOME/dev"
readonly DRY_RUN="${DRY_RUN:-1}"

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

    # Check if repo already exists.
    if tea repo list | grep -q "^${repo_name}\$"; then
        log "Repo $repo_name already exists on Gitea"
        return 0
    fi

    tea repo create --name "$repo_name" --private
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

    # Add gitea remote temporarily if not exists.
    if ! git -C "$repo_dir" remote get-url gitea &>/dev/null; then
        git -C "$repo_dir" remote add gitea \
            "https://${GITEA_HOST}/logan/${repo_name}.git"
    fi

    # Push all branches and tags.
    git -C "$repo_dir" push gitea --all
    git -C "$repo_dir" push gitea --tags
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

    # Rename origin to github.
    if git -C "$repo_dir" remote get-url origin &>/dev/null; then
        git -C "$repo_dir" remote rename origin github
    fi

    # Rename gitea to origin.
    if git -C "$repo_dir" remote get-url gitea &>/dev/null; then
        git -C "$repo_dir" remote rename gitea origin
    fi
}

# Process a single repo.
process_repo() {
    local repo_dir=$1
    local repo_name
    repo_name=$(get_repo_name "$repo_dir")

    log "Processing: $repo_name"

    # Create on Gitea.
    create_gitea_repo "$repo_name"

    # Push to Gitea.
    push_to_gitea "$repo_dir" "$repo_name"

    # Migrate remotes.
    migrate_remotes "$repo_dir" "$repo_name"

    log "Completed: $repo_name"
    echo ""
}

# Main execution.
main() {
    log "Starting migration (DRY_RUN=$DRY_RUN)"
    log "Looking for repos in: $DEV_DIR"

    local count=0
    local processed=0

    while IFS= read -r repo_dir; do
        ((count++)) || true

        if is_loganbarnett_repo "$repo_dir"; then
            ((processed++)) || true
            process_repo "$repo_dir"
        fi
    done < <(find_repos)

    log "Found $count repos total"
    log "Processed $processed LoganBarnett repos"

    if [ "$DRY_RUN" = "1" ]; then
        log ""
        log "This was a DRY RUN. To execute, run:"
        log "  DRY_RUN=0 $0"
    fi
}

main "$@"
