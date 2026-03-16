#!/usr/bin/env bash
# Sets up Claude code review for any project.
#
# Usage: setup-claude-project.sh [project-dir]

set -euo pipefail

PROJECT_DIR="${1:-.}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "Setting up Claude code review for: $PROJECT_DIR"

# Copy CLAUDE.md template if it doesn't exist.
if [ ! -f "$PROJECT_DIR/CLAUDE.md" ]; then
    echo "Creating CLAUDE.md from template..."
    cp "$SCRIPT_DIR/CLAUDE.md" "$PROJECT_DIR/CLAUDE.md"
    echo "✓ Created CLAUDE.md - add project-specific guidelines as needed"
else
    echo "✓ CLAUDE.md already exists"
fi

# Create reviews directory.
mkdir -p "$PROJECT_DIR/reviews"
echo "✓ Created reviews directory"

# Add to .gitignore if needed.
if [ -f "$PROJECT_DIR/.gitignore" ]; then
    if ! grep -q "^reviews/" "$PROJECT_DIR/.gitignore"; then
        echo "reviews/" >> "$PROJECT_DIR/.gitignore"
        echo "✓ Added reviews/ to .gitignore"
    fi
fi

echo ""
echo "Setup complete!  Claude will now:"
echo "1. Automatically review code changes"
echo "2. Check compliance with your standards"
echo "3. Verify engineering best practices"
echo "4. Write reports to reviews/"
echo ""
echo "Your global agents are available:"
echo "- standards-reviewer"
echo "- senior-engineer"
echo "- code-review-orchestrator"
echo "- work-mode"