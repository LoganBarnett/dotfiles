#!/usr/bin/env bash
# Add this to your git init template to auto-setup Claude in new repos.

# Add to ~/.gitconfig:
# [init]
#     templateDir = ~/.git-template

mkdir -p ~/.git-template/hooks

cat > ~/.git-template/hooks/post-checkout << 'EOF'
#!/usr/bin/env bash
# Auto-setup Claude if this is a new clone.

if [ "$1" = "0000000000000000000000000000000000000000" ]; then
    # New clone - check if CLAUDE.md exists.
    if [ ! -f "CLAUDE.md" ]; then
        echo "New project detected.  Run setup-claude-project.sh to enable"
        echo "automatic code reviews with Claude."
    fi
fi
EOF

chmod +x ~/.git-template/hooks/post-checkout

echo "Git template updated.  New repos will remind you to set up Claude."