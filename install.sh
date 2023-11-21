#!/usr/bin/env bash

REPO_URL="https://github.com/sk4rd/emacs.d.git"
CLONE_DIR="/tmp/emacs.d"
TARGET_DIR="$HOME/.emacs.d"

# Function to check if a command exists
command_exists() {
    command -v "$@" > /dev/null 2>&1
}

# Check if the repository is already cloned
if [ -d "$CLONE_DIR" ]; then
    echo "Emacs configuration repository already exists in $CLONE_DIR."
else
    # Cloning the Emacs configuration repository
    echo "Cloning Emacs configuration from $REPO_URL..."
    git clone "$REPO_URL" "$CLONE_DIR" || { echo "Cloning failed"; exit 1; }
fi

# Copying necessary files
echo "Setting up configuration files..."
mkdir -p "$TARGET_DIR"
cp "$CLONE_DIR/init.el" "$TARGET_DIR"
cp "$CLONE_DIR/early-init.el" "$TARGET_DIR"

echo "Emacs configuration setup is complete."
