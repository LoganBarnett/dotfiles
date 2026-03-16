#!/usr/bin/env bash
# Nuclear option to reset Nextcloud for a fresh install.  Removes
# configuration and the admin user data, but preserves other user files.
# Override NEXTCLOUD_DATA_DIR and NEXTCLOUD_ADMIN_USER if the defaults do
# not match your deployment.
DATA_DIR="${NEXTCLOUD_DATA_DIR:-/mnt/nextcloud}"
ADMIN_USER="${NEXTCLOUD_ADMIN_USER:-admin}"
rm "$DATA_DIR/config/config.php" || true
rm "$DATA_DIR/config/override.config.php" || true
mv "$DATA_DIR/data/$ADMIN_USER"{,.bak-"$(date '+%s')"} || true
# We could wipe the entire server but there might be other services using
# PostgreSQL.  So just wipe the nextcloud database.
sudo -u postgres psql -c "DROP DATABASE nextcloud;" || true
sudo -u postgres psql -c "CREATE DATABASE nextcloud OWNER nextcloud;" \
  || true
