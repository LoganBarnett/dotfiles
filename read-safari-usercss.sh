#! /usr/bin/env bash

################################################################################
# Dumps everything in the local storage for Injector onto disk. The result will
# include all transformations. This includes substitutions (which might be
# sensitive) and CSS variables.
################################################################################

set -e
# user_css_file=$(ls -1 ~/Library/Safari/LocalStorage/safari-extension_com.gridth.usercss*.localstorage);
user_css_file=$(ls -1 ~/Library/Safari/LocalStorage/safari-extension_com.tsbehlman.injector-*.localstorage);

get_keys_sql="select key from itemtable;"

# $1 - the key to select
function read_blob_sql() {
  echo "select hex(value) from itemtable where key = '$1';"
}

keys=`sqlite3 $user_css_file "$get_keys_sql"`

mkdir -p .tmp-css

for key in $keys; do
  if [ "$key" != "StyleStorageVersion" ]; then
      TMP_JSON=.tmp-css/user-css-$key.json
      sqlite3 $base_path$user_css_file "$(read_blob_sql $key)" |\
        xxd -r -p | tr -dc '[:print:]' | jq '.' > $TMP_JSON
      node ./write-css-from-safari.js $TMP_JSON custom-css $key
  fi
done
