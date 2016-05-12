#!/usr/bin/env zsh
set -ue

PKG=ab
VERSION=2.4.2
SRC_DIR=httpd-$VERSION

# Install prefix
PREFIX=/usr/local

# Package prefix (files will be symlinked to install prefix)
PKG_PREFIX=$PREFIX/pkg/$PKG-$VERSION

SUB_BIN=bin
SUB_MAN1=share/man/man1

# Create directories
sudo install -d $PKG_PREFIX/{$SUB_BIN,$SUB_MAN1}

WORK_DIR="$(mktemp -dt `basename $0`)" && cd $WORK_DIR

echo "\n### Compiling \`ab' in $WORK_DIR\n\n" >&2

curl http://www.apache.org/dist/httpd/$SRC_DIR.tar.bz2 | tar x
cd $SRC_DIR

# Patch ab.c to match latest SVN trunk version
PATCH='conditionally-bind-socket.diff'
curl 'http://svn.apache.org/viewvc/httpd/httpd/trunk/support/ab.c?r1=1351737&r2=1351736&pathrev=1351737&view=patch' -o $PATCH
patch -f -p3 -i $PATCH

# Patch `configure' script to do away with dependency on PCRE (`ab' does not use PCRE)
patch -p1 << 'EOP'
diff --git a/configure b/configure
index 5f4c09f..84d3de2 100755
--- a/configure
+++ b/configure
@@ -6037,8 +6037,6 @@ $as_echo "$as_me: Using external PCRE library from $PCRE_CONFIG" >&6;}
     done
   fi
 
-else
-  as_fn_error $? "pcre-config for libpcre not found. PCRE is required and available from http://pcre.org/" "$LINENO" 5
 fi
 
   APACHE_VAR_SUBST="$APACHE_VAR_SUBST PCRE_LIBS"
EOP

# Avoid an error of the APR libtool on Mountain Lion
export LTFLAGS='--tag CC'
# Configure
./configure --silent --prefix="$PREFIX" --disable-debug --disable-dependency-tracking

# Change to support directory, where `ab' is located
cd support

# Compile ab
make -s ab

sudo install ab $PKG_PREFIX/$SUB_BIN
sudo install ../docs/man/ab.1 $PKG_PREFIX/$SUB_MAN1

cd $PKG_PREFIX
for d ($SUB_BIN $SUB_MAN1); do
    for f ($d/*); do
        sudo ln -sf $PKG_PREFIX/$f $PREFIX/$d
    done
done

echo "\n\n### Successfully installed \`ab' to $PKG_PREFIX and linked it into $PREFIX\n\n" >&2