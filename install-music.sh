#! /usr/bin/env bash

echo "Ensuring Lilypond support (drawing sheet music)..."

brew cask install lilypond

# Lilypond's png conversion doesn't work out of box. install_name_tool fixes the
# linking issue. See
# http://lilypond.1069038.n5.nabble.com/DYLD-LIBRARY-PATH-fails-in-gs-OS-X-El-Capitan-td181481.html#a183547

cd /Applications/LilyPond.app/Contents/Resources/bin && \
    install_name_tool -change \
                      ./bin/../sobin/libgs.8.70.dylib \
                      @executable_path/../lib/libgs.8.70.dylib \
                      gs
