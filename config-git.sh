set -e

# By default git uses vi, which isn't always what we want. Set it to vim
# instead. See:
# http://www.freyskeyd.fr/fixing-there-was-a-problem-with-the-editor-vi-for-git/

git config --global core.editor /usr/bin/vim

# patience is an algorithm that is smarter and has greater context awareness
# when performing a diff. It takes a little longer, but diffs are pretty fast as
# it is.
git config --global diff.algorithm patience
