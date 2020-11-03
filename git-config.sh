set -e

# By default git uses vi, which isn't always what we want. Set it to vim
# instead. See:
# http://www.freyskeyd.fr/fixing-there-was-a-problem-with-the-editor-vi-for-git/
git config --global core.editor $(which vim)


DOTFILES="
gitattributes_global
gitconfig
gitignore_global
"

for dotfile in $DOTFILES
do
  ./link-dotfile.sh $dotfile
done
