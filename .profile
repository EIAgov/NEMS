umask u=rwx,g=rwx,o=rwx
export SH='bash'
shopt -s nocaseglob
export PS1='[\h][`cygpath -m "$PWD"`] '
export NEMSDIR="M:"
export NEMS="$NEMSDIR/nems"
export SCRIPTS="scripts"

alias clear="cmd /c cls"

cd $NEMS
. $NEMS/$SCRIPTS/commands.sh