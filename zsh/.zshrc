function loadlib() {
  lib=${1:?"You have to specify a library file"}
  if [ -f "$lib" ];then #ファイルの存在を確認
    . "$lib"
  fi
}

loadlib $ZDOTDIR/zshenv
loadlib $ZDOTDIR/zshautoload
loadlib $ZDOTDIR/zshfunc
loadlib $ZDOTDIR/zshalias
loadlib $ZDOTDIR/zshprompt
loadlib $ZDOTDIR/zshcomp
loadlib $ZDOTDIR/zshbase
loadlib $ZDOTDIR/zsh`uname`
loadlib $ZDOTDIR/zshlocal
