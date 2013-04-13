#!/bin/bash

if [ $UID -ne 0 ] || [ -z "$SUDO_USER" ] ; then
    echo "This script must be run with sudo" 1>&2
    exit
fi

# at this point we're running through sudo
# commands that should run as the user must be prefixed with $DO
DO="sudo -u $SUDO_USER"

CONF=$(dirname $(readlink -f $0))

# decrypt secret stuff
$DO gpg $CONF/secret.tar.gpg || exit

# make sure all submodules are pulled
cd $CONF
$DO git submodule init
$DO git submodule update
cd $HOME

# update
yes | apt-get update

# install source headers
yes | apt-get install linux-headers-$(uname -r)

# install packages
yes | apt-get install $(cat $CONF/packagelist)

# clean slate
$DO rm -vrf .bashrc .emacs .emacs.d .gdbinit .Xresources .gnupg .xmonad .ssh \
    .hindsight

# install links
$DO mkdir -vp .ssh .xmonad .hindsight/conf .config/terminator scratchpads \
    downloads bin code
$DO ln -vsTf $CONF/dotbashrc .bashrc
$DO ln -vsTf $CONF/dotgitconfig .gitconfig
$DO ln -vsTf $CONF/dotemacs.d/init.el .emacs
$DO ln -vsTf $CONF/dotemacs.d .emacs.d
$DO ln -vsTf $CONF/dotgdbinit .gdbinit
$DO ln -vsTf $CONF/dotXresources .Xresources
$DO ln -vsTf $CONF/ssh.conf .ssh/config
$DO ln -vsTf $CONF/xmonad.hs .xmonad/xmonad.hs
$DO ln -vsTf $CONF/terminator.conf .config/terminator/config
$DO ln -vsTf $CONF/xmonad-lib .xmonad/lib

$DO xrdb .Xresources

# install hindsight
$DO cp -rv $CONF/hindsight-modules .hindsight/modules

# update cabal
$DO cabal update
$DO cabal install cabal-install

# install xmonad
cp -rv $CONF/xmonad/usr /
ln -fsv $PWD/.cabal/bin/xmonad /usr/bin/xmonad
$DO git clone https://github.com/reenberg/xmonad.git code/xmonad 2>/dev/null
cd code/xmonad
$DO git pull
$DO cabal install
cd $HOME

# install XMonadContrib
$DO git clone https://github.com/reenberg/XMonadContrib.git code/XMonadContrib \
    2>/dev/null
cd code/XMonadContrib
$DO git pull
$DO cabal install
cd $HOME

# packages needed for my xmonad configuration
$DO cabal install regex-pcre

# install blink for irc notifications
gcc $CONF/blink.c -o .xmonad/blink
chmod u+s .xmonad/blink

# install SML dev env
# install mylib
echo 'Installing MyLib...'
$DO git clone git@github.com:mortenbp/mylib.git code/sml/mylib 2>/dev/null
cd code/sml/mylib
$DO git pull origin master
cd $HOME
echo 'DONE!'

# install preml
echo 'Installing Preml...'
$DO git clone git@github.com:mortenbp/PreML.git code/sml/preml 2>/dev/null
cd code/sml/preml
$DO git pull origin master
$DO make
make install
cd $HOME
echo 'DONE!'

# install shackl
echo 'Installing Shackl...'
$DO git clone git@github.com:mortenbp/shackl.git code/sml/shackl 2>/dev/null
cd code/sml/shackl
$DO git pull origin master
$DO make
make install
cd $HOME
$DO sh -c "echo \$\(SML_LIB\)/basis/basis.mlb > .shackl"
$DO sh -c "echo $HOME/code/sml/mylib/MyLib.mlb >> .shackl"
echo 'DONE!'

# modify sml-mode to work with do-notation
cd /usr/share/emacs23/site-lisp/sml-mode
mv sml-defs.el sml-defs.el.bak
cp $HOME/code/sml/preml/sml-defs.el .
emacs --batch --eval '(byte-compile-file "sml-defs.el")'
cd $HOME

# byte compile Emacs' files
$DO emacs --batch --eval '(byte-recompile-directory "~/.emacs.d" 0)'

# make GDB work properly
echo "kernel.yama.ptrace_scope = 0" > /etc/sysctl.d/10-ptrace.conf

# install secret stuff
cd $CONF
$DO tar xfv secret.tar
$DO rm secret.tar
$DO cp -v secret/id_rsa $HOME/.ssh/
$DO ssh-keygen -f $HOME/.ssh/id_rsa -y > $HOME/.ssh/id_rsa.pub
$DO cp -va secret/dotgnupg $HOME/.gnupg
$DO cp -v secret/hindsight-key $HOME/.hindsight/conf/key
cd $HOME

echo 'ALL DONE!'
