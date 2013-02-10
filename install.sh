#!/bin/bash

CONF=$(dirname $(readlink -f $0))

cd $HOME

# install packages
sudo apt-get install $(cat $CONF/packagelist)

# clean slate
rm -vrf .bashrc .emacs .emacs.d .gdbinit .Xresources .gnupg .xmonad .ssh .hindsight

# install links
mkdir -vp .ssh .xmonad .hindsight/conf .config/terminator
ln -vs $CONF/dotbashrc .bashrc
ln -vs $CONF/dotemacs .emacs
ln -vs $CONF/dotemacs.d .emacs.d
ln -vs $CONF/dotgdbinit .gdbinit
ln -vs $CONF/dotXresources .Xresources
ln -vs $CONF/ssh.conf .ssh/config
ln -vs $CONF/xmonad.hs .xmonad/xmonad.hs
ln -vs $CONF/blink .xmonad/blink
ln -vs $CONF/terminator.conf .config/terminator/config

xrdb .Xresources

# install secret stuff
cp -v $CONF/secret/id_rsa .ssh/
cp -va $CONF/secret/dotgnupg .gnupg
cp -v $CONF/secret/hindsight-key .hindsight/conf/key

# install xmonad
sudo cp -rv $CONF/xmonad/usr /
sudo ln -fsv $PWD/.cabal/bin/xmonad /usr/bin/xmonad

# disable desktop
gconftool-2 --type boolean --set /apps/nautilus/preferences/show_desktop false

echo 'ALL DONE!'
