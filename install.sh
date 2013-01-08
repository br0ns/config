#!/bin/bash

cd $HOME
rm -vrf .bashrc .emacs .emacs.d .gdbinit .Xresources .gnupg .xmonad .ssh .hindsight
ln -vs $PWD/config/dotbashrc .bashrc
ln -vs $PWD/config/dotemacs .emacs
ln -vs $PWD/config/dotemacs.d .emacs.d
ln -vs $PWD/config/dotgdbinit .gdbinit
ln -vs $PWD/config/dotXresources .Xresources
xrdb .Xresources
mkdir -vp .ssh .xmonad .hindsight/conf
ln -vs $PWD/config/dotsshslashconfig .ssh/config
ln -vs $PWD/config/xmonad.hs .xmonad/xmonad.hs
cp -v config/secret/id_rsa .ssh/
cp -va config/secret/dotgnupg .gnupg
cp -v config/secret/hindsight-key .hindsight/conf/key
