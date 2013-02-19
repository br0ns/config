#!/bin/bash

if [ $UID -ne 0 ] ; then
    sudo $0 $@
fi

CONF=$(dirname $(readlink -f $0))

cd $HOME

# update
sudo apt-get update

# install source headers
sudo apt-get install linux-headers-$(uname -r)

# install packages
sudo apt-get install $(cat $CONF/packagelist)

# set blink suid
sudo chown root. $CONF/blink
sudo chmod u+s $CONF/blink

# clean slate
rm -vrf .bashrc .emacs .emacs.d .gdbinit .Xresources .gnupg .xmonad .ssh .hindsight

# install links
mkdir -vp .ssh .xmonad .hindsight/conf .config/terminator scratchpads downloads
ln -vs $CONF/dotbashrc .bashrc
ln -vs $CONF/dotemacs.d/init.el .emacs
ln -vs $CONF/dotemacs.d .emacs.d
ln -vs $CONF/dotgdbinit .gdbinit
ln -vs $CONF/dotXresources .Xresources
ln -vs $CONF/ssh.conf .ssh/config
ln -vs $CONF/xmonad.hs .xmonad/xmonad.hs
ln -vs $CONF/blink .xmonad/blink
ln -vs $CONF/terminator.conf .config/terminator/config
ln -vs $CONF/xmonad-lib .xmonad/lib

xrdb .Xresources

# install secret stuff
cp -v $CONF/secret/id_rsa .ssh/
cp -va $CONF/secret/dotgnupg .gnupg
cp -v $CONF/secret/hindsight-key .hindsight/conf/key

# install xmonad
sudo cp -rv $CONF/xmonad/usr /
sudo ln -fsv $PWD/.cabal/bin/xmonad /usr/bin/xmonad

# install hindsight
cp -rv $CONF/hindsight-modules .hindsight/modules

# update cabal
cabal update
cabal install cabal-install

# install xmonad
cd $HOME/code/xmonad
git pull
cabal install
cd $HOME/code/XMonadContrib
git pull
cabal install
cd $HOME
# packages needed for my xmonad configuration
cabal install regex-pcre

echo 'ALL DONE!'
