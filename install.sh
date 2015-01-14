#!/bin/bash

if [ $UID -ne 0 ] || [ -z "$SUDO_USER" ] ; then
    echo "This script must be run with sudo" 1>&2
    exit
fi

set -x

# at this point we're running through sudo
# commands that should run as the user must be prefixed with $DO
DO="sudo -u $SUDO_USER"

CONF=$(dirname $(readlink -f $0))

# decrypt secret stuff
$DO gpg --yes $CONF/secret.tar.gpg || exit

# make sure all submodules are pulled
cd $CONF
$DO git submodule init
$DO git submodule update
HOME="/home/$SUDO_USER"
cd

# add additional repositories
grep -q "additional repositories" /etc/apt/sources.list || \
    cat >> /etc/apt/sources.list <<EOF

# additional repositories
deb http://http.debian.net/debian/ jessie main contrib non-free
deb-src http://http.debian.net/debian/ jessie main contrib non-free

deb http://ftp.dk.debian.org/debian/ squeeze main contrib non-free
deb-src http://ftp.dk.debian.org/debian/ squeeze main contrib non-free

deb http://ftp.dk.debian.org/debian/ wheezy main contrib non-free
deb-src http://ftp.dk.debian.org/debian/ wheezy main contrib non-free

deb http://www.emdebian.org/debian squeeze main
EOF

# add i386
dpkg --add-architecture i386

# install emdebian keyring
apt-get install emdebian-archive-keyring

# update
apt-get -y update

# install source headers
apt-get -y install linux-headers-$(uname -r)

# install packages
apt-get -y install $(cat $CONF/packagelist) || exit

# add googles dns server
grep -q "8.8.8.8" /etc/dhcp/dhclient.conf || \
    echo "append domain-name-servers 8.8.8.8, 8.8.4.4;" >> /etc/dhcp/dhclient.conf

# clean slate
$DO rm -vrf .bashrc .emacs .emacs.d .gdbinit .Xresources .gnupg .xmonad .ssh \
    .hindsight

# install links
$DO mkdir -vp .ssh .xmonad .hindsight/conf downloads bin code
$DO ln -vsTf $CONF/dotbashrc .bashrc
$DO ln -vsTf $CONF/dotgitconfig .gitconfig
$DO ln -vsTf $CONF/dotgit_template .git_template
$DO ln -vsTf $CONF/dotemacs.d/init.el .emacs
$DO ln -vsTf $CONF/dotemacs.d .emacs.d
$DO ln -vsTf $CONF/dotgdbinit .gdbinit
$DO ln -vsTf $CONF/dotXdefaults .Xdefaults
$DO ln -vsTf $CONF/dotXdefaults .Xresources
$DO ln -vsTf $CONF/xmonad.hs .xmonad/xmonad.hs
$DO ln -vsTf $CONF/urxvt .urxvt
$DO ln -vsTf $CONF/xmonad-lib .xmonad/lib
$DO ln -vsTf $CONF/dotxscreensaver .xscreensaver

$DO xrdb .Xdefaults

# install hindsight
$DO cp -rv $CONF/hindsight/modules .hindsight/modules
$DO ln -vsTf $CONF/hindsight/hindsight bin/hindsight
$DO ln -vsTf $CONF/hindsight/hindsight-backup bin/hindsight-backup
$DO ln -vsTf $CONF/hindsight/hindsight-mount bin/hindsight-mount

# install secret stuff
cd $CONF
$DO tar xfv secret.tar
$DO rm secret.tar

# ssh config
$DO ln -vsTf $CONF/secret/ssh.conf $HOME/.ssh/config

# main ssh key
$DO ln -vsTf $CONF/secret/id_rsa $HOME/.ssh/id_rsa
$DO ssh-keygen -f $HOME/.ssh/id_rsa -y > $HOME/.ssh/id_rsa.pub

# other keys
$DO mkdir $HOME/.ssh/keys
for k in secret/sshkeys/* ; do
  $DO ln -vsTf $CONF/$k $HOME/.ssh/keys/$(basename $k)
  $DO ssh-keygen -f $CONF/$k -y > $HOME/.ssh/keys/$(basename $k).pub
done

$DO ln -vsTf $CONF/secret/dotgnupg $HOME/.gnupg
$DO ln -vsTf $CONF/secret/hindsight-key $HOME/.hindsight/conf/key
$DO ln -vsTf $CONF/secret/ssh.conf $HOME/.ssh/config
cd

# update cabal
$DO cabal update
$DO cabal install cabal-install

# install xmonad
mkdir -p .config/autostart
cp $CONF/xmonad.desktop .config/autostart
ln -fsv $PWD/.cabal/bin/xmonad /usr/bin/xmonad
$DO git clone https://github.com/reenberg/xmonad.git code/xmonad 2>/dev/null
cd code/xmonad
$DO git pull
$DO cabal install
cd

# install XMonadContrib
$DO git clone https://github.com/reenberg/XMonadContrib.git code/XMonadContrib \
    2>/dev/null
cd code/XMonadContrib
$DO git pull
$DO cabal install
cd

# packages needed for my xmonad configuration
$DO cabal install regex-pcre

# install blink for irc notifications
gcc $CONF/blink.c -o .xmonad/blink
chmod u+s .xmonad/blink

# install SML dev env
mkdir code/sml
# install mylib
echo 'Installing MyLib...'
$DO git clone git@github.com:mortenbp/mylib.git code/sml/mylib
cd code/sml/mylib
$DO git pull origin master
cd
echo 'DONE!'

# install preml
echo 'Installing Preml...'
$DO git clone git@github.com:mortenbp/PreML.git code/sml/preml 2>/dev/null
cd code/sml/preml
$DO git pull origin master
$DO make
make install
cd
echo 'DONE!'

# install shackl
echo 'Installing Shackl...'
$DO git clone git@github.com:mortenbp/shackl.git code/sml/shackl 2>/dev/null
cd code/sml/shackl
$DO git pull origin master
$DO make
make install
cd
$DO sh -c "echo \$\(SML_LIB\)/basis/basis.mlb > .shackl"
$DO sh -c "echo $HOME/code/sml/mylib/MyLib.mlb >> .shackl"
echo 'DONE!'

# install firmware-mod-kit
echo 'Installing Firmware Modkit..'
cd /opt
git clone https://code.google.com/p/firmware-mod-kit/
cd firmware-mod-kit/src
./configure
make
cd
echo 'DONE!'

# install binwalk
echo 'Installing Binwalk...'
cd /tmp
svn checkout http://binwalk.googlecode.com/svn/trunk/ binwalk
cd binwalk/src
./easy_install.sh
cd
rm -rf /tmp/binwalk
echo 'DONE!'

# install Botox
echo 'Installing Botox...'
cd /tmp
svn checkout http://botox.googlecode.com/svn/trunk/ botox
cd botox/src
./configure
make
cp botox /usr/local/bin
cd
rm -rf /tmp/botox
echo 'DONE!'

# install PEDA
echo 'Installing PEDA...'
$DO git clone git@github.com:mortenbp/peda.git projects/forks/peda 2>/dev/null
echo 'DONE!'

# modify sml-mode to work with do-notation
cd /usr/share/emacs23/site-lisp/sml-mode
# mv sml-defs.el sml-defs.el.bak
# cp $HOME/code/sml/preml/sml-defs.el .
# emacs --batch --eval '(byte-compile-file "sml-defs.el")'
cd

# byte compile Emacs' files
$DO emacs --batch --eval '(byte-recompile-directory "~/.emacs.d" 0)'

# configure slim
cp $CONF/slim.conf /etc

# "install" xfce4-suspend
ln -fsv $PWD/xfce4-suspend /usr/bin

# make GDB work properly
echo "kernel.yama.ptrace_scope = 0" > /etc/sysctl.d/10-ptrace.conf

# install graybird gtk theme
wget https://github.com/shimmerproject/Greybird/tarball/master -O theme
tar xfv theme -C /usr/share/themes
rm -vf theme

# make qt use gtk theme
cat > .config/Trolltech.conf <<EOF
[Qt]
style=GTK+
EOF

# install network configurations
cp $CONF/secret/system-connections/* /etc/NetworkManager/system-connections
cp -r $CONF/secret/certs /etc/openvpn

# reload wireless kernel module
modprobe -r iwlwifi ; modprobe iwlwifi

echo 'ALL DONE!'
