echo "[Asuka]: This is Asuka's XMonad installation script. It will install XMonad, XMobar, and other dependencies. It will also copy the configuration files to the correct directories. This script is intended for Arch Linux. Do you want to continue? [Y/n]"
read -r confirm
if [ "$confirm" != "y" ]; then
    echo "[Asuka]: Exiting..."
    exit 1
fi

echo "[Asuka]: Creating .xmonad directory"
mkdir ~/.xmonad

echo "[Asuka]: Customizing XMonad configuration"
XMONAD_HS_FILE="xmonad.hs"
# Use sed to find and replace the line starting with 'home = "..."'
# The updated line will look like 'home = "/actual/absolute/path"'
sed -i.bak "s|^home = \".*\"|home = \"$(echo $HOME)/\"|" "$XMONAD_HS_FILE"

echo "[Asuka]: Do you want XMonad to adjust your screen brightness? This can be useful if you are using a laptop. XMonad will add you to the video group and add a udev rule to allow video group to edit brightness. It might not be necessary if you are using a desktop computer. [y/N]"
read -r adjust_brightness
if [ "$adjust_brightness" = "y" ]; then
    sudo usermod -aG video $USER
    echo "ACTION==\"add\", SUBSYSTEM==\"backlight\", RUN+=\"/bin/chgrp video \$sys\$devpath/brightness\", RUN+=\"/bin/chmod g+w \$sys\$devpath/brightness\"" | sudo tee /etc/udev/rules.d/backlight.rules
    if [ -e "/sys/class/backlight/intel_backlight/max_brightness" ]; then
        echo "[Asuka]: Found intel_backlight, using it to set maxBrightness"
        maxBrightness=$(cat /sys/class/backlight/intel_backlight/max_brightness)
        sed -i "s|^maxBrightness = .*|maxBrightness = $maxBrightness|" "$XMONAD_HS_FILE"
    else
        echo "[Asuka]: WARNING: please set maxBrightness manually in the configuration file, probably somewhere in /sys/class/backlight/"
    fi
fi


echo "[Asuka]: Copying configuration files"
cp $XMONAD_HS_FILE ~/.xmonad/
cp -r lib ~/.xmonad/

echo "[Asuka]: Copying XMobar configuration files"
cp xmobar/.xmobarrc ~/.xmobarrc
mkdir -p ~/.config/xmobar
cp xmobar/trayer-padding-icon.sh ~/.config/xmobar/trayer-padding-icon.sh

echo "[Asuka]: Installing dependencies"
sudo pacman -S --needed git kitty alacritty dmenu trayer picom fcitx dunst redshift feh flameshot bluedevil maim slock pavucontrol arandr htop yazi sddm

echo "[Asuka]: Do you want to install stack using pacman? If you have already installed stack via other methods, you should skip this step. Otherwise please let me help you install it owo [Y/n]"
read -r install_stack
if [ "$install_stack" = "y" ]; then
    sudo pacman -S stack
fi

echo "[Asuka]: Cloning XMonad and XMonad Contrib from github repository"
cd ~/.xmonad
git clone https://github.com/xmonad/xmonad
git clone https://github.com/xmonad/xmonad-contrib

if [ $! -ne 0 ]; then
    echo "[Asuka]: Failed to clone XMonad and XMonad Contrib. You might not have internet connection or you need magical network environment."
    exit 1
fi

echo "[Asuka]: Setup XMonad using stack"
stack init

echo "[Asuka]: Building XMonad"
stack install

echo "[Asuka]: Building XMobar"
stack install xmobar

echo "[Asuka]: Writing XMonad desktop entry"
echo "[Desktop Entry]
Name=XMonad
Comment=Lightweight tiling window manager
Exec=/home/$USER/.local/bin/xmonad
Type=Application
" > xmonad.desktop

echo "[Asuka]: Copying XMonad desktop entry to /usr/share/xsessions"
sudo cp xmonad.desktop /usr/share/xsessions/xmonad.desktop

echo "[Asuka]: Do you want to enable sddm? [Y/n]"
read -r enable_sddm
if [ "$enable_sddm" != "n" ]; then
    sudo systemctl enable sddm
fi

echo "[Asuka]: There might still be some manual configurations you need to / can do owo. 
For example adding some background pictures to ~/Pictures/Saved Pictures/ComputerBG/ (you can change the path in the configuration file).

You can find them in the configuration files. The configuration files are located in ~/.xmonad, the main file to confugure is ~/.xmonad/xmonad.hs. 

The xmobar configuration file is located in ~/.config/xmobar. You can also find the XMonad documentation at https://xmonad.org/documentation.html. Enjoy your XMonad experience!

Done! Please reboot your system or restart your display manager to use XMonad."