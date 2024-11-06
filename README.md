# Eiko's XMonad Configuration Guide

This document provides an overview of Eiko's custom XMonad configuration, including installation requirements, key bindings, and usage instructions.

## Introduction

Eiko's XMonad configuration has been finely tuned to create an efficient working environment, integrating various tools and utilities to enhance the user experience. I use some custom-made Haskell modules along with contributions from `xmonad-contrib`. XMonad is highly extensible, allowing everyone to modify it according to personal preferences.

### Installation Using Script

I wrote a script that tries to do most of the work for you, but you still have to configure it manually to fit your hardware and your work style. To install this configuration, run the following script:

```bash
git clone https://github.com/Eiko-Tokura/xmonad-config.git
cd xmonad-config
./install_asuka_xmonad.sh
```

### Installation Requirements (For Manual Installation)

To use this configuration, ensure you have the following command-line utilities installed on your system:

```bash
sudo pacman -S --needed kitty alacritty dmenu trayer picom fcitx dunst redshift feh flameshot bluedevil maim slock pavucontrol arandr htop yazi
```

If you are using Alacritty and wish to view image previews in `yazi`, install the AUR package:

```bash
paru -S ueberzugpp 
```

### Configuration Overview

The configuration file can be divided into several key parts:

- **Global Variables**: Change general settings like terminal program, workspaces, wallpaper path, and hardware management settings.

- **Key Bindings**: Custom shortcuts to launch applications, manage windows, and control system settings. All key bindings can be modified!

- **Layouts**: Define how windows are arranged and displayed on the screen.

- **Startup Hooks**: Commands executed when XMonad starts.

- **Hooks**: Event handling and logging with supported libraries (like `ewmh` and `xmobar`).

## Key Bindings

Here are some basic key bindings defined in the configuration:

- **Launch Terminal**: `Mod + Shift + Enter`

- **Launch Dmenu**: `Mod + P`
  Dmenu is a small menu for quickly searching and launching applications.

- **Application Shortcut Menu**: `Mod + Space`

- **Command Line, Launch!**: `Mod + Shift + Space`

- **Switch Workspaces**: `Mod + [0-9]`

- **Power Options**: `Mod + Delete`
    - Shutdown: `P`
    - Hibernate: `H`
    - Lock: `L`
    - Restart: `R`
    - Suspend: `S`
    - Logout: `O`

- **Recompile and Restart XMonad**: `Mod + F5`

### Workspace Management

- **Switch Workspaces**: `Mod + [0-9]`

- **Send Window to Workspace**: `Mod + Shift + [0-9]`

- **Switch to the Previous Non-empty Workspace**: `Mod + Tab`

- **Pull Window to Current Workspace by Searching Window Title**: `Mod + /`

### Window Management

- **Navigate Windows**: 
    - Next Window: `Mod + J`
    - Previous Window: `Mod + K`
    - Focus Master Window: `Mod + M`
    - Toggle between Primary and Secondary Windows: `Mod + N`

- **Move Windows**:
    - Swap Master Window: `Mod + Return`
    - Move Window Down: `Mod + Shift + J`
    - Move Window Up: `Mod + Shift + K`

- **Close Window**: `Mod + X` or `Mod + Q`

  Currently, I feel `Mod + Q` is a bit easier to trigger accidentally, but it seems more accessible than `Mod + X`. I'm not sure whether to remove it.

### Floating Windows

- **Pull Out Window**: `Mod + Shift + Left Mouse Drag`

- **Push Floating Window Back to Tiling State**: `Mod + T`

- **Resize Floating Window**: `Mod + Right Mouse Drag`

### Layout Management

- **Change Layout**: `Mod + Escape`
- **Toggle Local Layout**: `Mod + U`
- **Expand Master Area**: `Mod + L`
- **Shrink Master Area**: `Mod + H`
- **Increase Current Area's Window Capacity**: `Mod + ;`
- **Decrease Current Area's Window Capacity**: `Mod + Shift + ;`

### Desktop Background

The desktop background is set with `feh`, and transparency is achieved using `picom`.

- **Change Wallpaper**: `Mod + B`
- **Increase Transparency**: `Mod + O`
- **Decrease Transparency**: `Mod + Shift + O`

### Media Control

- **Play/Pause Music**: `Mod + Shift + P`
- **Adjust Volume**: 
    - Increase: `Volume Up Key on Keyboard`
    - Decrease: `Volume Down Key on Keyboard`

### Multi-Monitor

- **Switch Display Focus**: `Mod + W, E, R`
- **Send Window to Another Monitor**: `Mod + Shift + W, E, R`

### Screen Configuration

You'll need to modify the names of the displays and menus in the configuration file; otherwise, you'll see my menu.

- **Open Screen Configuration Menu**: `Mod + D`

### Screen Brightness

To adjust screen brightness, you may need additional configuration, including adding your user to the video group and modifying udev rules. Here are two approaches to modify in the configuration file.

```haskell
  -- this method directly writes to the hardware
  -- you need to add yourself to the video group, by running 
  --    usermod -a -G video $USER
  -- and add a udev rule in /etc/udev/rules.d/backlight.rules that contains the following line:
  --    ACTION=="add", SUBSYSTEM=="backlight", RUN+="/bin/chgrp video $sys$devpath/brightness", RUN+="/bin/chmod g+w $sys$devpath/brightness"
  -- in order to use this method
  extraActions (IncreaseBrightness _) = fromFile read >>= \hs -> do
    spawn $ "echo " ++ show (brightnessConverter $ brightness hs) ++ " > /sys/class/backlight/intel_backlight/brightness"
    flashText (def {st_bg = "#9999FF"}) 0.3 ("Brightness " ++ show (brightnessConverter $ brightness hs))
  extraActions (DecreaseBrightness _) = fromFile read >>= \hs -> do
    spawn $ "echo " ++ show (brightnessConverter $ brightness hs) ++ " > /sys/class/backlight/intel_backlight/brightness"
    flashText (def {st_bg = "#9999FF"}) 0.3 ("Brightness " ++ show (brightnessConverter $ brightness hs))

  -- this is an alternative method to control backlight that uses xrandr
  -- extraActions (IncreaseBrightness _) = fromFile read >>= \hs -> spawn $ "xrandr --output eDP-1 --brightness " ++ show (fromRational (brightness hs) :: Double)
  -- extraActions (DecreaseBrightness _) = fromFile read >>= \hs -> spawn $ "xrandr --output eDP-1 --brightness " ++ show (fromRational (brightness hs) :: Double)
```

You may also need to adjust the following value based on the output of `cat /sys/class/backlight/intel_backlight/max_brightness`:

```haskell
-- adjust this value based on the output of `cat /sys/class/backlight/intel_backlight/max_brightness`
maxBrightness = 704
```

- **Increase Brightness**: `Brightness Up Key on Keyboard`
- **Decrease Brightness**: `Brightness Down Key on Keyboard`

### Screenshot Management

- **Global Screenshot**: `Mod + F12`
- **Local Screenshot Tool**: `Mod + Shift + A`

### Toggle Dark/Light Mode

This will only toggle the themes defined in the configuration file for applications like `kitty`, `alacritty`, and my `neovim` setup. You can add more if you know how to modify the themes for your required applications.

- **Switch to Dark Mode**: `Mod + Shift + F1`
- **Switch to Light Mode**: `Mod + Shift + F2`

## Layout

This configuration includes various window layouts, such as:

- **Flex2Col**: A flexible two-column layout
- **Flex3Col**: A flexible three-column layout
- **Grid**: Arranging windows in a grid pattern
- **Tabbed**: Tabbed layout for grouping windows

You can modify or add these layouts in the configuration file.

## Startup Programs

The following programs are launched when XMonad starts:

- **Xmbarr**: Status bar for displaying system information.
- **Picom**: Compositor for window transparency.
- **Fcitx**: Input method framework.
- **Dunst**: Notification daemon.
- **Trayer**: System tray.

Modify the `myStartupHook` section to add or adjust the startup applications.

## Theme Change

You can switch the overall theme (dark/light) of the environment using defined functions:

- **Switch to Dark Mode**: `switchToDarkMode`
  Press `Mod + Shift + F1`
- **Switch to Light Mode**: `switchToLightMode`
  Press `Mod + Shift + F2`

## Hardware Management

Eiko's configuration includes the ability to manage touchpad settings and screen brightness directly through key bindings, allowing you to adjust settings easily at any time.

## Conclusion

This configuration aims to provide a powerful, customizable XMonad window management experience. Feel free to modify any key bindings, layouts, or startup applications to suit your workflow. Happy coding!
