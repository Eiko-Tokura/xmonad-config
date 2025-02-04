# Eiko 的 XMonad 配置指南

本文档提供了 Eiko 自定义 XMonad 配置的概述，包括安装要求、按键绑定和使用说明。

## 引言

Eiko 的 XMonad 配置经过精心调整，以创建一个高效的工作环境，集成了各种工具和实用程序，以增强用户体验。我使用了一些自定义编写的Haskell模块以及来自 `xmonad-contrib` 的贡献插件。XMonad在可扩展这方面是非常强的，大家可以根据个人偏好进行修改。

### 安装脚本

我写了一个脚本，可以完成大部分工作owo，但你可能仍然需要手动配置以适应你的硬件和习惯。要安装此配置，请运行以下脚本：

```bash
git clone https://github.com/Eiko-Tokura/xmonad-config.git
cd xmonad-config
./install_asuka_xmonad.sh
```

### 安装要求

要使用此配置，请确保您在系统上安装以下命令行实用程序：

```bash
sudo pacman -S --needed kitty alacritty dmenu trayer picom fcitx dunst redshift feh flameshot bluedevil maim slock pavucontrol arandr htop yazi
```

如果您使用 Alacritty 并希望在 `yazi` 中查看图像预览，请安装 AUR 包：

```bash
paru -S ueberzugpp 
```

### 配置概述

配置文件可以分为几个关键部分：

- **全局变量**：更改常规设置，例如终端程序、工作区、壁纸路径和硬件管理设置。

- **按键绑定**：自定义启动应用程序、管理窗口和控制系统设置的快捷键。
  所有的键位都可以自行修改！owo

- **布局**：定义窗口布局及其在屏幕上的显示方式。

- **启动钩子**：在启动 XMonad 时执行的命令。

- **钩子**：与支持库（如 `ewmh` 和 `xmobar`）的事件处理和日志记录。

## 按键绑定

以下是配置中定义的一些基本按键绑定：

- **启动终端**：`Mod + Shift + Enter`

- **启动 Dmenu**：`Mod + P`
  Dmenu是一个快速检索和启动应用程序的小菜单owo

- **应用程序快捷菜单**：`Mod + Space`

- **命令行，启动！**：`Mod + Shift + Space`

- **切换工作区**：`Mod + [0-9]`

- **电源选项**：`Mod + Delete`
    - 关机：`P`
    - 休眠：`H`
    - 锁定：`L`
    - 重启：`R`
    - 待机: `S`
    - 注销：`O`

- **重新编译并重启 XMonad**：`Mod + F5`

### 工作区管理

- **切换工作区**：`Mod + [0-9]`

- **发送窗口到工作区**：`Mod + Shift + [0-9]`

- **切换到上一个非空工作区**：`Mod + Tab`

- **通过搜索窗口标题，拉取窗口到当前工作区**：`Mod + /`

### 窗口管理

- **导航窗口**： 
    - 下一个窗口：`Mod + J`
    - 上一个窗口：`Mod + K`
    - 聚焦主窗口：`Mod + M`
    - 移动到下一个Pane: `Mod + L`
    - 移动到上一个Pane: `Mod + H`

- **移动窗口**：
    - 交换主窗口：`Mod + Return`
    - 向下移动窗口：`Mod + Shift + J`
    - 向上移动窗口：`Mod + Shift + K`

- **关闭窗口**：`Mod + X` 或 `Mod + Q`

  我现在觉得`Mod + Q`有一点容易误触，但是它好像比`Mod + X`好按到，我不清楚要不要移除它owo

### 浮动窗口

- **拉出窗口**: `Mod + Shift + 鼠标左键拖动`

- **将浮动窗口推回平铺状态**：`Mod + T`

- **调整浮动窗口大小**：`Mod + 鼠标右键拖动`

### 布局管理

- **更改布局**：`Mod + Escape`
- **切换局部布局**: `Mod + U`
- **扩大主区域**：`Mod + Shift + L`
- **缩小主区域**：`Mod + Shift + H`
- **增加当前区域的窗口容量**：`Mod + ;`
- **减少当前区域的窗口容量**：`Mod + Shift + ;`

### 桌面背景

桌面背景是用`feh`设置的，而透明度是通过`picom`实现的。

- **更改壁纸**：`Mod + B`
- **增加透明度**：`Mod + O`
- **减少透明度**：`Mod + Shift + O`

### 媒体控制

- **播放/暂停音乐**：`Mod + Shift + P`
- **调整音量**： 
    - 增加：`键盘上的音量增加键`
    - 减少：`键盘上的音量减少键`

### 多显示器

- **切换显示器聚焦**: `Mod + W, E, R`
- **发送窗口到另一个显示器**：`Mod + Shift + W, E, R`

### 屏幕配置

你需要先修改配置文件中的显示器名称和菜单，不然你看到的是我的菜单owo

- **打开屏幕配置菜单**: `Mod + D`

### 屏幕亮度

注意，如果需要修改屏幕亮度，你可能需要额外的配置，包括添加用户到 video 组和修改 udev 规则。以下是两种方法：在配置文件中可以修改。

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

你可能还需要根据 `cat /sys/class/backlight/intel_backlight/max_brightness` 的输出调整以下值：

```haskell
-- adjust this value based on the output of `cat /sys/class/backlight/intel_backlight/max_brightness`
maxBrightness = 704
```

- **增加亮度**：`键盘上的亮度增加键`
- **减少亮度**：`键盘上的亮度减少键`

### 截图管理

- **全局截图**：`Mod + F12`
- **局部截图工具**：`Mod + Shift + A`

### 切换暗/亮模式

这只会切换配置文件中描述了的应用的主题，基本上是 `kitty` 和 `alacritty`，还有我的`neovim`配置。
你可以自行添加，如果你知道怎么修改你需要的应用的主题owo

- **切换到暗模式**：`Mod + Shift + F1`
- **切换到亮模式**：`Mod + Shift + F2`

## 布局

此配置包括多种窗口布局，例如：

- **Flex2Col**：灵活的两列布局
- **Flex3Col**：灵活的三列布局
- **Grid**：以网格模式排列窗口
- **Tabbed**：用于分组窗口的标签式布局

您可以在配置文件中修改或添加这些布局。

## 启动程序

以下程序在 XMonad 启动时启动：

- **Xmbarr**：用于显示系统信息的状态栏。
- **Picom**：窗口透明度的合成器。
- **Fcitx**：输入法框架。
- **Dunst**：通知守护进程。
- **Trayer**：系统托盘。

修改 `myStartupHook` 部分以添加或调整启动应用程序。

## 更改主题

您可以使用定义的函数更改环境的整体主题（暗/亮）：

- **切换到暗模式**：`switchToDarkMode`
  按 `Mod + Shift + F1`
- **切换到亮模式**：`switchToLightMode`
  按 `Mod + Shift + F2`

## 硬件管理

Eiko 的配置包括直接通过按键绑定管理触摸板设置和屏幕亮度的功能，使您能更轻松地随时调整设置。

## 结论

此配置旨在提供强大、可定制的 XMonad 窗口管理体验。请随意修改任何按键绑定、布局或启动应用程序，以满足您的工作流。祝编程愉快！
