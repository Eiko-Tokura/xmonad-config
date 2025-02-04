{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
import XMonad -- hiding ((|||))
import XMonad.Actions.WindowBringer
import XMonad.Actions.ShowText
import XMonad.Actions.Submap
import XMonad.Actions.CycleRecentWS
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad
import XMonad.Util.Timer
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Run(spawnPipe, hPutStrLn)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.WindowSwallowing
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.RefocusLast
import XMonad.Hooks.ManageHelpers(isDialog)
import XMonad.Layout.LayoutHints
import XMonad.Layout.Renamed
import XMonad.Layout.Accordion
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.LayoutBuilder
import TallMastersCombo as TMC
import XMonad.Layout.Master
import XMonad.Layout.Combo
import Control.Monad (when, void)
import Data.Monoid
import Data.List (isSuffixOf)
import Data.Word(Word32)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.ExtraTypes.XF86
import System.FilePath ((</>))

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- | This is Eiko's xmonad configuration file.
-- It is a combination of the default xmonad configuration file and the configuration file of the xmonad-contrib package, together with certain custom modules created by Eiko.
-- here are all command line utilities used in this module, install them before running xmonad:
--   kitty/alacritty   dmenu    trayer
--   picom             fcitx    dunst
--   redshift          feh      xrandr
--   xprop             xinput   flameshot
--   bluedevil         light    pactl
--   maim              slock    pavucontrol
--   arandr            htop     yazi
--
-- Install them with the following command:
-- sudo pacman -S --needed kitty alacritty dmenu trayer picom fcitx dunst redshift feh flameshot bluedevil maim slock pavucontrol arandr htop yazi
--
-- if you use alacritty and wants pictures previews in yazi to display correctly you need the following aur package
-- paru -S ueberzugpp 

-- my own modules, inside .xmonad/lib
import ChangeConfig -- this module allows you to change the configuration of your software easily and systematically
import ViewDoc      -- if you want to use this feature to save opened documents, visit https://github.com/Eiko-Tokura/xmonad-session-saver

import System.Exit
import System.IO

---------------------------------------------------------------------------------------------
-- The global variables that are used in this configuration file, you can change them here

-- The preferred terminal program, which is used in a binding below and by certain contrib modules.
myTerminal      = "alacritty"
terminalExecutionFlag = " -e "

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth   = 3

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
-- | You can actually change their names by changing the strings in the list
myWorkspaces    = ["1","2","3","4","5","6","7","8","9","0"] -- ++ ["F1", "F2", "F3", "F4"]
-- | These are the keys corresponding to the workspaces listed above
myWorkspaceKeys = [xK_1 .. xK_9] ++ [xK_0] -- ++ [xK_F1 .. xK_F4]

-- Home path, should be changed to your home path
home :: String
home = "/home/eiko/"

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor  = "#efefef"
myFocusedBorderColor = "#66aaff"

-- path to find background pictures
bgPath = "~/Pictures/Saved\\ Pictures/ComputerBG/"

-- your phone bluetooth id for convienient sendfile
myphone = "bluetooth://E4-84-D3-94-46-44" 

-- The default screen, can be seen by running `xrandr`. Will be used to adjust the resolution, refresh rate, and brightness of the screen
defaultDisplay = "eDP-1"
defaultDisplaySetting = "xrandr --output " ++ defaultDisplay ++ " --mode 3120x2080 --rate 60"
modesAndRates = 
  [ ("3120x2080", "60")
  , ("3120x2080", "120")
  , ("2560x1600", "59.99")
  ]

-- adjust this value based on the output of `cat /sys/class/backlight/intel_backlight/max_brightness`
maxBrightness = 704

-- the directory to store your sceenshots, make sure the directory exists
scDir = "~/Pictures/SC" 

-- the commandline for taking a global screenshot
takeSC =  spawn ("maim " ++ scDir </> "$(date +%s).png") 
       -- >> threadDelay 0.1 >> flashText (def {st_bg = "#9999FF"}) 0.1 ">w<" -- a rapid flash to indicate the screenshot is taken

--take screenshot and send to phone
takeAndSendSCBlueTooth device = spawn $ "maim " ++ (scDir </> "$(date +%s).png") ++ "; cd " ++ scDir ++ "; ls -t | head -n 1 | xargs -I {} bluedevil-sendfile -k " ++ device ++ " -f \"$PWD/{}\"" 

-- increase the brightness of your screen, deprecated method
lightUp am   = spawn $ "light -s sysfs/backlight/intel_backlight -A " ++ am 
lightDown am = spawn $ "light -s sysfs/backlight/intel_backlight -U " ++ am

-- this command will fetch touchpad id automatically
getTouchPadId = "xinput list | grep 'Touchpad' | awk '{print $5} {print $6}' | grep '=' | awk -F= '{print $2}' | xargs -I {} " 

-- myRaiseVolume am = spawn $ "amixer -q sset Speaker " ++ am ++ "%+"
-- myLowerVolume am = spawn $ "amixer -q sset Speaker " ++ am ++ "%-"

myRaiseVolume am = spawn $ "pactl set-sink-volume @DEFAULT_SINK@ +" ++ am ++ "%"
myLowerVolume am = spawn $ "pactl set-sink-volume @DEFAULT_SINK@ -" ++ am ++ "%"

xmobarColorStr = xmobarColorLight
xmobarColorLight = "CC8899"
xmobarColorDark = "000000"

-----------------------------------------------------------------------
-- You can use the ChangeConfig module to change the configuration of your software easily and systematically

-- Change the theme of xmobar
data XMobar
instance HasConfigFile XMobar where configPath = home ++ ".xmobarrc"
instance Configurable XMobar where
  data Mode XMobar = XMobarDark | XMobarLight
  modeSetter XMobarDark  = setConfigLineKey "bgColor" ("\"#" ++ xmobarColorDark ++ "\"")  . setConfigLineKey "borderColor" ("\"#" ++ xmobarColorDark ++ "\"")
  modeSetter XMobarLight = setConfigLineKey "bgColor" ("\"#" ++ xmobarColorLight ++ "\"") . setConfigLineKey "borderColor" ("\"#" ++ xmobarColorLight ++ "\"")

-- Change the theme of xmonad
data XMonadItself
instance HasConfigFile XMonadItself where configPath = home ++ ".xmonad/xmonad.hs"
instance Configurable XMonadItself where
  data Mode XMonadItself = XMonadDark | XMonadLight 
  modeSetter XMonadDark  = setConfigLineKey "myNormalBorderColor" "\"#121212\""
                         . setConfigLineKey "xmobarColorStr" "xmobarColorDark"
  modeSetter XMonadLight = setConfigLineKey "myNormalBorderColor" "\"#efefef\""
                         . setConfigLineKey "xmobarColorStr" "xmobarColorLight"

-- Manage the hardware status of your computer, in this case the touchpad and the screen brightness
brightnessConverter 0 = 0
brightnessConverter x = round $ exp (6 * (fromRational x - 1)) * maxBrightness

data HardwareManager = HardwareManager { touchpadEnabled :: Bool, brightness :: Rational } deriving (Show, Read)
instance HasConfigFile HardwareManager where 
  configPath = home ++ ".xmonad/hardwareStatus"
  defaultConfig = Just $ show $ HardwareManager True 0.5
instance Configurable HardwareManager where
  data Mode HardwareManager = ToggleTouchPad | IncreaseBrightness Rational | DecreaseBrightness Rational 
  type MonadOf HardwareManager = X
  modeSetter ToggleTouchPad = modifyHSData $ \hs -> hs { touchpadEnabled = not $ touchpadEnabled hs }
  modeSetter (IncreaseBrightness x) = modifyHSData $ \hs -> hs { brightness = min 1 $ brightness hs + x }
  modeSetter (DecreaseBrightness x) = modifyHSData $ \hs -> hs { brightness = max 0 $ brightness hs - x }
  extraActions ToggleTouchPad = fromFile read >>= \hs -> if touchpadEnabled hs
    then spawn (getTouchPadId ++ "xinput enable  {}") >> flashText (def {st_bg = "#99FF99"}) 0.5 "Touchpad Enabled"
    else spawn (getTouchPadId ++ "xinput disable {}") >> flashText (def {st_bg = "#FF9999"}) 0.5 "Touchpad Disabled"
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

data Alacritty
instance HasConfigFile Alacritty where 
  configPath = home ++ ".config/alacritty/alacritty.toml"
  defaultConfig = Just $ unlines
    [ "[colors.primary]"
    , "background = \"0xFFFFFF\""
    , "foreground = \"0x000000\""
    , "[font]"
    , "size = 14"
    ]
instance Configurable Alacritty where
  data Mode Alacritty = AlacrittyDark | AlacrittyLight
  modeSetter AlacrittyDark  = setConfigLineKey "background" "\"0x000000\"" . setConfigLineKey "foreground" "\"0xFFFFFF\""
  modeSetter AlacrittyLight = setConfigLineKey "background" "\"0xFFFFFF\"" . setConfigLineKey "foreground" "\"0x000000\""

-- Change the theme of your terminal
data Kitty
instance HasConfigFile Kitty where configPath = home ++ ".config/kitty/kitty.conf"
instance Configurable Kitty where
  data Mode Kitty = KittyDark | KittyLight 
  modeSetter KittyDark  = setConfigLineKey "background" "#000000" . setConfigLineKey "foreground" "#FFFFFF"
  modeSetter KittyLight = setConfigLineKey "background" "#FFFFFF" . setConfigLineKey "foreground" "#000000"

-- Change the opacity of your desktop
data Picom
instance HasConfigFile Picom where 
  configPath = home ++ ".config/picom/picom.conf"
  defaultConfig = Just $ unlines -- if the file does not exist, it will create a default config file
    [ "inactive-opacity = 0.8"
    , "frame-opacity = 0.8"
    , "active-opacity = 0.8"
    , "backend = \"xrender\""
    ]
instance Configurable Picom where
  data Mode Picom = PicomOpacityInc Rational | PicomOpacityDec Rational deriving Show
  modeSetter (PicomOpacityInc delta) = foldr1 (.) 
    [ modifyConfigLineKeyType @Double prop (min 1 . (+ fromRational delta))
    | prop <- ["inactive-opacity", "frame-opacity", "active-opacity"]
    ]
  modeSetter (PicomOpacityDec delta) = foldr1 (.) 
    [ modifyConfigLineKeyType @Double prop (max 0.7 . subtract (fromRational delta))
    | prop <- ["inactive-opacity", "frame-opacity", "active-opacity"]
    ]

-- Change the theme of zathura
data Zathura
instance HasConfigFile Zathura where 
  configPath = home ++ ".config/zathura/zathurarc" 
  defaultConfig = Just $ unlines
    [ "set recolor false"
    , "set guioptions none"
    , "set adjust-open \"width\""
    , "set selection-clipboard clipboard"
    ]
instance Configurable Zathura where
  data Mode Zathura = ZathuraDark | ZathuraLight
  modeSetter ZathuraDark  = setConfigLineKey "set recolor" "true"
  modeSetter ZathuraLight = setConfigLineKey "set recolor" "false"

-- Change the theme of neovim
data Nvim
instance HasConfigFile Nvim where configPath = home ++ ".config/nvim/plugin/plugins.vim"
instance Configurable Nvim where
  data Mode Nvim = NvimDark | NvimLight
  modeSetter NvimDark  = setConfigLineKey "let g:isLightMode" "0"
  modeSetter NvimLight = setConfigLineKey "let g:isLightMode" "1"

switchToDarkMode =  xSetConfig KittyDark   >> xSetConfig XMonadDark >> xSetConfig XMobarDark
                 >> xSetConfig ZathuraDark >> xSetConfig NvimDark >> xSetConfig AlacrittyDark
                 >> xmonadRecompileRestart >> kittyReload

switchToLightMode = xSetConfig KittyLight   >> xSetConfig XMonadLight >> xSetConfig XMobarLight
                 >> xSetConfig ZathuraLight >> xSetConfig NvimLight >> xSetConfig AlacrittyLight
                 >> xmonadRecompileRestart  >> kittyReload

takeLocalSC = spawn "flameshot gui" -- take Local SC

kittyReload = spawn "kill -s USR1 $(pgrep -f kitty)" -- reload kitty config
xmonadRecompileRestart = spawn "xmonad --recompile; xmonad --restart"
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")

    -- launch a menu that allows me to easily switch available resolutions and refresh rates, you can view the available resolutions and refresh rates by running `xrandr`
    , ((modm,               xK_d     ), visualSubmap def . M.fromList . map (\(k, (mode, rate)) -> (k, (mode ++ " " ++ rate ++ "Hz", spawn $ "xrandr --output " ++ defaultDisplay ++ " --mode " ++ mode ++ " --rate " ++ rate))) 
        $ zip (zip (repeat 0) [xK_1 .. xK_9]) modesAndRates
      )

    -- launch Softwares in a submap
    , ((modm,               xK_space), visualSubmap def . M.fromList . map (\(k, s) -> (k, (s, spawn s))) $
        [ ((0, xK_a), "arandr")
        , ((0, xK_b), myTerminal ++ terminalExecutionFlag ++ " btop")
        , ((0, xK_d), "dolphin")
        , ((0, xK_f), "floorp")
        , ((0, xK_h), myTerminal ++ terminalExecutionFlag ++ " htop")
        , ((0, xK_k), myTerminal)
        , ((0, xK_p), "pavucontrol")
        , ((0, xK_y), myTerminal ++ terminalExecutionFlag ++ " yazi")
        , ((0, xK_s), myTerminal ++ terminalExecutionFlag ++ " sudo pacman -Syu")
        , ((0, xK_x), myTerminal ++ terminalExecutionFlag ++ " nvim ~/.xmonad/xmonad.hs")
        ])

    -- power options 
    , ((modm,               xK_Delete), visualSubmap def . M.fromList . map (\(k, hint, job) -> (k, (hint, job))) $      
        [ ((0, xK_p), "poweroff",  spawn "poweroff")
        , ((0, xK_s), "suspend",   spawn "systemctl suspend")
        , ((0, xK_h), "hibernate", spawn "systemctl hibernate")
        , ((0, xK_l), "lock",      spawn "slock")
        , ((0, xK_r), "reboot",    spawn "reboot")
        , ((0, xK_o), "logout xmonad", io exitSuccess)
        ])

    -- play-pause, control the playing of music
    , ((modm .|. shiftMask, xK_p     ), spawn "quodlibet --play-pause &")

    -- set new bg
    , ((modm,               xK_b     ), setANewBG)

    -- set windows transparency (decresing)
    , ((modm,               xK_o     ), xSetConfig $ PicomOpacityDec 0.05)

    -- set windows transparency (increasing)
    , ((modm .|. shiftMask, xK_o     ), xSetConfig $ PicomOpacityInc 0.05)

    -- close the focused window
    , ((modm , xK_x     ), kill)
    , ((modm , xK_q     ), kill)

    -- Rotate through the available layout algorithms
    , ((modm,               xK_Escape ), sendMessage NextLayout)

    -- Change next layout for the focused window
    , ((modm,               xK_u     ), sendMessage FocusedNextLayout)
    --
    -- Move focus to the next main window
    , ((modm,               xK_l     ), sendMessage NextFocus)

    -- Move focus to the previous main window
    , ((modm,               xK_h     ), sendMessage PrevFocus)

    -- Move focus to the recent workspace
    , ((modm,               xK_Tab   ), toggleRecentNonEmptyWS)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Move focus to the last focused window
    , ((modm,               xK_comma ), toggleFocus )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp )

    -- Shrink the master area
    , ((modm .|. shiftMask, xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm .|. shiftMask, xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- open todo list
    , ((modm .|. shiftMask, xK_t     ), spawn "nvim-qt ~/.todo/todo.txt")

    -- Spawn a scratch pad
    --, ((modm,               xK_s     ), scratchpadSpawnAction conf)

    -- Launch a BringMenu that allows you to bring windows to the current workspace
    , ((modm ,            xK_slash     ), bringMenu )
    
    -- Save sessions, visit http://github.com/Eiko-Tokura/xmonad-session-saver for more information
    , ((modm              , xK_s     ), toggleSaveState)
    , ((modm .|. shiftMask, xK_s     ), launchDocuments)
    
    -- hibernate
    , ((modm .|. shiftMask, xK_z     ), spawn "loginctl hibernate" )

    -- Increment the number of windows in the master area
    , ((modm              , xK_semicolon), sendMessage (IncMasterN 1)) -- >> sendMessage (IncLayoutN 1))

    -- Deincrement the number of windows in the master area
    , ((modm .|. shiftMask, xK_semicolon ), sendMessage (IncMasterN (-1))) -- >> sendMessage (IncLayoutN (-1)))
  
    , ((modm, xK_v), sendMessage SwitchOrientation)

    -- Toggle the status bar gap, 'fullscreen' mode
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    , ((modm              , xK_F11     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_c     ), io exitSuccess)

    -- (recompile and) Restart xmonad
    , ((modm              , xK_F5     ), xmonadRecompileRestart)

    -- Toggle touchpad disabled/enabled
    , ((modm              , xK_F9     ), setConfig ToggleTouchPad)

    -- Global switch between dark and light mode
    , ((modm .|. shiftMask , xK_F1     ), switchToDarkMode)
    , ((modm .|. shiftMask , xK_F2     ), switchToLightMode)

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    --, ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), flashText (def {st_bg = myFocusedBorderColor}) 0.5 i >> windows (f i))
        | (i, k) <- zip (XMonad.workspaces conf) myWorkspaceKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

    ++
    [
        -- Adjust screen brightness
        ((0, xF86XK_MonBrightnessUp), setConfig $ IncreaseBrightness 0.05), --lightUp "5"),
        --((modm, xK_F3), lightUp "1"),
        --((controlMask, xK_F3), lightUp "0.1"),
        ((0, xF86XK_MonBrightnessDown), setConfig $ DecreaseBrightness 0.05),-- lightDown "5"),
        --((modm, xK_F2), lightDown "1"),
        --((controlMask, xK_F2), lightDown "0.1"),
        -- Adjust system volume
        ((0, xF86XK_AudioRaiseVolume), void $ myRaiseVolume "4"),
        ((0, xF86XK_AudioLowerVolume), void $ myLowerVolume "4")
    ]
    ++
    [
        ((modm, xK_F12), takeSC)
      , ((modm .|. shiftMask, xK_F12), takeAndSendSCBlueTooth myphone)
      , ((modm .|. shiftMask, xK_a), takeLocalSC)
    ]

--myScratchPads :: [NamedScratchpad]
--myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
--                ]
--  where
--    spawnTerm  = myTerminal ++ " -t scratchpad"
--    findTerm   = title =? "scratchpad"
--    manageTerm = customFloating $ W.RationalRect l t w h
--               where
--                 h = 0.9
--                 w = 0.9
--                 t = 0.95 -h
--                 l = 0.95 -w
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm .|. shiftMask, button1), \w -> focus w >> mouseMoveWindow w   
                                       >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w  >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w  >> mouseResizeWindow w
                                       >> windows W.shiftMaster)

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]


------------------------------------------------------------------------
-- Layouts:
myLayout  =   flex2Col
          ||| flex3Col
          ||| myGrid
          ||| Full
  where
     layoutSet  = myGrid |||| Accordion |||| myTabbed
     flex2Col   = nameLayout "Flex2Col"   $ tmsCombineTwo True 1 delta (1/2) layoutSet layoutSet
     myGrid     = nameLayout "GridR"      $ GridRatio 1.3
     -- gridAccord = nameLayout "GridAccord" $ myGrid `flexCombineV` Accordion 
     -- gridGrid   = nameLayout "GridGrid"   $ myGrid `flexCombineV` myGrid 
     -- gridTabbed = nameLayout "GridTabbed" $ myGrid `flexCombineV` myTabbed 
     -- threeCol   = ThreeColMid nmaster delta 0.45
     flex3Col   = nameLayout "Flex3Col"   
        $ tmsCombineTwo True 1 delta (1/3)
          layoutSet
          (tmsCombineTwo True 1 delta (1/2) layoutSet layoutSet)
                                    
     -- tiled      = Tall nmaster delta ratio

     myTabbed = tabbed shrinkText def { activeColor = myFocusedBorderColor
                                      , activeBorderColor = "#FFFFFF"
                                      , activeTextColor = "#FFFFFF"
                                      , inactiveBorderColor = myFocusedBorderColor
                                      , inactiveColor = myNormalBorderColor--"#FFFFFF"
                                      , inactiveTextColor = myFocusedBorderColor
                                      }

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 1/34

     nameLayout string = renamed [Replace string]
     --combineLayouts ratio lay1 lay2 = layoutN 1 (relBox 0 0 ratio 1) Nothing lay1 (layoutAll (relBox (1-ratio) 0 1 1) lay2)
     --evenCombine lay1 lay2 = combineLayouts 0.5 lay1 lay2

     flexCombineV :: (a ~ Window, LayoutClass l1 a, LayoutClass l2 a) => l1 a -> l2 a -> TMSCombineTwo l1 l2 a
     flexCombineV = TMC.tmsCombineTwo True 1 delta ratio 

     flexCombineH :: (a ~ Window, LayoutClass l1 a, LayoutClass l2 a) => l1 a -> l2 a -> TMSCombineTwo l1 l2 a
     flexCombineH = TMC.tmsCombineTwo False 1 delta ratio 


instance TMC.GetFocused Accordion Window

------------------------------------------------------------------------
-- use these functions:
-- runQuery :: Query a -> Window -> X a
-- withWindowSet :: (WindowSet -> X a) -> X a
------------------------------------------------------------------------
-- Window rules:

-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ isDialog                              --> doFloat     -- float dialog windows
    , manageDocks
    , title     =? "图片查看器"             --> doFloat     -- used by QQ
    , title     =? "视频播放器"             --> doFloat     -- used by QQ
    , title =? "Media viewer"               --> doFloat     -- used by Telegram
    , ("的聊天记录" `isSuffixOf`) <$> title --> doFloat     -- used by QQ
    , className =? "Quodlibet"              --> doShift "0" -- music player
    , className =? "thunderbird"            --> doShift "0" -- email client
    , resource  =? "desktop_window"         --> doIgnore
    , resource  =? "kdesktop"               --> doIgnore
    ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
myEventHook = composeAll
  [ swallowEventHook  -- Swallow parent if child processes are running
      (className =? myTerminal <||> className =? "Anki")  --swallow only happens for terminal and anki windows
      (not <$> willFloat <&&> not <$> className =? "Zathura" <&&> not <$> title =? "ghc-vis")  --do not swallow floating windows
  , handleTimerEvent
--, handleBGTimerEvent
  ]

--
--newtype MyXState = MyXState { timerId :: Maybe TimerId }
--
--instance ExtensionClass MyXState where
--  initialValue = MyXState Nothing
--
-- handleBGTimerEvent :: Event -> X All
-- handleBGTimerEvent e = do
--   MyXState mtid <- XS.get
--   case mtid of
--     Just tid -> do
--       handleTimer tid e $ do
--         setANewBG
--         startBGTimer
--         return Nothing
--       return (All True)
--     Nothing -> return (All True)
--
--handleBGTimerEvent :: Event -> X All
--handleBGTimerEvent (ClientMessageEvent _ _ _ dis _ mtyp d) = do
--    a <- io $ internAtom dis "XMONAD_TIMER" False
--    myState <- XS.get :: X MyXState
--    when (mtyp == a && (== Just (fromIntegral (head d))) (timerId myState)) $ do
--        setANewBG
--        startBGTimer
--    mempty
--
--handleBGTimerEvent _ = mempty
--
--startBGTimer :: X ()
--startBGTimer = do
--  tid <- startTimer 60
--  XS.put $ MyXState (Just tid)
--  mempty
--
--myEventHookWithTimer tid regenerateTimer event =
--  handleTimer tid event $ do
--    setANewBG
--    tid' <- regenerateTimer
--    myEventHookWithTimer tid' regenerateTimer event
------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = colorSaved 

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
myStartupHook = do
    --startUpCleanUp      -- Clean .viewedDocs
    setConfig $ IncreaseBrightness 0 -- set the brightness to the default value
    --spawnOnce defaultDisplaySetting -- set the default resolution
    spawn "killall trayer" -- kill trayer
    spawn ("sleep 2 && trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 " ++ "--tint 0x" ++ xmobarColorStr ++ " --height 17") -- restart trayer
    setANewBGOnce --set a bg! owo
    spawnOnce $ "picom --config \"" ++ configPath @Picom ++ "\" &" -- transparent compositor
    spawnOnce "fcitx &" -- input method
    spawnOnce "dunst &" -- dunst
    --spawnOnce "redshift -P -O 5000" -- redshift, you can change the screen temparature here
    spawnOnOnce "0" "quodlibet &" -- launch music player
    --spawnOnOnce "0" "thunderbird" -- launch email client
    --when laptop $ spawn $ setTouchPad "libinput Natural Scrolling Enabled" "1"
    --when laptop $ spawn $ setTouchPad "libinput Tapping Enabled"           "1"
    replaceCapslockToEscape -- replace Caps to ESC
    -- startBGTimer --detect-client-opacity 
        where
          setTouchPad str val = getTouchPadId ++ "xinput set-prop " ++ touchpadId ++ " '" ++ str ++ "' " ++ val
          touchpadId = "{}"
          laptop = True

setNewBGsString = unlines --requires feh
  [ "WALLPAPER_DIR=" ++ bgPath 
  , "NUM_MONITORS=$(xrandr --query | grep ' connected' | wc -l)"
  , "WALLPAPERS=()"
  , "for i in $(seq 1 $NUM_MONITORS); do"
  , "    WALLPAPERS+=(\"$(find \"$WALLPAPER_DIR\" -type f -print0 | shuf -n1 -z | xargs -0 echo)\")"
  , "done"
  , "feh --bg-fill \"${WALLPAPERS[@]}\""
  ]

setANewBGOnce = spawnOnce setNewBGsString 
setANewBG = spawn setNewBGsString 

replaceCapslockToEscape = spawn "setxkbmap -option caps:escape"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks . ewmh $ defaults
    { manageHook = manageHook defaults
    , layoutHook = avoidStruts $ layoutHook defaults
    , logHook = colorSaved <> refocusLastLogHook <> do
          dynamicLogWithPP xmobarPP
                  { ppOutput = hPutStrLn xmproc
                  , ppTitle = xmobarColor "#99FFFF" "" . shorten 60
                  , ppCurrent = wrap "[" "]"
                  , ppOrder = \(ws:lo:t:ex) -> t:lo:ws:ex
                  , ppHiddenNoWindows = const "-"
                  , ppSep = " "
                  }
    --, handleEventHook = <> handleEventHook defaults
    }
    --`additionalKeys`
    --[ (
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def 
      -- simple stuff
      { terminal           = myTerminal
      , focusFollowsMouse  = myFocusFollowsMouse
      , clickJustFocuses   = myClickJustFocuses
      , borderWidth        = myBorderWidth
      , modMask            = myModMask
      , workspaces         = myWorkspaces
      , normalBorderColor  = myNormalBorderColor
      , focusedBorderColor = myFocusedBorderColor

      -- key bindings
      , keys               = myKeys
      , mouseBindings      = myMouseBindings

      -- hooks, layouts
      , layoutHook         = myLayout
      , manageHook         = myManageHook
      , handleEventHook    = myEventHook 
      , logHook            = myLogHook
      , startupHook        = myStartupHook
      }
