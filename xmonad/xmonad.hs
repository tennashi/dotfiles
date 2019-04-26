import qualified Data.Map as M
import Control.Monad (liftM2)
import Data.Monoid
import System.IO

import XMonad
-- import qualified XMonad.StackSet as W

-- import XMonad.Actions.CopyWindow
-- import XMonad.Actions.CycleWS
import qualified XMonad.Actions.FlexibleResize as Flex
-- import XMonad.Actions.FloatKeys
-- import XMonad.Actions.UpdatePointer
-- import XMonad.Actions.WindowGo

import qualified XMonad.Hooks.DynamicLog as DL
import qualified XMonad.Hooks.ManageDocks as MD
import qualified XMonad.Hooks.ManageHelpers as MH
import qualified XMonad.Hooks.EwmhDesktops as EWMH

import XMonad.Layout
-- import XMonad.Layout.DragPane
import qualified XMonad.Layout.Gaps as Ga
-- import XMonad.Layout.LayoutScreens
import qualified XMonad.Layout.NoBorders as NB
import qualified XMonad.Layout.PerWorkspace as PW
-- import XMonad.Layout.ResizableTile
import qualified XMonad.Layout.Simplest as Si
import qualified XMonad.Layout.SimplestFloat as SF
import qualified XMonad.Layout.Spacing as Sp
import qualified XMonad.Layout.ToggleLayouts as TL
-- import XMonad.Layout.TwoPane
import qualified XMonad.Layout.BinarySpacePartition as BSP

-- import XMonad.Prompt
-- import XMonad.Prompt.Window
import qualified XMonad.Util.EZConfig as EZC
-- import XMonad.Util.Run
-- import XMonad.Util.Run(spawnPipe)
import qualified XMonad.Util.SpawnOnce as SO
import qualified XMonad.Config.Gnome as Gn

import qualified Graphics.X11.ExtraTypes.XF86 as XF86

myBorderWidth = 4
myTerminal = "alacritty"
myModMask = mod4Mask
myFocusedBorderColor = "#88C0D0"
myNormalBorderColor = "#4C566A"
myWorkspace = ["1", "2", "3", "4", "5"]
myManageHookFloat = composeAll
    [ MH.isFullscreen --> MH.doFullFloat
    , MH.isDialog     --> MH.doCenterFloat
    ]
myBar = "xmobar $HOME/.xmonad/xmobarrc"
myPP = DL.xmobarPP { DL.ppTitle = DL.xmobarColor "#88C0D0" "" . DL.shorten 50
                   , DL.ppCurrent = DL.xmobarColor "#88C0D0" "" . DL.wrap "⟨" "⟩"
		   , DL.ppSep = " ║ "
                   }
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)
myLayout =   Sp.spacing 9 $ Ga.gaps [(Ga.U, 20), (Ga.D, 20), (Ga.L, 20), (Ga.R, 20)]
             $ BSP.emptyBSP
	       ||| Si.Simplest
myMouse x = [ ((myModMask, button3), (\w -> focus w >> Flex.mouseResizeWindow w)) ]
newMouse x = M.union (mouseBindings Gn.gnomeConfig x) (M.fromList (myMouse x))

main :: IO ()
main = do
    (xmonad =<<) $ DL.statusBar myBar myPP toggleStrutsKey $ EWMH.ewmh $ Gn.gnomeConfig
        { borderWidth = myBorderWidth
        , terminal = myTerminal
        , modMask = myModMask
        , focusFollowsMouse = False
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , workspaces = myWorkspace
        , mouseBindings = newMouse
            , startupHook = myStartupHook
        , manageHook = myManageHookFloat <+>
                          MD.manageDocks <+>
                          manageHook Gn.gnomeConfig
        , layoutHook = MD.avoidStruts $ ( TL.toggleLayouts (NB.noBorders Full)
                                      $ PW.onWorkspace "3" SF.simplestFloat
                                      $ myLayout
                                      )
        } `EZC.additionalKeysP`
        [ ("M-S-z", spawn "i3lock -c 333333; xset dpms force off")
        , ("M-f", sendMessage $ TL.Toggle "Full")
        , ("C-<Space>", spawn "rofi -show run")
        , ("C-<Print>", spawn "sleep 0.2; scrot -s")
        , ("<Print>", spawn "scrot")
        , ("<XF86PowerOff>", spawn "i3lock -c 333333; xset dpms force off")
        , ("<XF86AudioStop>", spawn "brightnessctl s +10%")
        , ("<XF86AudioPlay>", spawn "brightnessctl s 10%-")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Speaker 10%+")
        , ("<XF86AudioLowerVolume>", spawn "amixer set Speaker 10%-")
        ]

myStartupHook = do
        SO.spawnOnce "compton --config ~/.compton.conf &"
        SO.spawnOnce "~/.fehbg"
        SO.spawnOnce "sudo brightd -d -w 600"
        SO.spawnOnce "sh ~/dotfiles/scripts/sands.sh"
