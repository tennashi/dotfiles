import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import System.IO

myborderWidth = 1
myterminal = "urxvt"
mymodMask = mod4Mask

main :: IO ()
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/tennashi/.xmobarrc"
    xmonad $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , startupHook = myStartupHook
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mymodMask
        , borderWidth = myborderWidth
        , terminal = myterminal
        , focusFollowsMouse = False
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        , ((controlMask, xK_space), spawn "")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        , ((controlMask, xK_Return), spawn "rofi -show run -config ~/.Xresources")
        ]
myStartupHook = do
        spawn "compton --config /home/tennashi/.config/compton/compton.conf &"
        spawn "xscreensaver -nosplash &"
        spawn "nitrogen --restore &"
