import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import System.IO

myborderWidth = 1
myterminal = "gnome-terminal"
mymodMask = mod4Mask

main :: IO ()
main = do
    xmproc <- spawnPipe "/usr/bin/xmobar /home/yota/.xmobarrc"
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
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "i3lock")
        , ((controlMask, xK_space), spawn "")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        ]
myStartupHook = do
        -- spawn "compton --config /home/tennashi/.config/compton/compton.conf &"
        spawn "compton --config /home/tennashi/.compton.conf &"
        -- spawn "xscreensaver -nosplash &"
        -- spawn "nitrogen --restore &"
        spawn "albert"
        spawn "~/scripts/sands.sh"
