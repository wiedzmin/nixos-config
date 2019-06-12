module StatusBar where

import qualified Data.Text as T

import XMonad
import XMonad.Actions.CopyWindow (wsContainingCopies)
import XMonad.Actions.GroupNavigation (historyHook)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.DynamicBars (DynamicStatusBar, DynamicStatusBarCleanup, multiPP)
import XMonad.Hooks.DynamicLog (pad, ppHidden, ppLayout, ppTitle, xmobarColor, xmobarPP)
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsLogHook)
import XMonad.Util.Run (spawnPipe)


replaceSubstring :: String -> String -> String -> String
replaceSubstring what to src = T.unpack (T.replace (T.pack $ take 1 what) (T.pack $ take 1 to) (T.pack src))

loghookXmobar = do
  historyHook
  ewmhDesktopsLogHook
  updatePointer (0.5, 0.5) (0, 0)
  copies <- wsContainingCopies
  let check ws | ws `elem` copies = xmobarColor "red" "black" $ ws
               | otherwise = ws
  let wsPP = xmobarPP { ppTitle = xmobarColor "green" "" . replaceSubstring "{" "[" . replaceSubstring "}" "]"
                      , ppLayout = ppLayoutText
                      , ppHidden = check
                      }
  multiPP wsPP wsPP

ppLayoutAscii = (\ x -> case x of
                    "Tabbed"                         -> "[]"
                    "Float"                          -> "><>"
                    "Tiled"                          -> "||"
                    "Mirror"                         -> "[|]"
                    "Mosaic"                         -> "*"
                    "CGrid"                          -> "{#}"
                    "OneBig"                         -> "[^]"
                    "IM ReflectX IM Tabbed Simplest" -> "IM []"
                    "IM ReflectX IM Grid"            -> "IM #"
                    _                                -> pad x
                )

ppLayoutText = (\ x -> case x of
                   "IM ReflectX IM Tabbed Simplest"        -> "IM Tab"
                   "IM ReflectX IM Grid"                   -> "IM Grid"
                   _                                       -> pad x
               )

barCreatorXmobar :: DynamicStatusBar
barCreatorXmobar (S sid) = do
    trace ("CREATING " ++ show sid)
    spawnPipe ("@xmobar@ $HOME/.config/xmobar/xmobarrc --screen " ++ show sid)

barDestroyer :: DynamicStatusBarCleanup
barDestroyer = return ()
