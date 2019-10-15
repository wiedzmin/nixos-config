module Main where

import qualified Data.Text as T

import XMonad hiding ((|||))
import XMonad.Actions.CopyWindow (wsContainingCopies)
import XMonad.Actions.GroupNavigation (historyHook)
import XMonad.Actions.Navigation2D (withNavigation2DConfig)
import XMonad.Actions.UpdatePointer (updatePointer)

import XMonad.Hooks.DynamicBars (DynamicStatusBar, DynamicStatusBarCleanup, dynStatusBarEventHook, dynStatusBarStartup, multiPP)
import XMonad.Hooks.DynamicLog (ppHidden, ppTitle, xmobarColor, xmobarPP)
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsLogHook, ewmhDesktopsStartup)
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, docks, manageDocks)
import XMonad.Hooks.Place (placeHook)
import XMonad.Hooks.XPropManage (xPropManageHook)

import XMonad.Layout.AutoMaster
import XMonad.Layout.DwmStyle (dwmStyle, shrinkText, defaultTheme)
import XMonad.Layout.Grid
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.MultiDishes
import XMonad.Layout.OneBig
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.StackTile
import XMonad.Layout.Tabbed (tabbed, shrinkText, defaultTheme)
import qualified XMonad.Layout.HintedTile as HT

import XMonad.Util.NamedScratchpad (namedScratchpadManageHook)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.WindowTypes (checkDialog, checkMenu)
import XMonad.Util.Xkb (xkbLayout)

import qualified Controls as C
import qualified Workspaces as WS


tabbedLayout = tabbed shrinkText defaultTheme
dwmLayout = dwmStyle shrinkText defaultTheme

layouts = onWorkspace "scratch" (renamed [Replace "tabs"] tabbedLayout) $
          onWorkspace "im" (renamed [Replace "tabs"] tabbedLayout) $
          onWorkspace "web" (renamed [Replace "OneBig"] (OneBig (3/4) (3/4))) $
          onWorkspace "web2" (StackTile 1 (3/100) (1/2)) $
          onWorkspace "web3" (StackTile 1 (3/100) (1/2)) $
          onWorkspace "work" (renamed [Replace "OneBig"] (OneBig (3/4) (3/4)) |||
                              renamed [Replace "Dishes"] (MultiDishes 2 3 (1/6)) |||
                              renamed [Replace "Grid"] Grid |||
                              renamed [Replace "Tiled"] (autoMaster 1 (1/100)
                                                          (HT.HintedTile 1 (3/100) (1/2) HT.TopLeft HT.Tall))) $
          onWorkspace "shell" (renamed [Replace "OneBig"] (OneBig (3/4) (3/4))) $
          renamed [Replace "OneBig"] (OneBig (3/4) (3/4))

layoutMappings = [ ("g", "Grid")
                 , ("b", "OneBig")
                 , ("d", "Dishes")
                 , ("t", "Tiled")
                 ]

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
                      , ppHidden = check
                      }
  multiPP wsPP wsPP

barCreatorXmobar :: DynamicStatusBar
barCreatorXmobar (S sid) = do
    spawnPipe ("@xmobar@ $HOME/.config/xmobar/xmobarrc --screen " ++ show sid)

barDestroyer :: DynamicStatusBarCleanup
barDestroyer = return ()

configModifiers = withNavigation2DConfig C.customNavigation2DConfig . docks

manageMenus = checkMenu --> doFloat
manageDialogs = checkDialog --> doFloat

main = do
  xmonad $ configModifiers def {
    focusFollowsMouse  = False,
    borderWidth        = 3,
    modMask            = mod4Mask,
    workspaces         = map WS.wsName $ filter WS.wsEnabled $ WS.mergedWorkspaces,
    normalBorderColor  = "#507060",
    focusedBorderColor = "orange",
    keys               = C.myKeys,
    mouseBindings      = C.myMouseBindings,
    handleEventHook    = WS.showWSOnProperScreenHook <+>
                         WS.deleteLastWSWindowHook <+>
                         docksEventHook <+>
                         dynStatusBarEventHook barCreatorXmobar barDestroyer <+>
                         WS.onRescreen WS.placeWorkplaces,
    layoutHook         = xkbLayout $ avoidStruts $ layouts,
    logHook            = loghookXmobar,
    manageHook         = manageDocks <+>
                         manageMenus <+> manageDialogs <+>
                         xPropManageHook WS.xPropMatches <+>
                         namedScratchpadManageHook C.scratchpads <+>
                         placeHook WS.placePolicy,
    startupHook        = WS.placeWorkplaces <+>
                         ewmhDesktopsStartup <+>
                         dynStatusBarStartup barCreatorXmobar barDestroyer
    }
