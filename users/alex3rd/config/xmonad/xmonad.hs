module Main where

import XMonad hiding ((|||))
import XMonad.Actions.Navigation2D (withNavigation2DConfig)
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, docks, manageDocks)

import XMonad.Util.Xkb (xkbLayout)
import XMonad.Util.WindowTypes (checkDialog, checkMenu)
import XMonad.Hooks.DynamicBars (dynStatusBarEventHook, dynStatusBarStartup)
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsEventHook, ewmhDesktopsStartup)
import XMonad.Hooks.Place (placeHook)
import XMonad.Hooks.XPropManage (xPropManageHook)
import XMonad.Util.NamedScratchpad (namedScratchpadManageHook)

import qualified Controls as C
import qualified Layouts as L
import qualified StatusBar as SB
import qualified Workspaces as WS


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
                         dynStatusBarEventHook SB.barCreatorXmobar SB.barDestroyer <+>
                         WS.onRescreen WS.placeWorkplaces,
    layoutHook         = xkbLayout $ avoidStruts $ L.layouts,
    logHook            = SB.loghookXmobar,
    manageHook         = manageDocks <+>
                         manageMenus <+> manageDialogs <+>
                         xPropManageHook WS.xPropMatches <+>
                         namedScratchpadManageHook C.scratchpads <+>
                         placeHook WS.placePolicy,
    startupHook        = WS.placeWorkplaces <+>
                         ewmhDesktopsStartup <+>
                         dynStatusBarStartup SB.barCreatorXmobar SB.barDestroyer
    }
