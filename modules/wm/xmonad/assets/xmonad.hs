module Main where

import System.Exit (ExitCode(ExitSuccess), exitWith)
import Data.Map (fromList)
import Data.Text (pack, replace, unpack)

import XMonad hiding ((|||))
import qualified XMonad.StackSet as W

import XMonad.Actions.CopyWindow (copyToAll, kill1, killAllOtherCopies)
import XMonad.Actions.CopyWindow (wsContainingCopies)
import XMonad.Actions.CycleWS (Direction1D(Next), Direction1D(Prev), WSType(EmptyWS), WSType(NonEmptyWS),
                               findWorkspace, moveTo, nextWS, prevWS, shiftToNext, shiftToPrev, toggleWS)
import XMonad.Actions.GroupNavigation (Direction(History), historyHook, nextMatch)
import XMonad.Actions.Navigation2D (Direction2D(D), Direction2D(L), Direction2D(R), Direction2D(U),
                                    centerNavigation, defaultTiledNavigation, hybridOf, sideNavigation,
                                    windowGo, withNavigation2DConfig)
import XMonad.Actions.PhysicalScreens (ScreenComparator, screenComparatorById, sendToScreen,  viewScreen)
import XMonad.Actions.Promote (promote)
import XMonad.Actions.SinkAll (sinkAll)
import XMonad.Actions.UpdatePointer (updatePointer)
import qualified XMonad.Actions.FlexibleManipulate as Flex

import XMonad.Hooks.DynamicBars (DynamicStatusBar, DynamicStatusBarCleanup,
                                 dynStatusBarEventHook, dynStatusBarStartup, multiPP)
import XMonad.Hooks.DynamicLog (ppHidden, ppTitle, xmobarColor, xmobarPP)
import XMonad.Hooks.EwmhDesktops (ewmhDesktopsLogHook, ewmhDesktopsStartup)
import XMonad.Hooks.ManageDocks (avoidStruts, docks, docksEventHook, manageDocks)
import XMonad.Hooks.Place (fixed, placeFocused, placeHook)
import XMonad.Hooks.XPropManage (XPropMatch, pmP, xPropManageHook)
import XMonad.Hooks.FadeInactive (fadeInactiveLogHook)

import XMonad.Layout.AutoMaster
import XMonad.Layout.DwmStyle as Dwm
import XMonad.Layout.Grid
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.MultiDishes
import XMonad.Layout.OneBig
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Renamed (Rename(Replace), renamed)
import XMonad.Layout.StackTile
import XMonad.Layout.Tabbed as Tabs
import qualified XMonad.Layout.HintedTile as HT

import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.ExtraCombinators ((~>))
import XMonad.Util.NamedScratchpad (NamedScratchpad(NS),
                                    namedScratchpadAction, namedScratchpadManageHook, nonFloating)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.WindowTypes (checkDialog, checkMenu)
import XMonad.Util.Xkb

import XMonad.Workspaces


placePolicy = (fixed (0.5, 0.5))

-- TODO: make layouts negotiable, i.e. they could live in separate module
tabbedLayout = Tabs.tabbed Tabs.shrinkText Tabs.def
dwmLayout = Dwm.dwmStyle Dwm.shrinkText Dwm.def {
  @dwmLayoutFont@
}

layouts = onWorkspace "scratch" (renamed [Replace "tabs"] tabbedLayout) $
          onWorkspace "im" (renamed [Replace "tabs"] tabbedLayout |||
                            renamed [Replace "Dishes"] (dwmLayout (MultiDishes 2 3 (1/6)))) $
          onWorkspace "web" (renamed [Replace "OneBig"] (dwmLayout (OneBig (3/4) (3/4)))) $
          onWorkspace "web2" (StackTile 1 (3/100) (1/2)) $
          onWorkspace "web3" (StackTile 1 (3/100) (1/2)) $
          onWorkspace "work" (renamed [Replace "OneBig"] (dwmLayout (OneBig (3/4) (3/4))) |||
                              renamed [Replace "Dishes"] (dwmLayout (MultiDishes 2 3 (1/6))) |||
                              renamed [Replace "Grid"] (dwmLayout Grid) |||
                              renamed [Replace "Tiled"] (dwmLayout (autoMaster 1 (1/100)
                                                          (HT.HintedTile 1 (3/100) (1/2) HT.TopLeft HT.Tall)))) $
          onWorkspace "work2" (renamed [Replace "OneBig"] (dwmLayout (OneBig (3/4) (3/4))) |||
                              renamed [Replace "Dishes"] (dwmLayout (MultiDishes 2 3 (1/6))) |||
                              renamed [Replace "Grid"] (dwmLayout Grid) |||
                              renamed [Replace "Tiled"] (dwmLayout (autoMaster 1 (1/100)
                                                          (HT.HintedTile 1 (3/100) (1/2) HT.TopLeft HT.Tall)))) $
          onWorkspace "work3" (renamed [Replace "OneBig"] (dwmLayout (OneBig (3/4) (3/4))) |||
                              renamed [Replace "Dishes"] (dwmLayout (MultiDishes 2 3 (1/6))) |||
                              renamed [Replace "Grid"] (dwmLayout Grid) |||
                              renamed [Replace "Tiled"] (dwmLayout (autoMaster 1 (1/100)
                                                          (HT.HintedTile 1 (3/100) (1/2) HT.TopLeft HT.Tall)))) $
          onWorkspace "shell" (renamed [Replace "Dishes"] (dwmLayout (MultiDishes 2 3 (1/6))) |||
                              renamed [Replace "Grid"] (dwmLayout Grid) |||
                              renamed [Replace "Tiled"] (dwmLayout (autoMaster 1 (1/100)
                                                          (HT.HintedTile 1 (3/100) (1/2) HT.TopLeft HT.Tall)))) $
                              renamed [Replace "OneBig"] (OneBig (3/4) (3/4))

layoutMappings = [ ("g", "Grid")
                 , ("b", "OneBig")
                 , ("d", "Dishes")
                 , ("t", "Tiled")
                 ]

loghookXmobar = do
  historyHook
  fadeInactiveLogHook 0.7
  ewmhDesktopsLogHook
  updatePointer (0.5, 0.5) (0, 0)
  copies <- wsContainingCopies
  let check ws | ws `elem` copies = xmobarColor "red" "black" $ ws
               | otherwise = ws
  let wsPP = xmobarPP { ppTitle = xmobarColor "green" "" . replaceSubstring "{" "[" . replaceSubstring "}" "]"
                      , ppHidden = check
                      }
  multiPP wsPP wsPP
  where
    replaceSubstring what to src = unpack (replace (pack $ take 1 what) (pack $ take 1 to) (pack src))

scratchpadTerminal = "alacritty"
scratchpads = [ NS "htop" (scratchpadTerminal ++ " -t htop -e @htopBinary@") (title =? "htop") nonFloating
              , NS "iotop" (scratchpadTerminal ++ " -t iotop -e sudo @iotopBinary@") (title =? "iotop") nonFloating
              , NS "gotop" (scratchpadTerminal ++ " -t gotop -e @gotopBinary@") (title =? "gotop") nonFloating
              , NS "bc" (scratchpadTerminal ++ " -t calc -e @bcBinary@") (title =? "calc") nonFloating
              -- , NS "redis" (scratchpadTerminal ++ " -t redis -e redis-tui") (title =? "redis") nonFloating -- FIXME: no package available
              ]

customKeys conf = [ @xmonadKeys@
                  ]

layoutKeys = [ "M-; " ++ keys ~> sendMessage $ JumpToLayout $ layout | (keys, layout) <- layoutMappings ]

switchScreenKeys = [ "M-" ++ m ++ key ~> f sc
                   | (f, m) <- [(viewScreen naturalScreenOrderer, "C-"), (sendToScreen def, "M1-")]
                   , (key, sc) <- [ ("<Down>", 0)
                                  , ("<Up>", 1)
                                  , ("<Right>", 2) ]]
  where
    naturalScreenOrderer = screenComparatorById comparator where
    comparator id1 id2 = compare id1 id2

switchWorkspaceKeys conf = [ m ++ k ~> windows $ a i
                           | (a, m) <- [ (W.shift, "M-S-")
                                       , (greedyViewOnScreenFocus 0, "M-S-<Down> M-S-")
                                       , (greedyViewOnScreenFocus 1, "M-S-<Up> M-S-")
                                       , (greedyViewOnScreenFocus 2, "M-S-<Right> M-S-")
                                       ]
                           , (i, k) <- map wsMapping (filter wsMapped mergedWorkspaces) ] ++
                           [ ("M-" ++ key, switchToPrimary name)
                           | (name, key) <- map wsMapping $ filter wsMapped primaryWorkspaces ] ++
                           [ ("M-" ++ key, switchToSecondary name)
                           | (name, key) <- map wsMapping $ filter wsMapped secondaryWorkspaces ] ++
                           [ ("M-" ++ key, switchToTertiary name)
                           | (name, key) <- map wsMapping $ filter wsMapped tertiaryWorkspaces ]

myKeys = \conf -> mkKeymap conf $ customKeys conf ++ layoutKeys ++ switchScreenKeys ++ switchWorkspaceKeys conf

myMouseBindings (XConfig {XMonad.modMask = modMask}) = fromList $
                [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
                , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
                , ((modMask, button3), (\w -> focus w >> Flex.mouseWindow Flex.linear w))
                ]

-- TODO: try to find a block of keys for http://hackage.haskell.org/package/xmonad-contrib-0.14/docs/XMonad-Layout-WindowArranger.html
-- TODO: try to find a block of keys for http://hackage.haskell.org/package/xmonad-contrib-0.15/docs/XMonad-Layout-BinarySpacePartition.html

barCreatorXmobar :: DynamicStatusBar
barCreatorXmobar (S sid) = do
    spawnPipe ("@xmobarBinary@ $HOME/.config/xmobar/xmobarrc --screen " ++ show sid)

barDestroyer :: DynamicStatusBarCleanup
barDestroyer = return ()

customNavigation2DConfig = def { defaultTiledNavigation = hybridOf sideNavigation centerNavigation }
configModifiers = withNavigation2DConfig customNavigation2DConfig . docks

manageMenus = checkMenu --> doFloat
manageDialogs = checkDialog --> doFloat

main = do
  xmonad $ configModifiers def {
    focusFollowsMouse  = False,
    borderWidth        = 3,
    modMask            = mod4Mask,
    workspaces         = map wsName mergedWorkspaces,
    normalBorderColor  = "#507060",
    focusedBorderColor = "orange",
    keys               = myKeys,
    mouseBindings      = myMouseBindings,
    handleEventHook    = showWSOnProperScreenHook <+>
                         deleteLastWSWindowHook <+>
                         docksEventHook <+>
                         dynStatusBarEventHook barCreatorXmobar barDestroyer <+>
                         onRescreen placeWorkplaces,
    layoutHook         = xkbLayout $ avoidStruts $ layouts,
    logHook            = loghookXmobar,
    manageHook         = manageDocks <+>
                         manageMenus <+> manageDialogs <+>
                         xPropManageHook xPropMatches <+>
                         namedScratchpadManageHook scratchpads <+>
                         placeHook placePolicy,
    startupHook        = placeWorkplaces <+>
                         ewmhDesktopsStartup <+>
                         dynStatusBarStartup barCreatorXmobar barDestroyer
    }
