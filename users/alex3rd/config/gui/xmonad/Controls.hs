module Controls where

import Data.Maybe (fromJust)
import System.Exit (ExitCode(ExitSuccess), exitWith)
import qualified Data.Map as M

import XMonad hiding ((|||))
import XMonad.Actions.Commands (defaultCommands, runCommand)
import XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies, kill1)
import XMonad.Actions.CycleWS (Direction1D(Next), WSType(EmptyWS), moveTo, nextWS,
                               prevWS, shiftNextScreen, shiftPrevScreen, shiftToNext,
                               shiftToPrev, toggleWS)
import XMonad.Actions.CycleWindows (rotFocusedDown, rotFocusedUp, rotOpposite,
                                    rotUnfocusedDown, rotUnfocusedUp)
import XMonad.Actions.GroupNavigation (nextMatch, Direction(History))
import XMonad.Actions.Navigation2D (centerNavigation, defaultTiledNavigation, hybridOf, sideNavigation,
                                    windowGo, Direction2D(R), Direction2D(L), Direction2D(U), Direction2D(D))
import XMonad.Actions.PhysicalScreens (sendToScreen, viewScreen)
import XMonad.Actions.Promote (promote)
import XMonad.Actions.SinkAll (sinkAll)
import XMonad.Hooks.Place (placeFocused)
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.ResizableTile (MirrorResize(MirrorExpand), MirrorResize(MirrorShrink))
import XMonad.Prompt.XMonad (xmonadPrompt)
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.NamedScratchpad (NamedScratchpad(NS), namedScratchpadAction, nonFloating)
import qualified XMonad.Actions.FlexibleManipulate as Flex
import qualified XMonad.StackSet as W

import XMonad.Util.ExtraCombinators ((~>))
import XMonad.Util.Xkb
import qualified Workspaces as WS
import qualified Layouts as L

customNavigation2DConfig = def { defaultTiledNavigation = hybridOf sideNavigation centerNavigation }

scratchpadTerminal = "alacritty"
scratchpads = [ NS "htop" (scratchpadTerminal ++ " -t htop -e htop") (title =? "htop") nonFloating
              , NS "iotop" (scratchpadTerminal ++ " -t iotop -e sudo iotop") (title =? "iotop") nonFloating
              , NS "gotop" (scratchpadTerminal ++ " -t gotop -e gotop") (title =? "gotop") nonFloating
              , NS "bc" (scratchpadTerminal ++ " -t calc -e bc") (title =? "calc") nonFloating
              ]

basicKeys conf = [ "C-\\"         ~> sendMessage (XkbToggle Nothing)
                 , "M-<Home>"     ~> toggleWS
                 , "M-<Return>"   ~> promote
                 , "M-<Space>"    ~> sendMessage NextLayout
                 , "M-<Tab>"      ~> windows W.focusDown
                 , "M-S-<Space>"  ~> setLayout $ XMonad.layoutHook conf
                 , "M-S-<Tab>"    ~> windows W.focusUp
                 , "M-S-c"        ~> kill1
                 , "M-S-j"        ~> windows W.swapDown
                 , "M-S-k"        ~> windows W.swapUp
                 , "M-S-q"        ~> io (exitWith ExitSuccess)
                 , "M-b"          ~> sendMessage (XkbToggle (Just 0)) >> spawn "@rofiWindow@"
                 , "M-h"          ~> sendMessage Shrink
                 , "M-l"          ~> sendMessage Expand
                 , "M-m"          ~> windows W.focusMaster
                 , "M-n"          ~> refresh
                 , "M-t"          ~> withFocused $ windows . W.sink
                 , "M-x a"        ~> windows copyToAll -- @@ Make focused window always visible
                 , "M-x k"        ~> killAllOtherCopies -- @@ Toggle window state back
                 , "M-r"          ~> WS.placeWorkplaces
                 --
                 , "<Print>"      ~> spawn "@screenshotActiveWindow@"
                 , "<XF86ScreenSaver>" ~> spawn "@lockScreen@"
                 , "C-<Print>"    ~> spawn "@screenshotFull@"
                 , "M-<Print>"    ~> spawn "@screenshotRegion@"
                 -- workspace-dependent bindings
                 , "M-/"          ~> spawn "@searchSelection@" >> WS.showWSOnProperScreen "web"
                 , "M-C-/"        ~> spawn "@searchPrompt@" >> WS.showWSOnProperScreen "web"
                 , "M-C-d"        ~> spawn "@dockerShell@" >> WS.showWSOnProperScreen "shell"
                 , "M-C-j"        ~> spawn "@serviceJournal@" >> WS.showWSOnProperScreen "shell"
                 , "M-C-l"        ~> spawn "@remoteDockerLogs@" >> WS.showWSOnProperScreen "shell"
                 , "M-C-s"        ~> spawn "@dockerStacksInfo@" >> WS.showWSOnProperScreen "shell"
                 , "M-C-y"        ~> spawn "@dbms@" >> WS.showWSOnProperScreen "shell"
                 , "M-S-d"        ~> spawn "@sshCustomUser@" >> WS.showWSOnProperScreen "shell"
                 , "M-S-s"        ~> spawn "@rofiSsh@" >> WS.showWSOnProperScreen "shell"
                 , "M-j"          ~> spawn "@webjumps@" >> WS.showWSOnProperScreen "web"
                 , "M-M1-w"       ~> spawn "@wpaGui@" -- "tmux new-window wpa_cli" >> WS.showWSOnProperScreen "shell"
                 --
                 , "M-C-q"        ~> spawn "xmonad --recompile; xmonad --restart"
                 , "M-q"          ~> spawn "xmonad --restart"
                 , "M-S-p"        ~> spawn "@rofiCombiRun@"
                 , "M-S-h"        ~> spawn "@currentSystemHash@"
                 , "M-M1-q"       ~> spawn "@mergeXresources@"
                 --
                 , "M-C-a"        ~> spawn "@autorandrProfiles@"
                 , "M-C-c"        ~> spawn "@dockerContainerTraits@"
                 , "M-C-h"        ~> spawn "@extraHostsTraits@"
                 , "M-C-m"        ~> spawn "@mountNasVolume@"
                 , "M-C-t"        ~> spawn "@tmuxpSessions@"
                 , "M-C-u"        ~> spawn "@unmountNasVolume@"
                 , "M-S-b"        ~> spawn "@bookshelf@"
                 , "M-S-u"        ~> spawn "@uptimeInfo@"
                 , "M-y"          ~> spawn "@bukuAdd@"
                 , "M-S-x"        ~> spawn "@sctlRestart@"
                 ]

auxKeys = [ "M-a 1"        ~> namedScratchpadAction scratchpads "htop"
          , "M-a 2"        ~> namedScratchpadAction scratchpads "iotop"
          , "M-a 3"        ~> namedScratchpadAction scratchpads "gotop"
          , "M-a 4"        ~> namedScratchpadAction scratchpads "bc"
          --
          , "M-a S-w"      ~> spawn "@jogWifi@"
          , "M-a c"        ~> spawn "@sctlUserRestart@ compton.service"
          , "M-a q"        ~> spawn "@rofiPass@"
          ]

-- TODO: recall and add "multiple app windows"-aware raising
appKeys = [ "M-w <Backspace>" ~> nextMatch History (return True)
          , "M-w S-e"      ~> spawn "@jogEmacs@"
          ]

servicesKeys = [ "M-s d <Up>" ~> spawn "@sctlRestart@ docker-devdns.service"
               , "M-s d <Down>" ~> spawn "@sctlStop@ docker-devdns.service"
               , "M-s s <Up>" ~> spawn "@personalVpnUp@"
               , "M-s s <Down>" ~> spawn "@personalVpnDown@"
               , "M-s v <Up>" ~> spawn "@jobVpnUp@"
               , "M-s v <Down>" ~> spawn "@jobVpnDown@"
               , "M-s x <Up>" ~> spawn "@sctlUserRestart@ xsuspender.service"
               , "M-s x <Down>" ~> spawn "@sctlUserStop@ xsuspender.service"
               ]

hardwareKeys = [ "<XF86AudioRaiseVolume>" ~> spawn "@pctlRaiseVolume@"
               , "<XF86AudioLowerVolume>" ~> spawn "@pctlLowerVolume@"
               , "<XF86AudioPrev>" ~> spawn "@pctl@ previous"
               , "<XF86AudioPlay>" ~> spawn "@pctl@ play-pause"
               , "<XF86AudioNext>" ~> spawn "@pctl@ next"
               , "<XF86AudioMute>" ~> spawn "@pactl@ set-sink-mute @DEFAULT_SINK@ toggle"
               , "<XF86AudioMicMute>" ~> spawn "@pactl@ set-source-mute @DEFAULT_SOURCE@ toggle"
               , "M-<XF86AudioNext>" ~> spawn "@pctlSeekForward@"
               , "M-<XF86AudioPrev>" ~> spawn "@pctlSeekBackward@"
               , "<XF86MonBrightnessUp>" ~> spawn "@brightnessUp@"
               , "<XF86MonBrightnessDown>" ~> spawn "@brightnessDown@"
               , "C-<XF86MonBrightnessUp>" ~> spawn "@brightnessMax@"
               , "C-<XF86MonBrightnessDown>" ~> spawn "@brightnessMin@"
               , "<XF86Launch1>" ~> spawn "@rofiCombiRun@"
               ]

windowKeys = [ "M-S-w M-S-n"       ~> moveTo Next EmptyWS -- find a free workspace
             , "M-S-w M-S-<Up>"    ~> shiftToPrev
             , "M-S-w M-S-<Down>"  ~> shiftToNext
             , "M-S-w M-S-<Right>" ~> shiftToNext >> nextWS
             , "M-S-w M-S-<Left>"  ~> shiftToPrev >> prevWS
             , "M-S-w <Up>"        ~> prevWS
             , "M-S-w <Down>"      ~> nextWS
             , "M-w M-s"           ~> sinkAll
             , "M-S-."             ~> placeFocused WS.placePolicy
             ]

layoutKeys = [ "M-; " ++ keys ~> sendMessage $ JumpToLayout $ layout | (keys, layout) <- L.layoutMappings ] ++
             [ "M-<Right>"     ~> windowGo R True
             , "M-<Left>"      ~> windowGo L True
             , "M-<Up>"        ~> windowGo U True
             , "M-<Down>"      ~> windowGo D True
             ]

switchScreenKeys = [ "M-" ++ m ++ key ~> f sc
                   | (f, m) <- [(viewScreen WS.naturalScreenOrderer, "C-"), (sendToScreen def, "M1-")]
                   , (key, sc) <- [ ("<Down>", 0)
                                  , ("<Up>", 1)
                                  , ("<Right>", 2) ]]

switchWorkspaceKeys conf = [ m ++ k ~> windows $ a i
                           | (a, m) <- [ (W.shift, "M-S-")
                                       , (WS.greedyViewOnScreenFocus 0, "M-S-<Down> M-S-")
                                       , (WS.greedyViewOnScreenFocus 1, "M-S-<Up> M-S-")
                                       , (WS.greedyViewOnScreenFocus 2, "M-S-<Right> M-S-")
                                       ]
                           , (i, k) <- map WS.wsMapping (filter WS.wsMapped WS.mergedWorkspaces) ] ++
                           [ ("M-" ++ key, WS.switchToPrimary name)
                           | (name, key) <- map WS.wsMapping $ filter WS.wsMapped WS.primaryWorkspaces ] ++
                           [ ("M-" ++ key, WS.switchToSecondary name)
                           | (name, key) <- map WS.wsMapping $ filter WS.wsMapped WS.secondaryWorkspaces ] ++
                           [ ("M-" ++ key, WS.switchToTertiary name)
                           | (name, key) <- map WS.wsMapping $ filter WS.wsMapped [WS.scratchpadWorkspace]]

myKeys = \conf -> mkKeymap conf $
         appKeys ++
         auxKeys ++
         hardwareKeys ++
         basicKeys conf ++
         layoutKeys ++
         servicesKeys ++
         switchScreenKeys ++
         switchWorkspaceKeys conf ++
         windowKeys

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
                [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
                , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
                , ((modMask, button3), (\w -> focus w >> Flex.mouseWindow Flex.linear w))
                ]

-- TODO: try to find a block of keys for http://hackage.haskell.org/package/xmonad-contrib-0.14/docs/XMonad-Layout-WindowArranger.html
