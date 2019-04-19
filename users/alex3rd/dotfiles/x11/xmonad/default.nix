{config, pkgs, lib, ...}:

with import <home-manager/modules/lib/dag.nix> { inherit lib; }; # TODO: make more declarative
with import ../../../../toolbox/util.nix {inherit lib config pkgs;};
with import ../../const.nix {inherit config pkgs;};
{
    home-manager.users.alex3rd = {
        home.file = {
            ".xmonad/lib/Extensions/Xkb.hs".source = ./lib/Xkb.hs;
            ".xmonad/lib/Extensions/ExtraCombinators.hs".source = ./lib/ExtraCombinators.hs;
            ".xmonad/lib/Controls.hs" = {
                text = ''
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

                    import Extensions.ExtraCombinators ((~>))
                    import Extensions.Xkb
                    import qualified Workspaces as WS
                    import qualified Layouts as L

                    customNavigation2DConfig = def { defaultTiledNavigation = hybridOf sideNavigation centerNavigation }

                    scratchpadTerminal = "alacritty"
                    scratchpads = [ NS "htop" (scratchpadTerminal ++ " -t htop -e htop") (title =? "htop") nonFloating
                                  , NS "iotop" (scratchpadTerminal ++ " -t iotop -e sudo iotop") (title =? "iotop") nonFloating
                                  , NS "gotop" (scratchpadTerminal ++ " -t gotop -e gotop") (title =? "gotop") nonFloating
                                  , NS "nmtui" (scratchpadTerminal ++ " -t nmtui -e nmtui") (title =? "nmtui") nonFloating
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
                                     , "M-b"          ~> sendMessage (XkbToggle (Just 0)) >> spawn "${pkgs.rofi}/bin/rofi -show window"
                                     , "M-h"          ~> sendMessage Shrink
                                     , "M-l"          ~> sendMessage Expand
                                     , "M-m"          ~> windows W.focusMaster
                                     , "M-n"          ~> refresh
                                     , "M-t"          ~> withFocused $ windows . W.sink
                                     , "M-x a"        ~> windows copyToAll -- @@ Make focused window always visible
                                     , "M-x k"        ~> killAllOtherCopies -- @@ Toggle window state back
                                     , "M-r"          ~> WS.placeWorkplaces
                                     --
                                     , "<Print>"      ~> spawn "${pkgs.screenshot_active_window}/bin/screenshot_active_window"
                                     , "<XF86ScreenSaver>" ~> spawn "${pkgs.i3lock-color}/bin/i3lock-color -c 232729 && ${pkgs.xorg.xset}/bin/xset dpms force off"
                                     , "C-<Print>"    ~> spawn "${pkgs.screenshot_full}/bin/screenshot_full"
                                     , "M-<Print>"    ~> spawn "${pkgs.screenshot_region}/bin/screenshot_region"
                                     -- workspace-dependent bindings
                                     , "M-/"          ~> spawn "${pkgs.rofi_searchengines_selection}/bin/rofi_searchengines_selection" >> WS.showWSOnProperScreen "web"
                                     , "M-C-/"        ~> spawn "${pkgs.rofi_searchengines_prompt}/bin/rofi_searchengines_prompt" >> WS.showWSOnProperScreen "web"
                                     , "M-C-d"        ~> spawn "${pkgs.rofi_docker_shell}/bin/rofi_docker_shell" >> WS.showWSOnProperScreen "shell"
                                     , "M-C-j"        ~> spawn "${pkgs.rofi_service_journal}/bin/rofi_service_journal" >> WS.showWSOnProperScreen "shell"
                                     , "M-C-l"        ~> spawn "${pkgs.rofi_remote_docker_logs}/bin/rofi_remote_docker_logs" >> WS.showWSOnProperScreen "shell"
                                     , "M-C-s"        ~> spawn "${pkgs.rofi_docker_stacks_info}/bin/rofi_docker_stacks_info" >> WS.showWSOnProperScreen "shell"
                                     , "M-C-y"        ~> spawn "${pkgs.rofi_dbms}/bin/rofi_dbms" >> WS.showWSOnProperScreen "shell"
                                     , "M-S-d"        ~> spawn "${pkgs.rofi_ssh_custom_user}/bin/rofi_ssh_custom_user" >> WS.showWSOnProperScreen "shell"
                                     , "M-S-s"        ~> spawn "${pkgs.rofi}/bin/rofi -show ssh" >> WS.showWSOnProperScreen "shell"
                                     , "M-j"          ~> spawn "${pkgs.rofi_webjumps}/bin/rofi_webjumps" >> WS.showWSOnProperScreen "web"
                                     --
                                     , "M-C-q"        ~> spawn "xmonad --recompile; xmonad --restart"
                                     , "M-q"          ~> spawn "xmonad --restart"
                                     , "M-S-p"        ~> spawn "${pkgs.rofi}/bin/rofi -combi-modi drun,run -show combi -modi combi"
                                     , "M-S-h"        ~> spawn "${pkgs.show_current_system_hash}/bin/show_current_system_hash"
                                     , "M-M1-w"       ~> spawn "${pkgs.networkmanager_dmenu}/bin/networkmanager_dmenu" -- using rofi, despite naming
                                     , "M-M1-q"       ~> spawn "${pkgs.xorg.xrdb}/bin/xrdb -merge $HOME/.Xresources"
                                     --
                                     , "M-C-a"        ~> spawn "${pkgs.rofi_autorandr_profiles}/bin/rofi_autorandr_profiles"
                                     , "M-C-c"        ~> spawn "${pkgs.rofi_docker_container_traits}/bin/rofi_docker_container_traits"
                                     , "M-C-h"        ~> spawn "${pkgs.rofi_extra_hosts_traits}/bin/rofi_extra_hosts_traits"
                                     , "M-C-i"        ~> spawn "${pkgs.rofi_insert_snippet}/bin/rofi_insert_snippet"
                                     , "M-C-m"        ~> spawn "${pkgs.rofi_mount_nas_volume}/bin/rofi_mount_nas_volume"
                                     , "M-C-t"        ~> spawn "${pkgs.rofi_tmuxp_sessions}/bin/rofi_tmuxp_sessions"
                                     , "M-C-u"        ~> spawn "${pkgs.rofi_unmount_nas_volume}/bin/rofi_unmount_nas_volume"
                                     , "M-S-b"        ~> spawn "${pkgs.rofi_bookshelf}/bin/rofi_bookshelf"
                                     , "M-S-u"        ~> spawn "${pkgs.show_uptime_info}/bin/show_uptime_info"
                                     , "M-e"          ~> spawn "${pkgs.rofi_entrypoint}/bin/rofi_entrypoint"
                                     , "M-y"          ~> spawn "${pkgs.rofi_buku_add}/bin/rofi_buku_add"
                                     , "M-C-b"        ~> spawn "${pkgs.rofi_buku_entrypoint}/bin/rofi_buku_entrypoint"
                                     -- TODO: consider adding keybinding for restarting xkeysnail service, it turned out it could stick some functonality (for example, copy to clipboard twice)
                                     ]

                    auxKeys = [ "M-a 1"        ~> namedScratchpadAction scratchpads "htop"
                              , "M-a 2"        ~> namedScratchpadAction scratchpads "iotop"
                              , "M-a 3"        ~> namedScratchpadAction scratchpads "gotop"
                              , "M-a 4"        ~> namedScratchpadAction scratchpads "bc"
                              --
                              , "M-a S-w"      ~> spawn "${pkgs.wifictl}/bin/wifictl jog"
                              , "M-a c"        ~> spawn "${pkgs.systemd}/bin/systemctl --user restart compton.service"
                              , "M-a q"        ~> spawn "${pkgs.rofi-pass}/bin/rofi-pass"
                              , "M-a w"        ~> spawn "${pkgs.rescale-wallpaper}/bin/rescale-wallpaper"
                              ]

                    -- TODO: recall and add "multiple app windows"-aware raising
                    appKeys = [ "M-w <Backspace>" ~> nextMatch History (return True)
                              , "M-w S-e"      ~> spawn "${pkgs.procps}/bin/pkill -SIGUSR2 emacs"
                              ]

                    servicesKeys = [ "M-s s <Up>" ~> spawn "${pkgs.sshuttlectl}/bin/sshuttlectl start"
                                   , "M-s s <Down>" ~> spawn "${pkgs.sshuttlectl}/bin/sshuttlectl stop"
                                   , "M-s v <Up>" ~> spawn "${pkgs.jobvpnctl}/bin/jobvpnctl start"
                                   , "M-s v <Down>" ~> spawn "${pkgs.jobvpnctl}/bin/jobvpnctl stop"
                                   , "M-s x <Up>" ~> spawn "${pkgs.systemd}/bin/systemctl restart xsuspender.service"
                                   , "M-s x <Down>" ~> spawn "${pkgs.systemd}/bin/systemctl stop xsuspender.service"
                                   ]

                    multimediaKeys = [ "<XF86AudioRaiseVolume>" ~> spawn "${pkgs.volumectl}/bin/volumectl inc"
                                     , "<XF86AudioLowerVolume>" ~> spawn "${pkgs.volumectl}/bin/volumectl dec"
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

                    mpdKeys = [ "<XF86AudioPrev>" ~> spawn "${pkgs.mpc_cli}/bin/mpc prev"
                              , "<XF86AudioPlay>" ~> spawn "${pkgs.mpc_cli}/bin/mpc toggle"
                              , "<XF86AudioNext>" ~> spawn "${pkgs.mpc_cli}/bin/mpc next"
                              , "<XF86AudioMute>" ~> spawn "${pkgs.volumectl}/bin/volumectl tog"
                              , "M-<XF86AudioNext>" ~> spawn "${pkgs.mpc_cli}/bin/mpc seek +2%"
                              , "M-<XF86AudioPrev>" ~> spawn "${pkgs.mpc_cli}/bin/mpc seek -2%"
                              ]

                    backlightKeys = [ "<XF86MonBrightnessUp>" ~> spawn "${pkgs.backlightctl}/bin/backlightctl inc"
                                    , "<XF86MonBrightnessDown>" ~> spawn "${pkgs.backlightctl}/bin/backlightctl dec"
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
                             backlightKeys ++
                             basicKeys conf ++
                             layoutKeys ++
                             mpdKeys ++
                             multimediaKeys ++
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
                '';
                onChange = ''xmonad --recompile'';
            };
            ".xmonad/lib/Layouts.hs" = {
                text = ''
                    {-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

                    module Layouts where

                    import XMonad hiding ((|||))
                    import XMonad.Layout.AutoMaster
                    import XMonad.Layout.Dishes
                    import XMonad.Layout.DwmStyle (dwmStyle, shrinkText)
                    import XMonad.Layout.Grid
                    import XMonad.Layout.IM (Property(And), Property(ClassName), Property(Or),
                                             Property(Role), Property(Title), Property(Resource),
                                             withIM)
                    import XMonad.Layout.LayoutCombinators
                    import XMonad.Layout.OneBig
                    import XMonad.Layout.PerWorkspace (onWorkspace)
                    import XMonad.Layout.Reflect (reflectHoriz)
                    import XMonad.Layout.Renamed (renamed, Rename(Replace))
                    import XMonad.Layout.ResizableTile
                    import XMonad.Layout.Tabbed (tabbed)
                    import qualified XMonad.Layout.HintedTile as HT

                    import Themes (tabbedConf)


                    customHintedTile = HT.HintedTile nmaster delta ratio HT.TopLeft HT.Tall
                      where
                        nmaster = 1
                        ratio   = 1/2
                        delta   = 3/100

                    imLayout = withIM (0.15) (isPsiRoster `Or` isPsiPlusRoster) $ reflectHoriz $ withIM (0.15) (isPidginRoster) (tabbed shrinkText def ||| Grid)
                    gimpLayout = withIM (0.17) (isGimpToolbox) $ reflectHoriz $ withIM (0.15) (isGimpDock) (Full)
                    gimpLayoutAlt = reflectHoriz $ withIM (11/64) (isGimpToolbox) $ ResizableTall 2 (1/118) (11/20) [5/4,5/4,5/4]
                    tabbedLayout = tabbed shrinkText tabbedConf -- | defaultTheme
                    dwmLayout = dwmStyle shrinkText tabbedConf

                    isPidginRoster = And (ClassName "Pidgin") (Role "buddy_list")
                    isPidginChat = And (ClassName "Pidgin") (Role "conversation")
                    isPsiRoster = And (ClassName "psi") (Role "psimain")
                    isPsiPlusRoster = Or (Title "Psi+") (And (Resource "main") (ClassName "psi"))
                    isPsiChat = And (Resource "chat") (ClassName "psi")

                    isGimpMain = And (ClassName "Gimp") (Role "gimp-image-window-1")
                    isGimpToolbox = And (ClassName "Gimp") (Role "gimp-toolbox-1")
                    isGimpDock = And (ClassName "Gimp") (Role "gimp-dock-1")
                    isGimpMessage = And (ClassName "Gimp") (Role "gimp-message")

                    layouts = onWorkspace "scratch" gimpLayout $
                              onWorkspace "im" imLayout $
                              onWorkspace "web" (renamed [Replace "Grid"] Grid |||
                                                 renamed [Replace "OneBig"] (OneBig (3/4) (3/4))) $
                              onWorkspace "work" (renamed [Replace "Tiled"] (autoMaster 1 (1/100) customHintedTile) |||
                                                  renamed [Replace "Dishes"] (Dishes 2 (1/6)) |||
                                                  renamed [Replace "Mirror"] (Mirror customHintedTile) |||
                                                  renamed [Replace "Grid"] Grid |||
                                                  renamed [Replace "OneBig"] (OneBig (3/4) (3/4))) $
                              onWorkspace "shell" (renamed [Replace "OneBig"] (OneBig (3/4) (3/4))) $
                              renamed [Replace "OneBig"] (OneBig (3/4) (3/4))

                    layoutMappings = [ ("g", "Grid")
                                     , ("m", "Mirror")
                                     , ("b", "OneBig")
                                     , ("d", "Dishes")
                                     , ("t", "Tiled")
                                     ]
                '';
                onChange = ''xmonad --recompile'';
            };
            ".xmonad/lib/StatusBar.hs" = {
                text = ''
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
                        spawnPipe ("${pkgs.xmobar}/bin/xmobar $HOME/.config/xmobar/xmobarrc --screen " ++ show sid)

                    barDestroyer :: DynamicStatusBarCleanup
                    barDestroyer = return ()
                '';
                onChange = ''xmonad --recompile'';
            };
            ".xmonad/lib/Themes.hs" = {
                text = ''
                    module Themes where

                    import XMonad.Layout.Decoration
                    import XMonad.Prompt
                    import XMonad.Prompt.FuzzyMatch (fuzzyMatch)

                    -- Font definitions
                    fontDejavuSans2       = "xft:DejaVu Sans Mono:pixelsize=14"
                    fontDjvuSans          = "xft:DejaVu Sans:size=10"
                    fontDjvuSansMono      = "xft:Dejavu Sans Mono-8"
                    fontInconsolata       = "xft:inconsolata-14"
                    fontIosevkaMain       = "xft:Iosevka:style=Bold:pixelsize=16"
                    fontMonospace         = "xft:monospace:size=8"
                    fontMonospaceSmall    = "xft:monospace:size=5"
                    fontProfont           = "xft:Profont:pixelsize=15:autohint=true"
                    fontTerminusNormal    = "xft:Terminus:size=12"

                    fontDmenu             = fontIosevkaMain
                    fontPrompt            = fontIosevkaMain
                    fontTabbed            = fontIosevkaMain

                    ------------------------------------------------------------------------
                    -- Various color settings
                    ------------------------------------------------------------------------

                    -- This mimics the "zenburn" style as found for vim and Emacs.
                    colorDarkRed      = "#705050"
                    colorDarkGreen    = "#507060" -- Original zenburn: "#60b48a"
                    colorDarkYellow   = "#dfaf8f"
                    colorDarkBlue     = "#506070"
                    colorDarkMagenta  = "#dc8cc3"
                    colorDarkCyan     = "#8cd0d3"
                    colorDarkGray     = "#3f3f3f"
                    colorLightRed     = "#dca3a3"
                    colorLightGreen   = "#c3bf9f"
                    colorLightYellow  = "#f0dfaf"
                    colorLightBlue    = "#94bff3"
                    colorLightMagenta = "#ec93d3"
                    colorLightCyan    = "#93e0e3"
                    colorLightGray    = "#dcdccc"
                    colorWhite        = "#ffffff"
                    colorYellow       = "#ffff00"

                    -- Status Bar colors
                    -- Actually, the definitions for BG and FG colors are used differently
                    -- sometimes. dmenu (used for launching apps) turns them around, making the
                    -- change in the status bar location easily visible.

                    curGlobalBGColor      = colorDarkBlue
                    curGlobalFGColor      = colorLightGray
                    curNormalBGColor      = colorDarkGreen
                    curNormalFGColor      = colorLightGray
                    curTitleFGColor       = colorLightYellow
                    curUrgentBGColor      = colorDarkRed
                    curUrgentFGColor      = colorLightRed

                    curFocusedBGColor     = curGlobalFGColor
                    curFocusedFGColor     = curGlobalBGColor
                    curSeparatorColor     = curGlobalFGColor
                    curTitleBGColor       = curGlobalBGColor

                    -- intended to be an example of all used params
                    -- should not be used unless filled with appropriate param values
                    xpConfigFullPH :: XPConfig
                    xpConfigFullPH = def { font        = fontPrompt
                                         , bgColor     = ""
                                         , fgColor     = ""
                                         , fgHLight    = ""
                                         , bgHLight    = ""
                                         , borderColor = ""
                                         , promptBorderWidth = 1
                                         , position          = Bottom -- Top
                                         , height            = 20 -- = 15 -- 18 px line height - 3 border px
                                         , autoComplete      = Just 1000 -- Just (seconds 0.5)
                                         , historySize       = 1000
                                         , historyFilter     = deleteAllDuplicates -- nub [String] -> [String]
                                         , showCompletionOnTab = True
                                         , defaultText     = ""
                                         }

                    xpConfigDefault :: XPConfig
                    xpConfigDefault = xpConfigFullPH { bgColor         = curNormalBGColor
                                                     , fgColor         = curTitleFGColor
                                                     , fgHLight        = curNormalBGColor
                                                     , bgHLight        = curTitleFGColor
                                                     , borderColor     = curNormalFGColor
                                                     , searchPredicate = fuzzyMatch
                                                     -- TODO: try to make promptKeymap for layouts switching
                                                     }

                    -- Hint: default theme can be substituted by any from Util.Themes
                    tabbedConf = def {
                      fontName = fontTabbed
                      }
                '';
                onChange = ''xmonad --recompile'';
            };
            ".xmonad/lib/Workspaces.hs" = {
                text = ''
                    module Workspaces where

                    import Control.Lens
                    import Control.Monad (liftM2, when)
                    import Data.List (find, isInfixOf, sortBy)
                    import Data.Maybe (fromJust, fromMaybe, isNothing, isJust, maybeToList)
                    import Data.Monoid (All(..))

                    import XMonad
                    import XMonad.Actions.CycleWS (Direction1D(Prev), WSType(NonEmptyWS), findWorkspace)
                    import XMonad.Actions.DynamicWorkspaceOrder (getSortByOrder)
                    import XMonad.Actions.DynamicWorkspaces (removeEmptyWorkspace)
                    import XMonad.Actions.OnScreen (greedyViewOnScreen, Focus(FocusNew), onScreen)
                    import XMonad.Actions.PhysicalScreens (ScreenComparator, screenComparatorById)
                    import XMonad.Hooks.Place (fixed)
                    import XMonad.Hooks.XPropManage (pmP, XPropMatch)
                    import XMonad.Util.WorkspaceCompare (getSortByIndex)
                    import XMonad.Util.XUtils (fi)
                    import qualified XMonad.StackSet as W
                    import qualified XMonad.Util.ExtensibleState as ES


                    newWSName = "new ws"

                    type WSData = ( String         -- name
                                  , (Maybe [Char]) -- mapping
                                  , Bool           -- enabled
                                  , Bool           -- permanent
                                  )

                    primaryWorkspaces, secondaryWorkspaces :: [WSData]

                    primaryWorkspaces =
                      [ ("web", Just "<F1>", True, True)
                      , ("work", Just "<F2>", True, True)
                      , ("virt", Just "<F4>", True, True)
                      ]

                    secondaryWorkspaces =
                      [ ("shell", Just "<F3>", True, True)
                      , ("read", Just "4", True, True)
                      , ("im", Just "3", True, True)
                      ]

                    scratchpadWorkspace :: WSData
                    scratchpadWorkspace = ("scratch", Just "<Esc>", True, True)

                    xPropMatches :: [XPropMatch]
                    xPropMatches = [ ([ (wM_CLASS, any ("Alacritty" ==))], pmP (viewShift "shell"))
                                   , ([ (wM_CLASS, any ("Apvlv" ==))], pmP (viewShift "read"))
                                   , ([ (wM_CLASS, any ("Chromium-browser" ==))], pmP (viewShift "web"))
                                   , ([ (wM_CLASS, any ("Digikam" ==))], pmP (viewShift "scratch"))
                                   , ([ (wM_CLASS, any ("Djview3" ==))], pmP (viewShift "read"))
                                   , ([ (wM_CLASS, any ("Djview4" ==))], pmP (viewShift "read"))
                                   , ([ (wM_CLASS, any ("Dolphin" ==))], pmP (viewShift "work"))
                                   , ([ (wM_CLASS, any ("Emacs" ==))], pmP (viewShift "work"))
                                   , ([ (wM_CLASS, any ("FBReader" ==))], pmP (viewShift "read"))
                                   , ([ (wM_CLASS, any ("Firefox" ==))], pmP (viewShift "web"))
                                   , ([ (wM_CLASS, any ("Gimp" ==))], pmP (viewShift "scratch"))
                                   , ([ (wM_CLASS, any ("Gwenview" ==))], pmP (viewShift "scratch"))
                                   , ([ (wM_CLASS, any ("Homebank" ==))], pmP (viewShift "scratch"))
                                   , ([ (wM_CLASS, any ("Inkview" ==))], pmP (viewShift "scratch"))
                                   , ([ (wM_CLASS, any ("Nautilus" ==))], pmP (viewShift "scratch"))
                                   , ([ (wM_CLASS, any ("Pidgin" ==))], pmP (viewShift "im"))
                                   , ([ (wM_CLASS, any ("Rapid Photo Downloader" ==))], pmP (viewShift "scratch"))
                                   , ([ (wM_CLASS, any ("Sakura" ==))], pmP (viewShift "shell"))
                                   , ([ (wM_CLASS, any ("Simple-scan" ==))], pmP (viewShift "scratch"))
                                   , ([ (wM_CLASS, any ("Skype" ==))], pmP (viewShift "im"))
                                   , ([ (wM_CLASS, any ("Slack" ==))], pmP (viewShift "im"))
                                   , ([ (wM_CLASS, any ("TelegramDesktop" ==))], pmP (viewShift "im"))
                                   , ([ (wM_CLASS, any ("URxvt" ==))], pmP (viewShift "shell"))
                                   , ([ (wM_CLASS, any ("Virt-manager" ==))], pmP (viewShift "scratch"))
                                   , ([ (wM_CLASS, any ("VirtualBox" ==))], pmP (viewShift "virt"))
                                   , ([ (wM_CLASS, any ("Xsane" ==))], pmP (viewShift "scratch"))
                                   , ([ (wM_CLASS, any ("aft-linux-qt" ==))], pmP (viewShift "scratch"))
                                   , ([ (wM_CLASS, any ("athura" `isInfixOf`))], pmP (viewShift "read"))
                                   , ([ (wM_CLASS, any ("calibre" ==))], pmP (viewShift "read"))
                                   , ([ (wM_CLASS, any ("glogg" ==))], pmP (viewShift "scratch"))
                                   , ([ (wM_CLASS, any ("llpp" ==))], pmP (viewShift "read"))
                                   , ([ (wM_CLASS, any ("polar-bookshelf" ==))], pmP (viewShift "read"))
                                   , ([ (wM_CLASS, any ("psi" ==))], pmP (viewShift "im"))
                                   , ([ (wM_CLASS, any ("scantailor-advanced" ==))], pmP (viewShift "scratch"))
                                   , ([ (wM_CLASS, any ("zoom" ==))], pmP (viewShift "im"))
                                   , ([ (wM_CLASS, any ("Soffice" ==))], pmP (viewShift "read"))
                                   , ([ (wM_CLASS, any ("Code" ==))], pmP (viewShift "work"))
                                   ]
                      where
                        viewShift = liftM2 (.) W.view W.shift

                    placePolicy = (fixed (0.5, 0.5))

                    naturalScreenOrderer :: ScreenComparator
                    naturalScreenOrderer = screenComparatorById comparator where
                      comparator id1 id2 = compare id1 id2

                    mergedWorkspaces = (p:s:scratchpadWorkspace:ps) ++ ss
                      where p:ps = primaryWorkspaces
                            s:ss = secondaryWorkspaces

                    wsEnabled (_, _, enabled, _) = enabled
                    wsMapped (_, mapping, _, _) = isJust mapping
                    wsMapping (name, mapping, _, _) = (name, fromJust mapping)
                    wsName (name, _, _, _) = name
                    wsTransient (_, _, _, permanent) = not permanent


                    greedyViewOnScreenFocus :: ScreenId     -- ^ screen id
                                            -> WorkspaceId  -- ^ index of the workspace
                                            -> WindowSet    -- ^ current stack
                                            -> WindowSet
                    greedyViewOnScreenFocus sid i = onScreen (W.greedyView i) FocusNew sid

                    isCurrentWorkspaceEmpty ss = isNothing . W.stack . W.workspace . W.current $ ss

                    isCurrentWorkspaceTransient ss = (W.currentTag $ ss) `elem` transientWorkspaceNames
                      where
                        transientWorkspaceNames = map wsName $ filter wsTransient $ filter wsEnabled mergedWorkspaces

                    deleteLastWSWindowHook :: Event -> X All
                    deleteLastWSWindowHook (DestroyWindowEvent {ev_window = w}) = do
                      ws <- gets windowset
                      when (isCurrentWorkspaceTransient ws) removeEmptyWorkspace
                      when (isCurrentWorkspaceEmpty ws && (not $ isCurrentWorkspaceTransient ws))
                        (findWorkspace getSortByOrder Prev NonEmptyWS 2 >>= \t -> windows . W.greedyView $ t)
                      return (All True)
                    deleteLastWSWindowHook _ = return (All True)

                    showWSOnProperScreenHook :: Event -> X All
                    showWSOnProperScreenHook ClientMessageEvent { ev_window = w
                                                                , ev_message_type = mt
                                                                , ev_data = d } =
                      withWindowSet $ \s -> do
                        sort' <- getSortByIndex
                        let ws = sort' $ W.workspaces s

                        a_aw <- getAtom "_NET_ACTIVE_WINDOW"
                        a_cd <- getAtom "_NET_CURRENT_DESKTOP"
                        a_cw <- getAtom "_NET_CLOSE_WINDOW"
                        a_d <- getAtom "_NET_WM_DESKTOP"
                        if mt == a_cd then do
                          let n = head d
                          if 0 <= n && fi n < length ws then showWSOnProperScreen (W.tag (ws !! fi n))
                          else trace $ "Bad _NET_CURRENT_DESKTOP with data[0]=" ++ show n
                        else if mt == a_d then do
                          let n = head d
                          if 0 <= n && fi n < length ws then windows $ W.shiftWin (W.tag (ws !! fi n)) w
                          else trace $ "Bad _NET_DESKTOP with data[0]=" ++ show n
                        else if mt == a_aw then do
                          case W.findTag w s of
                            Nothing -> pure ()
                            Just tag -> do
                              showWSOnProperScreen tag
                              windows $ W.focusWindow w
                        else if mt == a_cw then do
                          killWindow w
                        else do
                          -- The Message is unknown to us, but that is ok, not all are meant
                          -- to be handled by the window manager
                          pure ()
                        return (All True)
                    showWSOnProperScreenHook _ = return (All True)

                    showWSOnProperScreen :: String -> X ()
                    showWSOnProperScreen ws = case classifyWorkspace ws of
                      Primary -> switchToPrimary ws
                      Secondary -> switchToSecondary ws
                      Tertiary -> switchToTertiary ws

                    data WorkspaceType = Primary | Secondary | Tertiary deriving (Eq)

                    classifyWorkspace :: WorkspaceId -> WorkspaceType
                    classifyWorkspace ws
                      | ws `elem` (map wsName primaryWorkspaces) = Primary
                      | ws `elem` (map wsName secondaryWorkspaces) = Secondary
                      | otherwise = Tertiary

                    switchToPrimary :: WorkspaceId -> X ()
                    switchToPrimary name = do
                      windows $ viewPrimary name
                      ES.modify $ primaryChoiceL .~ name
                      pure ()

                    switchToSecondary :: WorkspaceId -> X ()
                    switchToSecondary name = do
                      windows $ viewSecondary name
                      ES.modify $ secondaryChoiceL .~ name
                      pure ()

                    switchToTertiary :: WorkspaceId -> X ()
                    switchToTertiary name = do
                      windows $ viewTertiary name
                      ES.modify $ tertiaryChoiceL .~ name
                      pure ()

                    viewPrimary, viewSecondary, viewTertiary :: WorkspaceId -> WindowSet -> WindowSet

                    viewPrimary i ss = go (detectMonitorConfig ss)
                      where
                        go SingleMonitor = W.view i ss
                        go _ = greedyViewOnScreenFocus 1 i ss

                    viewSecondary i ss = go (detectMonitorConfig ss)
                      where
                        go SingleMonitor = W.view i ss
                        go DualMonitor = greedyViewOnScreenFocus 1 i ss
                        go (TripleMonitor sec _) = greedyViewOnScreenFocus sec i ss

                    viewTertiary i ss = go (detectMonitorConfig ss)
                      where
                        go SingleMonitor = W.view i ss
                        go DualMonitor = greedyViewOnScreenFocus 0 i ss
                        go (TripleMonitor _ ter) = greedyViewOnScreenFocus ter i ss

                    data WorkspaceChoice = WorkspaceChoice WorkspaceId WorkspaceId WorkspaceId deriving (Typeable, Read, Show, Eq)
                    instance ExtensionClass WorkspaceChoice where
                      initialValue = WorkspaceChoice
                        (wsName $ head primaryWorkspaces)
                        (wsName $ head secondaryWorkspaces)
                        (wsName scratchpadWorkspace)
                      extensionType = PersistentExtension

                    primaryChoiceL :: Lens' WorkspaceChoice WorkspaceId
                    primaryChoiceL k (WorkspaceChoice prim sec ter) = fmap (\newPrim -> WorkspaceChoice newPrim sec ter) (k prim)

                    secondaryChoiceL :: Lens' WorkspaceChoice WorkspaceId
                    secondaryChoiceL k (WorkspaceChoice prim sec ter) = fmap (\newSec -> WorkspaceChoice prim newSec ter) (k sec)

                    tertiaryChoiceL :: Lens' WorkspaceChoice WorkspaceId
                    tertiaryChoiceL k (WorkspaceChoice prim sec ter) = fmap (\newTer -> WorkspaceChoice prim sec newTer) (k ter)

                    data MonitorConfig = SingleMonitor
                                       | DualMonitor
                                       | TripleMonitor ScreenId ScreenId

                    detectMonitorConfig :: WindowSet -> MonitorConfig
                    detectMonitorConfig W.StackSet {W.visible = []} = SingleMonitor
                    detectMonitorConfig W.StackSet {W.visible = [_]} = DualMonitor
                    detectMonitorConfig ss@(W.StackSet {W.visible = (_:_:[])}) = TripleMonitor (chooseSecondary ss) (chooseTertiary ss)
                    detectMonitorConfig _ = SingleMonitor -- No idea how to handle more than 3 monitors =)

                    chooseSecondary :: WindowSet -> ScreenId
                    chooseSecondary W.StackSet { W.visible = visible, W.current = current } =
                      case sorted of
                        _primary:a:b:_ ->
                          case W.screen a of
                            1 -> W.screen b
                            _ -> W.screen a
                        _ -> W.screen current
                      where
                        allScreens = current : visible
                        sorted = sortBy (\x y -> compare (W.screen x) (W.screen y)) allScreens

                    chooseTertiary :: WindowSet -> ScreenId
                    chooseTertiary ss = case chooseSecondary ss of
                                          1 -> 2
                                          2 -> 0
                                          _ -> 0

                    scratchPadPosition :: WindowSet -> ScreenId
                    scratchPadPosition ss = go (detectMonitorConfig ss)
                      where
                        go SingleMonitor = 0
                        go DualMonitor = 1
                        go (TripleMonitor _ ter) = ter

                    placeWorkplaces :: X ()
                    placeWorkplaces = do
                      monConf <- detectMonitorConfig <$> gets windowset
                      WorkspaceChoice prim sec ter <- ES.get
                      case monConf of
                        SingleMonitor -> do
                          switchToPrimary prim
                        DualMonitor -> do
                          switchToSecondary sec
                          switchToPrimary prim
                        TripleMonitor _ _ -> do
                          switchToTertiary ter
                          switchToSecondary sec
                          switchToPrimary prim

                    onRescreen :: X () -> Event -> X All
                    onRescreen u (ConfigureEvent {ev_window = w}) = do
                      rootPred <- isRoot w
                      case rootPred of
                        True -> do
                          rescreen
                          u
                          return (All False)
                        _ -> return (All True)
                    onRescreen _ _ = return (All True)

                    -- TODO: rethink tagging setup
                '';
                onChange = ''xmonad --recompile'';
            };
            ".xmonad/xmonad.hs" = {
                text = ''
                    module Main where

                    import XMonad hiding ((|||))
                    import XMonad.Actions.Navigation2D (withNavigation2DConfig)
                    import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, docks, manageDocks)

                    import Extensions.Xkb (xkbLayout)
                    import XMonad.Hooks.DynamicBars (dynStatusBarEventHook, dynStatusBarStartup)
                    import XMonad.Hooks.EwmhDesktops (ewmhDesktopsEventHook, ewmhDesktopsStartup)
                    import XMonad.Hooks.Place (placeHook)
                    import XMonad.Hooks.XPropManage (xPropManageHook)
                    import XMonad.Util.NamedScratchpad (namedScratchpadManageHook)

                    import qualified Controls as C
                    import qualified Layouts as L
                    import qualified StatusBar as SB
                    import qualified Themes as T
                    import qualified Workspaces as WS


                    configModifiers = withNavigation2DConfig C.customNavigation2DConfig . docks

                    main = do
                      xmonad $ configModifiers def {
                        focusFollowsMouse  = False,
                        borderWidth        = 3,
                        modMask            = mod4Mask,
                        workspaces         = map WS.wsName $ filter WS.wsEnabled $ WS.mergedWorkspaces,
                        normalBorderColor  = T.colorDarkGreen,
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
                                             xPropManageHook WS.xPropMatches <+>
                                             namedScratchpadManageHook C.scratchpads <+>
                                             placeHook WS.placePolicy,
                        startupHook        = WS.placeWorkplaces <+>
                                             ewmhDesktopsStartup <+>
                                             dynStatusBarStartup SB.barCreatorXmobar SB.barDestroyer
                        }
                '';
                onChange = ''xmonad --recompile'';
            };
        };
    };
}
