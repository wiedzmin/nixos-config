{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.wm.xmonad;
  dmenu_runapps = pkgs.writeShellScriptBin "dmenu_runapps" ''
    ${pkgs.j4-dmenu-desktop}/bin/j4-dmenu-desktop --display-binary \
      --dmenu="(${pkgs.coreutils}/bin/cat ; (${pkgs.dmenu}/bin/stest -flx $(echo $PATH | tr : ' ') | sort -u)) | \
      ${if cfg.dmenuFrecency.enable
        then "${pkgs.haskellPackages.yeganesh}/bin/yeganesh -- -i -l 15 -fn '${config.attributes.fonts.dmenu}'\""
        else "${pkgs.dmenu}/bin/dmenu -i -l 15 -fn '${config.attributes.fonts.dmenu}'\""}
  '';
  dmenu_select_windows = pkgs.writeShellScriptBin "dmenu_select_windows" ''
    ${pkgs.wmctrl}/bin/wmctrl -a $(${pkgs.wmctrl}/bin/wmctrl -l | \
                                 ${pkgs.coreutils}/bin/cut -d" " -f5- | \
                                 ${pkgs.dmenu}/bin/dmenu -i -l 15 -fn '${config.attributes.fonts.dmenu}')
  '';
  basicKeys = {
    "C-\\\\" = "sendMessage (XkbToggle Nothing)";
    "M-<Home>" = "toggleWS";
    "M-<Return>" = "promote";
    "M-<Space>" = "sendMessage NextLayout";
    "M-<Tab>" = "windows W.focusDown";
    "M-S-<Space>" = "setLayout $ XMonad.layoutHook conf";
    "M-S-<Tab>" = "windows W.focusUp";
    "M-S-<Return>" = ''spawn "${config.attributes.defaultCommands.terminal}"'';
    "M-S-c" = "kill1";
    "M-S-j" = "windows W.swapDown";
    "M-S-k" = "windows W.swapUp";
    "M-S-q" = "io (exitWith ExitSuccess)";
    "M-b" = ''spawn "${dmenu_select_windows}/bin/dmenu_select_windows"'';
    "M-h" = "sendMessage Shrink";
    "M-l" = "sendMessage Expand";
    "M-m" = "windows W.focusMaster";
    "M-n" = "refresh";
    "M-t" = "withFocused $ windows . W.sink";
    "M-x a" = "windows copyToAll"; # @@ Make focused window always visible
    "M-x k" = "killAllOtherCopies"; # @@ Toggle window state back
    "M-r" = "placeWorkplaces";
    "M-C-q" = ''spawn "xmonad --recompile; xmonad --restart"'';
    "M-q" = ''spawn "xmonad --restart"'';
    "M-S-p" = ''spawn "${dmenu_runapps}/bin/dmenu_runapps"'';
    "M-a 1" = ''namedScratchpadAction scratchpads "htop"'';
    "M-a 2" = ''namedScratchpadAction scratchpads "iotop"'';
    "M-a 3" = ''namedScratchpadAction scratchpads "gotop"'';
    "M-a 4" = ''namedScratchpadAction scratchpads "bc"'';
    # -- TODO: recall and add "multiple app windows"-aware raising
    "M-w <Backspace>" = ''nextMatch History (return True)'';
    "<XF86Launch1>" = ''spawn "${dmenu_runapps}/bin/dmenu_runapps"'';
    "M-S-w M-S-n" = "moveTo Next EmptyWS"; # find a free workspace
    "M-S-w M-S-<Up>" = "shiftToPrev";
    "M-S-w M-S-<Down>" = "shiftToNext";
    "M-S-w M-S-<Right>" = "shiftToNext >> nextWS";
    "M-S-w M-S-<Left>" = "shiftToPrev >> prevWS";
    "M-S-w <Up>" = "prevWS";
    "M-S-w <Down>" = "nextWS";
    "M-w M-s" = "sinkAll";
    "M-S-." = "placeFocused placePolicy";
    "M-<Right>" = "windowGo R True";
    "M-<Left>" = "windowGo L True";
    "M-<Up>" = "windowGo U True";
    "M-<Down>" = "windowGo D True";
  };
  extraCombinators = ''
    -----------------------------------------------------------------
    -- |
    -- Module  : ~/.xmonad/lib/ExtraCombinators
    -- License : BSD3
    --
    -- Missing operators commonly imported or defined locally:
    --
    -- first, second, (>>>); infixr 1 >>>, and aliases for (.) and fmap from
    --
    -- <http://conal.net/blog/posts/semantic-editor-combinators> (SECs)
    --
    -- plus few other similar operators, aliases, and re-exports.
    -----------------------------------------------------------------

    module XMonad.Util.ExtraCombinators where

    import Control.Applicative ((<$>),(<*>))
    import Control.Arrow (first,second,(>>>))
    import Control.Monad (liftM2)
    import Data.Monoid (Monoid(..),mappend,mempty,mconcat)

    import XMonad
    import qualified XMonad.StackSet as W


    -- Semantic Editor Combinators (SECs) {{{
    -- | SEC alias for (.)
    result ::  (b -> c) -> (a -> b) -> a -> c
    result = (.)

    -- | SEC alias for fmap
    element ::  (Functor f) => (a -> b) -> f a -> f b
    element = fmap
    --}}}

    -- | Infix bracketless @(,)@
    -- <http://mauke.ath.cx/stuff/xmonad/xmonad.hs>
    infixr 0 ~>
    (~>) :: a -> b -> (a, b)
    (~>) = (,)

    -- | Less aggressive @$@
    infixr 2 $.
    ($.) :: (a -> b) -> a -> b
    ($.) = id

    -- | Infix @mappend@
    infixr 4 <>
    (<>) ::  (Monoid a) => a -> a -> a
    (<>) = mappend

    -- | Infix version of @flip (++)@
    infixr 5 +\+
    (+\+) :: [a] -> [a] -> [a]
    (+\+) = flip (++)

    -- | Prefix alias for @flip (++)@
    append ::  [a] -> [a] -> [a]
    append = (+\+)

    -- | Prefix alias for @(++)@
    prepend ::  [a] -> [a] -> [a]
    prepend = (++)

    -- TBD but very cool looking
    infixr 2 ...
    -- as fmap . fmap :: (Functor f, Functor f1) => (a -> b) -> f (f1 a) -> f (f1 b)
    -- as (.)(.) ::  (a1 -> b -> c) -> a1 -> (a -> b) -> a -> c
    -- as liftM2 (.) ::  (Monad m) => m (b -> c) -> m (a -> b) -> m (a -> c)
    (...) ::  a
    (...) = undefined
  '';
  windowTypes = ''
    module XMonad.Util.WindowTypes where

    import XMonad

    import Graphics.X11.Xlib.Extras

    import Foreign.C.Types (CLong)

    getProp :: Atom -> Window -> X (Maybe [CLong])
    getProp a w = withDisplay $ \dpy -> io $ getWindowProperty32 dpy a w

    checkAtom name value = ask >>= \w -> liftX $ do
              a <- getAtom name
              val <- getAtom value
              mbr <- getProp a w
              case mbr of
                Just [r] -> return $ elem (fromIntegral r) [val]
                _ -> return False

    checkDialog = checkAtom "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DIALOG"
    checkMenu = checkAtom "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_MENU"

    -- TODO: since this code is 10 years old, check if it was finally made its way to contrib
    -- reference module is probably XMonad.Hooks.ManageHelpers
  '';
  xkbToggle = ''
    {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

    module XMonad.Util.Xkb where

    import XMonad
    import qualified Data.Map        as M
    import qualified XMonad.StackSet as W


    data XkbToggle = XkbToggle (Maybe XID) deriving ( Typeable )
    instance Message XkbToggle
    data XkbLayout l a = XkbLayout XID (M.Map Window XID) (l a) deriving ( Read, Show )

    xkbLayout = XkbLayout 0 M.empty

    instance (Ord a, LayoutClass l a) => LayoutClass (XkbLayout l) a where
        runLayout (W.Workspace tag m@(XkbLayout defXkbGroup xkbWinMap l) ms) r =
            do withWindowSet $ \ws ->
                   if tag == W.currentTag ws
                   then whenJust (W.peek ws) (lockGroup m)
                   else return ()
               (ws, l') <- runLayout (W.Workspace tag l ms) r
               case l' of
                 Nothing -> return (ws, Nothing)
                 Just l -> return (ws, Just (XkbLayout defXkbGroup xkbWinMap l))

        handleMessage xkb@(XkbLayout defXkbGroup xkbWinMap l) m
            | Just (XkbToggle group) <- fromMessage m = withWindowSet (return . W.peek) >>= maybe (return Nothing) (toggleGroup xkb group)
            | otherwise = (handleMessage l m) >>= maybe (return Nothing) (\l -> return $ Just (XkbLayout defXkbGroup xkbWinMap l))

        description (XkbLayout _ _ l) = description l

    xkbGetGroup :: (Ord b) => XkbLayout a b -> Window -> XID
    xkbGetGroup (XkbLayout d m _) w  = M.findWithDefault d w m

    foreign import ccall unsafe "XkbLockGroup" xkbLockGroup :: Display -> XID -> XID ->IO ()

    lockGroup :: (Ord b) => XkbLayout a b -> Window -> X ()
    lockGroup l@(XkbLayout d m _) w = withDisplay $ \dpy -> io $ xkbLockGroup dpy 0x100 (xkbGetGroup l w)

    toggleGroup :: (Ord b) => XkbLayout a b -> Maybe XID -> Window -> X (Maybe (XkbLayout a b))
    toggleGroup l@(XkbLayout d m l') (Just g) w = return $ Just (XkbLayout d (M.insert w g m) l')
    toggleGroup l@(XkbLayout d m l') Nothing w =
        do XConf { display = dpy, theRoot = root } <- ask
           classHint <- io $ getClassHint dpy w
           if resClass classHint == "Emacs"
            then do io $ allocaXEvent $ \ev ->
                       do setEventType ev keyPress
                          keyCode <- keysymToKeycode dpy xK_backslash
                          setKeyEvent ev w root 0 controlMask keyCode True
                          sendEvent dpy w False 0 ev
                    return Nothing
            else if resClass classHint == "Conkeror"
             then do io $ allocaXEvent $ \ev ->
                       do setEventType ev keyPress
                          keyCode <- keysymToKeycode dpy xK_backslash
                          setKeyEvent ev w root 0 controlMask keyCode True
                          sendEvent dpy w False 0 ev
                     return Nothing
             else return $ Just (XkbLayout d (M.insert w (flop $ xkbGetGroup l w) m) l')
                   where flop 0 = 1
                         flop _ = 0
  '';
  # TODO: extract parameters later
  configText = ''
    module Main where

    import Control.Lens
    import Control.Monad (liftM2, when)
    import Data.List (isInfixOf, isPrefixOf, sortBy)
    import Data.Maybe (fromJust, fromMaybe, isJust, isNothing, maybeToList)
    import Data.Monoid (All(..))
    import System.Exit (ExitCode(ExitSuccess), exitWith)
    import Data.Map (fromList)
    import Data.Text (pack, replace, unpack)

    import XMonad hiding ((|||))
    import qualified XMonad.StackSet as W

    import XMonad.Actions.CopyWindow (copyToAll, kill1, killAllOtherCopies)
    import XMonad.Actions.CopyWindow (wsContainingCopies)
    import XMonad.Actions.CycleWS (Direction1D(Next), Direction1D(Prev), WSType(EmptyWS), WSType(NonEmptyWS),
                                   findWorkspace, moveTo, nextWS, prevWS, shiftToNext, shiftToPrev, toggleWS)
    import XMonad.Actions.DynamicWorkspaceOrder (getSortByOrder)
    import XMonad.Actions.DynamicWorkspaces (removeEmptyWorkspace)
    import XMonad.Actions.GroupNavigation (Direction(History), historyHook, nextMatch)
    import XMonad.Actions.Navigation2D (Direction2D(D), Direction2D(L), Direction2D(R), Direction2D(U),
                                        centerNavigation, defaultTiledNavigation, hybridOf, sideNavigation,
                                        windowGo, withNavigation2DConfig)
    import XMonad.Actions.OnScreen (Focus(FocusNew), greedyViewOnScreen, onScreen)
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
    import XMonad.Util.WorkspaceCompare (getSortByIndex)
    import XMonad.Util.XUtils (fi)
    import XMonad.Util.Xkb
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
      , ("tools", Just "<F4>", True, True)
      , ("scan", Just "<F5>", True, True)
      ]

    secondaryWorkspaces =
      [ ("shell", Just "<F3>", True, True)
      , ("read", Just "4", True, True)
      , ("media", Just "5", True, True)
      , ("im", Just "3", True, True)
      , ("web2", Just "1", True, True)
      , ("web3", Just "`", True, True)
      , ("work2", Just "2", True, True)
      ]

    scratchpadWorkspace :: WSData
    scratchpadWorkspace = ("scratch", Just "<Esc>", True, True)

    xPropMatches :: [XPropMatch]
    xPropMatches = [ ([ (wM_CLASS, any ("Alacritty" ==))], pmP (viewShift "shell"))
                   , ([ (wM_CLASS, any ("Apvlv" ==))], pmP (viewShift "read"))
                   , ([ (wM_CLASS, any ("Chromium-browser" ==))], pmP (viewShift "web"))
                   , ([ (wM_CLASS, any ("Code" ==))], pmP (viewShift "work"))
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
                   , ([ (wM_CLASS, any ("Skype" ==))], pmP (viewShift "im"))
                   , ([ (wM_CLASS, any ("Slack" ==))], pmP (viewShift "im"))
                   , ([ (wM_CLASS, any ("Soffice" ==))], pmP (viewShift "read"))
                   , ([ (wM_CLASS, any ("TelegramDesktop" ==))], pmP (viewShift "im"))
                   , ([ (wM_CLASS, any ("URxvt" ==))], pmP (viewShift "shell"))
                   , ([ (wM_CLASS, any ("Virt-manager" ==))], pmP (viewShift "tools"))
                   , ([ (wM_CLASS, any ("Virt-viewer" ==))], pmP (viewShift "tools"))
                   , ([ (wM_CLASS, any ("VirtualBox Manager" ==))], pmP (viewShift "tools"))
                   , ([ (wM_CLASS, any ("Wire" ==))], pmP (viewShift "im"))
                   , ([ (wM_CLASS, any ("Xsane" ==))], pmP (viewShift "scan"))
                   , ([ (wM_CLASS, any ("aft-linux-qt" ==))], pmP (viewShift "scratch"))
                   , ([ (wM_CLASS, any ("athura" `isInfixOf`))], pmP (viewShift "read"))
                   , ([ (wM_CLASS, any ("calibre" ==))], pmP (viewShift "read"))
                   , ([ (wM_CLASS, any ("com-eviware-soapui-SoapUI" ==))], pmP (viewShift "tools"))
                   , ([ (wM_CLASS, any ("glogg" ==))], pmP (viewShift "scratch"))
                   , ([ (wM_CLASS, any ("llpp" ==))], pmP (viewShift "read"))
                   , ([ (wM_CLASS, any ("mpv" ==))], pmP (viewShift "media"))
                   , ([ (wM_CLASS, any ("polar-bookshelf" ==))], pmP (viewShift "read"))
                   , ([ (wM_CLASS, any ("psi" ==))], pmP (viewShift "im"))
                   , ([ (wM_CLASS, any ("scantailor-advanced" ==))], pmP (viewShift "scan"))
                   , ([ (wM_CLASS, any ("zoom" `isPrefixOf`))], pmP (viewShift "im"))
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

    tabbedLayout = Tabs.tabbed Tabs.shrinkText Tabs.def
    dwmLayout = Dwm.dwmStyle Dwm.shrinkText Dwm.def {
      ${lib.optionalString (cfg.font != "") "fontName = \"${cfg.font}\""}
    }

    layouts = onWorkspace "scratch" (renamed [Replace "tabs"] tabbedLayout) $
              onWorkspace "im" (renamed [Replace "tabs"] tabbedLayout) $
              onWorkspace "web" (renamed [Replace "OneBig"] (dwmLayout (OneBig (3/4) (3/4)))) $
              onWorkspace "web2" (StackTile 1 (3/100) (1/2)) $
              onWorkspace "web3" (StackTile 1 (3/100) (1/2)) $
              onWorkspace "work" (renamed [Replace "OneBig"] (dwmLayout (OneBig (3/4) (3/4))) |||
                                  renamed [Replace "Dishes"] (dwmLayout (MultiDishes 2 3 (1/6))) |||
                                  renamed [Replace "Grid"] (dwmLayout Grid) |||
                                  renamed [Replace "Tiled"] (dwmLayout (autoMaster 1 (1/100)
                                                              (HT.HintedTile 1 (3/100) (1/2) HT.TopLeft HT.Tall)))) $
              onWorkspace "shell" (renamed [Replace "OneBig"] (dwmLayout (OneBig (3/4) (3/4)))) $
                                   renamed [Replace "OneBig"] (OneBig (3/4) (3/4))

    layoutMappings = [ ("g", "Grid")
                     , ("b", "OneBig")
                     , ("d", "Dishes")
                     , ("t", "Tiled")
                     ]

    replaceSubstring :: String -> String -> String -> String
    replaceSubstring what to src = unpack (replace (pack $ take 1 what) (pack $ take 1 to) (pack src))

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

    customNavigation2DConfig = def { defaultTiledNavigation = hybridOf sideNavigation centerNavigation }

    scratchpadTerminal = "alacritty"
    scratchpads = [ NS "htop" (scratchpadTerminal ++ " -t htop -e htop") (title =? "htop") nonFloating
                  , NS "iotop" (scratchpadTerminal ++ " -t iotop -e sudo iotop") (title =? "iotop") nonFloating
                  , NS "gotop" (scratchpadTerminal ++ " -t gotop -e gotop") (title =? "gotop") nonFloating
                  , NS "bc" (scratchpadTerminal ++ " -t calc -e bc") (title =? "calc") nonFloating
                  ]

    customKeys conf = [
               ${lib.concatStringsSep "\,\n           "
                   (lib.mapAttrsToList (keys: command: ''"${keys}" ~> ${command}'') basicKeys)},
               ${lib.concatStringsSep "\,\n           "
                   (lib.mapAttrsToList (keys: command: ''"${keys}" ~> ${command}'') cfg.keybindings)}
               ]

    layoutKeys = [ "M-; " ++ keys ~> sendMessage $ JumpToLayout $ layout | (keys, layout) <- layoutMappings ]

    switchScreenKeys = [ "M-" ++ m ++ key ~> f sc
                       | (f, m) <- [(viewScreen naturalScreenOrderer, "C-"), (sendToScreen def, "M1-")]
                       , (key, sc) <- [ ("<Down>", 0)
                                      , ("<Up>", 1)
                                      , ("<Right>", 2) ]]

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
                               | (name, key) <- map wsMapping $ filter wsMapped [scratchpadWorkspace]]

    myKeys = \conf -> mkKeymap conf $ customKeys conf ++ layoutKeys ++ switchScreenKeys ++ switchWorkspaceKeys conf

    ------------------------------------------------------------------------
    -- Mouse bindings: default actions bound to mouse events
    --
    myMouseBindings (XConfig {XMonad.modMask = modMask}) = fromList $
                    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
                    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
                    , ((modMask, button3), (\w -> focus w >> Flex.mouseWindow Flex.linear w))
                    ]

    -- TODO: try to find a block of keys for http://hackage.haskell.org/package/xmonad-contrib-0.14/docs/XMonad-Layout-WindowArranger.html
    -- TODO: try to find a block of keys for http://hackage.haskell.org/package/xmonad-contrib-0.15/docs/XMonad-Layout-BinarySpacePartition.html


    barCreatorXmobar :: DynamicStatusBar
    barCreatorXmobar (S sid) = do
        spawnPipe ("${pkgs.xmobar}/bin/xmobar $HOME/.config/xmobar/xmobarrc --screen " ++ show sid)

    barDestroyer :: DynamicStatusBarCleanup
    barDestroyer = return ()

    configModifiers = withNavigation2DConfig customNavigation2DConfig . docks

    manageMenus = checkMenu --> doFloat
    manageDialogs = checkDialog --> doFloat

    main = do
      xmonad $ configModifiers def {
        focusFollowsMouse  = False,
        borderWidth        = 3,
        modMask            = mod4Mask,
        workspaces         = map wsName $ filter wsEnabled $ mergedWorkspaces,
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
  '';
in {
  options = {
    wm.xmonad = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable xmonad.
        '';
      };
      dmenuFrecency.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Frecency trakcing for Dmenu with Yeganesh.
        '';
      };
      font = mkOption {
        type = types.str;
        default = "";
        description = ''
          XMonad `internal` font' definition.
        '';
      };
      keybindings = mkOption {
        type = types.attrs;
        default = {};
        description = ''
          XMonad keybindings.
        '';
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      services.xserver = {
        windowManager = {
          xmonad = {
            enable = true;
            enableContribAndExtras = true;
            extraPackages = p: [ p.dbus p.monad-logger p.lens p.split ];
          };
        };
        displayManager = {
          defaultSession = "none+xmonad";
        };
      };
      environment.systemPackages = with pkgs; [ haskellPackages.xmobar ];
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.file = {
          ".xmonad/lib/XMonad/Util/ExtraCombinators.hs".text = extraCombinators;
          ".xmonad/lib/XMonad/Util/WindowTypes.hs".text = windowTypes;
          ".xmonad/lib/XMonad/Util/Xkb.hs".text = xkbToggle;
          ".xmonad/xmonad.hs" = {
            text = configText;
            onChange = "xmonad --recompile";
          };
        };
        home.packages = with pkgs; [ dmenu_runapps ];
        xdg.configFile."xmobar/xmobarrc".text = ''
          Config { ${lib.optionalString (config.attributes.fonts.xmobar != "") ''font = "${config.attributes.fonts.xmobar}"''}
                 , bgColor = "black"
                 , fgColor = "grey"
                 , position = TopW L 100
                 , lowerOnStart = False
                 , allDesktops = True
                 , persistent = True
                 , commands = [ Run Date "%a %d/%m/%y %H:%M:%S" "date" 10
                              , Run StdinReader
                              , Run BatteryP ["BAT0"] ["-t", "<acstatus><left>%(<timeleft>)", "-L", "10", "-H", "80", "-p", "3", "--", "-O",
                                                       "<fc=green>▲</fc>", "-i", "<fc=green>=</fc>", "-o", "<fc=yellow>▼</fc>",
                                                       "-L", "-15", "-H", "-5", "-l", "red", "-m", "blue", "-h", "green"] 200
                              , Run Com "wifi-status" [] "wifi" 60
                              , Run Kbd [ ("us", "<fc=#ee9a00>us</fc>")
                                        , ("ru", "<fc=green>ru</fc>")
                                        ]
                              ]
                 , sepChar = "%"
                 , alignSep = "}{"
                 , template = "%StdinReader% }{| %battery% | %wifi% | <fc=#ee9a00>%date%</fc> |%kbd%"
                 }
        '';
      };
    })
    (mkIf cfg.dmenuFrecency.enable {
      environment.systemPackages = with pkgs; [ dmenu ];
    })
  ];
}
