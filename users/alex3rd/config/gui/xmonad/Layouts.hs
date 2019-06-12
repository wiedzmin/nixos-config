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
          onWorkspace "web" (renamed [Replace "OneBig"] (OneBig (3/4) (3/4))) $
          onWorkspace "work" (renamed [Replace "OneBig"] (OneBig (3/4) (3/4)) |||
                              renamed [Replace "Dishes"] (Dishes 2 (1/6)) |||
                              renamed [Replace "Grid"] Grid |||
                              renamed [Replace "Tiled"] (autoMaster 1 (1/100) customHintedTile)) $
          onWorkspace "shell" (renamed [Replace "OneBig"] (OneBig (3/4) (3/4))) $
          renamed [Replace "OneBig"] (OneBig (3/4) (3/4))

layoutMappings = [ ("g", "Grid")
                 , ("b", "OneBig")
                 , ("d", "Dishes")
                 , ("t", "Tiled")
                 ]
