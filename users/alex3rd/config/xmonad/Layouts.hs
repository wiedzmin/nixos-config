{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

module Layouts where

import XMonad hiding ((|||))
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
