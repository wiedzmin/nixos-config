module Themes where

import XMonad.Layout.Decoration
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)

-- This mimics the "zenburn" style as found for vim and Emacs.
curNormalBGColor      = "#507060" -- Original zenburn: "#60b48a"
curNormalFGColor      = "#dcdccc"
curTitleFGColor       = "#f0dfaf"

-- intended to be an example of all used params
-- should not be used unless filled with appropriate param values
xpConfigFullPH :: XPConfig
xpConfigFullPH = def { font        = "@fontDefault@"
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
  fontName = "@fontTabbed@"
  }
