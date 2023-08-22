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
