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

-- vim:foldmethod=marker
