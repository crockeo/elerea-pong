module Utils where

--------------------
-- Global Imports --
import FRP.Elerea.Param

----------
-- Code --

-- | Mapping a function over a @'SignalGen'@ of a @'Signal'@.
signalMap :: (a -> b) -> SignalGen p (Signal a) -> SignalGen p (Signal b)
signalMap fn s = fmap (fmap fn) s
