-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Flow.Utils.Connections
-- License     :  BSD3-style (see the file LICENSE)
--
-- Pure port of @utils\/connections.ts@ from @\@xyflow\/system@.
----------------------------------------------------------------------------
module Miso.Flow.Utils.Connections
  ( areConnectionMapsEqual
  , handleConnectionChange
  , getConnectionStatus
  ) where
-----------------------------------------------------------------------------
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Prelude
-----------------------------------------------------------------------------
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
import           Miso.Flow.Types
-----------------------------------------------------------------------------
-- | Two connection maps hold the same connection keys.
areConnectionMapsEqual
  :: Maybe (Map MisoString HandleConnection)
  -> Maybe (Map MisoString HandleConnection)
  -> Bool
areConnectionMapsEqual Nothing Nothing = True
areConnectionMapsEqual (Just a) (Just b)
  | M.size a /= M.size b = False
  | M.null a && M.null b = True
  | otherwise = all (`M.member` b) (M.keys a)
areConnectionMapsEqual _ _ = False
-----------------------------------------------------------------------------
-- | Connections in @a@ that are not in @b@ (used to fire
-- connect\/disconnect callbacks).
handleConnectionChange
  :: Map MisoString HandleConnection
  -> Map MisoString HandleConnection
  -> [HandleConnection]
handleConnectionChange a b =
  [ c | (k, c) <- M.toList a, not (k `M.member` b) ]
-----------------------------------------------------------------------------
-- | @\'valid\'@ \/ @\'invalid\'@ status string for an ongoing connection.
getConnectionStatus :: Maybe Bool -> Maybe MisoString
getConnectionStatus = \case
  Nothing -> Nothing
  Just True -> Just "valid"
  Just False -> Just "invalid"
-----------------------------------------------------------------------------
