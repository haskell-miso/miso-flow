-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Flow.Utils.Marker
-- License     :  BSD3-style (see the file LICENSE)
--
-- Pure port of @utils\/marker.ts@ from @\@xyflow\/system@.
----------------------------------------------------------------------------
module Miso.Flow.Utils.Marker
  ( getMarkerId
  , createMarkerIds
  ) where
-----------------------------------------------------------------------------
import           Data.List (sortBy)
import           Data.Maybe (mapMaybe)
import           Data.Ord (comparing)
import qualified Data.Set as S
import           Prelude
-----------------------------------------------------------------------------
import           Miso.String (MisoString, intercalate)
-----------------------------------------------------------------------------
import           Miso.Flow.Internal.JSNum (jsShow)
import           Miso.Flow.Types
-----------------------------------------------------------------------------
-- | Stable id for a marker; port of @getMarkerId@. Inline markers are
-- keyed by their sorted @key=value@ fields (matching the TS object-key
-- serialization), marker references use their name directly.
getMarkerId :: Maybe EdgeMarkerType -> Maybe MisoString -> MisoString
getMarkerId Nothing _ = ""
getMarkerId (Just (MarkerRef name)) _ = name
getMarkerId (Just (Marker m)) flowId =
  maybe "" (<> "__") flowId <> intercalate "&" (markerFields m)
-----------------------------------------------------------------------------
-- | The marker's present fields as @key=value@ pairs in (TS
-- @Object.keys(marker).sort()@) alphabetical key order.
markerFields :: EdgeMarker -> [MisoString]
markerFields EdgeMarker {..} =
  mapMaybe render
    -- alphabetical: color, height, markerUnits, orient, strokeWidth, type, width
    [ ("color", markerColor)
    , ("height", jsShow <$> markerHeight)
    , ("markerUnits", markerUnits)
    , ("orient", markerOrient)
    , ("strokeWidth", jsShow <$> markerStrokeWidth)
    , ("type", Just (markerTypeToText markerType))
    , ("width", jsShow <$> markerWidth)
    ]
  where
    render (_, Nothing) = Nothing
    render (k, Just v) = Just (k <> "=" <> v)
-----------------------------------------------------------------------------
-- | All distinct inline markers used by the given edges, with their ids;
-- port of @createMarkerIds@.
createMarkerIds
  :: [Edge e]
  -> Maybe MisoString
  -- ^ flow id
  -> Maybe MisoString
  -- ^ default color
  -> Maybe EdgeMarkerType
  -- ^ default marker start
  -> Maybe EdgeMarkerType
  -- ^ default marker end
  -> [MarkerProps]
createMarkerIds edges flowId defaultColor defaultMarkerStart defaultMarkerEnd =
  sortBy (comparing markerPropsId) (go S.empty edges)
  where
    go _ [] = []
    go seen (e : rest) =
      let candidates =
            [ orDefault (edgeMarkerStart e) defaultMarkerStart
            , orDefault (edgeMarkerEnd e) defaultMarkerEnd
            ]
          (seen', props) = foldl step (seen, []) candidates
      in reverse props <> go seen' rest
    orDefault m d = case m of
      Just x -> Just x
      Nothing -> d
    step (seen, acc) = \case
      Just (Marker m) ->
        let markerId = getMarkerId (Just (Marker m)) flowId
        in if markerId `S.member` seen
             then (seen, acc)
             else
               ( S.insert markerId seen
               , MarkerProps markerId
                   m { markerColor =
                         case markerColor m of
                           Just c -> Just c
                           Nothing -> defaultColor }
                 : acc
               )
      _ -> (seen, acc)
-----------------------------------------------------------------------------
