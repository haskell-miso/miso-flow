-----------------------------------------------------------------------------
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Flow.Types
-- License     :  BSD3-style (see the file LICENSE)
--
-- Core types for miso-flow. This is a Haskell port of the type layer of
-- @\@xyflow\/system@ (@types\/utils.ts@, @types\/general.ts@,
-- @types\/nodes.ts@, @types\/edges.ts@, @types\/handles.ts@,
-- @types\/panzoom.ts@, @types\/changes.ts@).
--
-- Numeric fields are 'Double' (JS @number@), strings are 'MisoString',
-- optional fields are 'Maybe'.
----------------------------------------------------------------------------
module Miso.Flow.Types
  ( -- * Geometry (@types\/utils.ts@)
    Position (..)
  , oppositePosition
  , positionToText
  , positionFromText
  , XYPosition (..)
  , xy
  , zeroPosition
  , XYZPosition (..)
  , Dimensions (..)
  , zeroDimensions
  , Rect (..)
  , Box (..)
  , Transform
  , CoordinateExtent (..)
  , NodeExtent (..)
  , coordinateExtent
    -- * Viewport (@types\/general.ts@)
  , Viewport (..)
  , defaultViewport
  , viewportToTransform
  , transformToViewport
  , SnapGrid (..)
  , PanelPosition (..)
  , panelPositionToText
  , SelectionMode (..)
  , SelectionRect (..)
  , PanOnScrollMode (..)
  , PanOnDrag (..)
  , ColorMode (..)
  , ZIndexMode (..)
  , Align (..)
    -- * Connections (@types\/general.ts@)
  , Connection (..)
  , connection
  , HandleConnection (..)
  , NodeConnection
  , ConnectionMode (..)
  , ConnectionState (..)
  , ConnectionInProgress (..)
  , FinalConnectionState (..)
  , noConnection
  , connectionInProgress
  , OnConnectStartParams (..)
    -- * Handles (@types\/handles.ts@)
  , HandleType (..)
  , handleTypeToText
  , handleTypeFromText
  , Handle (..)
  , NodeHandle (..)
  , nodeHandle
    -- * Nodes (@types\/nodes.ts@)
  , NodeId
  , Node (..)
  , node
  , Measured (..)
  , noMeasure
  , measuredToDimensions
  , NodeOrigin (..)
  , defaultOrigin
  , InternalNode (..)
  , internalUserNode
  , NodeHandleBounds (..)
  , NodeBounds (..)
  , NodeDragItem (..)
  , NodeLookup
  , ParentLookup
  , InternalNodeUpdate (..)
    -- * Edges (@types\/edges.ts@)
  , EdgeId
  , Edge (..)
  , edge
  , ConnectionLineType (..)
  , EdgeMarker (..)
  , edgeMarker
  , EdgeMarkerType (..)
  , MarkerType (..)
  , markerTypeToText
  , MarkerProps (..)
  , EdgePosition (..)
  , EdgeLookup
  , ConnectionLookup
    -- * Path options (@types\/edges.ts@)
  , BezierPathOptions (..)
  , defaultBezierPathOptions
  , SmoothStepPathOptions (..)
  , defaultSmoothStepPathOptions
  , StepPathOptions (..)
    -- * Padding & fit view (@types\/general.ts@)
  , PaddingWithUnit (..)
  , Padding (..)
  , defaultFitViewPadding
  , FitViewOptions (..)
  , defaultFitViewOptions
  , ViewportHelperOptions (..)
  , defaultViewportHelperOptions
  , Interpolation (..)
  , SetCenterOptions (..)
  , defaultSetCenterOptions
  , FitBoundsOptions (..)
    -- * Changes (@types\/changes.ts@)
  , NodeChange (..)
  , SetAttributes (..)
  , EdgeChange (..)
    -- * Resizer types (@xyresizer\/types.ts@)
  , ControlPosition (..)
  , controlPositionToText
  , ControlLinePosition (..)
  , ResizeControlVariant (..)
  , ResizeControlDirection (..)
  , ResizeParams (..)
  , ResizeParamsWithDirection (..)
  , xyResizerHandlePositions
  , xyResizerLinePositions
    -- * Misc
  , ErrorCode
  , OnError
  ) where
-----------------------------------------------------------------------------
import           Control.Applicative ((<|>))
import           Data.Map.Strict (Map)
import           Data.Maybe (catMaybes)
import           GHC.Generics (Generic)
import           Prelude
-----------------------------------------------------------------------------
import           Miso.JSON
  ( FromJSON (..)
  , ToJSON (..)
  , Value (..)
  , object
  , withArray
  , withObject
  , withText
  , (.:)
  , (.:?)
  , (.=)
  )
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
-- | Side of a node an edge or handle attaches to.
--
-- Port of the @Position@ enum in @types\/utils.ts@.
data Position
  = PositionLeft
  | PositionTop
  | PositionRight
  | PositionBottom
  deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
-- | Port of @oppositePosition@.
oppositePosition :: Position -> Position
oppositePosition = \case
  PositionLeft   -> PositionRight
  PositionRight  -> PositionLeft
  PositionTop    -> PositionBottom
  PositionBottom -> PositionTop
-----------------------------------------------------------------------------
-- | Wire representation used by xyflow (@\'left\' | \'top\' | \'right\' | \'bottom\'@).
positionToText :: Position -> MisoString
positionToText = \case
  PositionLeft   -> "left"
  PositionTop    -> "top"
  PositionRight  -> "right"
  PositionBottom -> "bottom"
-----------------------------------------------------------------------------
positionFromText :: MisoString -> Maybe Position
positionFromText = \case
  "left"   -> Just PositionLeft
  "top"    -> Just PositionTop
  "right"  -> Just PositionRight
  "bottom" -> Just PositionBottom
  _        -> Nothing
-----------------------------------------------------------------------------
-- | A point in the flow coordinate system.
data XYPosition = XYPosition
  { xyX :: !Double
  , xyY :: !Double
  } deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
-- | Shorthand constructor.
xy :: Double -> Double -> XYPosition
xy = XYPosition
-----------------------------------------------------------------------------
zeroPosition :: XYPosition
zeroPosition = XYPosition 0 0
-----------------------------------------------------------------------------
data XYZPosition = XYZPosition
  { xyzX :: !Double
  , xyzY :: !Double
  , xyzZ :: !Double
  } deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
data Dimensions = Dimensions
  { dimensionsWidth  :: !Double
  , dimensionsHeight :: !Double
  } deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
zeroDimensions :: Dimensions
zeroDimensions = Dimensions 0 0
-----------------------------------------------------------------------------
-- | @Rect = Dimensions & XYPosition@.
data Rect = Rect
  { rectX      :: !Double
  , rectY      :: !Double
  , rectWidth  :: !Double
  , rectHeight :: !Double
  } deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
-- | A box given by its top-left and bottom-right corners.
data Box = Box
  { boxX  :: !Double
  , boxY  :: !Double
  , boxX2 :: !Double
  , boxY2 :: !Double
  } deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
-- | In @\@xyflow\/system@ a @Transform@ is the tuple @[x, y, zoom]@ — the
-- same three numbers as a 'Viewport'; only the intent differs.
type Transform = Viewport
-----------------------------------------------------------------------------
-- | Two points describing the top-left and bottom-right corners of a
-- bounding area.
data CoordinateExtent = CoordinateExtent
  { extentMinX :: !Double
  , extentMinY :: !Double
  , extentMaxX :: !Double
  , extentMaxY :: !Double
  } deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
-- | Smart constructor from the TS nested-pair shape @[[x1,y1],[x2,y2]]@.
coordinateExtent :: (Double, Double) -> (Double, Double) -> CoordinateExtent
coordinateExtent (x1, y1) (x2, y2) = CoordinateExtent x1 y1 x2 y2
-----------------------------------------------------------------------------
-- | A node's @extent@ field: either clamped to its parent or to explicit
-- coordinates (TS: @\'parent\' | CoordinateExtent@).
data NodeExtent
  = ExtentParent
  | ExtentCoordinates !CoordinateExtent
  deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
-- | Position and zoom of the flow pane.
data Viewport = Viewport
  { viewportX    :: !Double
  , viewportY    :: !Double
  , viewportZoom :: !Double
  } deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
defaultViewport :: Viewport
defaultViewport = Viewport 0 0 1
-----------------------------------------------------------------------------
-- | 'Transform' and 'Viewport' are the same three numbers in this port.
viewportToTransform :: Viewport -> Transform
viewportToTransform = id
-----------------------------------------------------------------------------
transformToViewport :: Transform -> Viewport
transformToViewport = id
-----------------------------------------------------------------------------
data SnapGrid = SnapGrid
  { snapGridX :: !Double
  , snapGridY :: !Double
  } deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
-- | Corner \/ side of the pane used by panel-like components.
data PanelPosition
  = TopLeft | TopCenter | TopRight
  | BottomLeft | BottomCenter | BottomRight
  | CenterLeft | CenterRight
  deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
panelPositionToText :: PanelPosition -> MisoString
panelPositionToText = \case
  TopLeft      -> "top-left"
  TopCenter    -> "top-center"
  TopRight     -> "top-right"
  BottomLeft   -> "bottom-left"
  BottomCenter -> "bottom-center"
  BottomRight  -> "bottom-right"
  CenterLeft   -> "center-left"
  CenterRight  -> "center-right"
-----------------------------------------------------------------------------
data SelectionMode
  = SelectionPartial
  | SelectionFull
  deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
data SelectionRect = SelectionRect
  { selectionRect   :: !Rect
  , selectionStartX :: !Double
  , selectionStartY :: !Double
  } deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
data PanOnScrollMode
  = PanOnScrollFree
  | PanOnScrollVertical
  | PanOnScrollHorizontal
  deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
-- | TS: @panOnDrag: boolean | number[]@ — enabled\/disabled, or the list
-- of mouse buttons that pan.
data PanOnDrag
  = PanOnDrag !Bool
  | PanOnDragButtons ![Int]
  deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
data ColorMode
  = ColorModeLight
  | ColorModeDark
  | ColorModeSystem
  deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
-- | How z-indexes of nodes and edges are computed.
data ZIndexMode
  = ZIndexAuto
  | ZIndexBasic
  | ZIndexManual
  deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
data Align
  = AlignCenter
  | AlignStart
  | AlignEnd
  deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
-- | Minimal description of an edge between two nodes.
data Connection = Connection
  { connectionSource       :: !NodeId
  , connectionTarget       :: !NodeId
  , connectionSourceHandle :: !(Maybe MisoString)
  , connectionTargetHandle :: !(Maybe MisoString)
  } deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
-- | Handle-less connection between two nodes.
connection :: NodeId -> NodeId -> Connection
connection s t = Connection s t Nothing Nothing
-----------------------------------------------------------------------------
-- | A 'Connection' together with the id of the edge realizing it.
data HandleConnection = HandleConnection
  { handleConnection       :: !Connection
  , handleConnectionEdgeId :: !EdgeId
  } deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
type NodeConnection = HandleConnection
-----------------------------------------------------------------------------
-- | @Strict@ only allows source-to-target edges; @Loose@ also allows
-- source-to-source and target-to-target.
data ConnectionMode
  = ConnectionModeStrict
  | ConnectionModeLoose
  deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
data OnConnectStartParams = OnConnectStartParams
  { onConnectStartNodeId     :: !(Maybe NodeId)
  , onConnectStartHandleId   :: !(Maybe MisoString)
  , onConnectStartHandleType :: !(Maybe HandleType)
  } deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
-- | Everything known about an ongoing connection gesture.
data ConnectionInProgress n = ConnectionInProgress
  { cipIsValid      :: !(Maybe Bool)
  , cipFrom         :: !XYPosition
  , cipFromHandle   :: !Handle
  , cipFromPosition :: !Position
  , cipFromNode     :: !(InternalNode n)
  , cipTo           :: !XYPosition
  , cipToHandle     :: !(Maybe Handle)
  , cipToPosition   :: !Position
  , cipToNode       :: !(Maybe (InternalNode n))
  , cipPointer      :: !XYPosition
  } deriving (Show, Eq, Generic, Functor)
-----------------------------------------------------------------------------
-- | State of the connection gesture; port of @ConnectionState@.
data ConnectionState n
  = NoConnection
  | InProgress !(ConnectionInProgress n)
  deriving (Show, Eq, Generic, Functor)
-----------------------------------------------------------------------------
noConnection :: ConnectionState n
noConnection = NoConnection
-----------------------------------------------------------------------------
connectionInProgress :: ConnectionState n -> Bool
connectionInProgress = \case
  NoConnection -> False
  InProgress {} -> True
-----------------------------------------------------------------------------
-- | Connection state handed to @onConnectEnd@: like 'ConnectionInProgress'
-- but @toPosition@ may be absent when the gesture did not end on a handle.
data FinalConnectionState n = FinalConnectionState
  { finalIsValid      :: !(Maybe Bool)
  , finalFrom         :: !(Maybe XYPosition)
  , finalFromHandle   :: !(Maybe Handle)
  , finalFromPosition :: !(Maybe Position)
  , finalFromNode     :: !(Maybe (InternalNode n))
  , finalTo           :: !(Maybe XYPosition)
  , finalToHandle     :: !(Maybe Handle)
  , finalToPosition   :: !(Maybe Position)
  , finalToNode       :: !(Maybe (InternalNode n))
  } deriving (Show, Eq, Generic, Functor)
-----------------------------------------------------------------------------
data HandleType
  = SourceHandle
  | TargetHandle
  deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
handleTypeToText :: HandleType -> MisoString
handleTypeToText = \case
  SourceHandle -> "source"
  TargetHandle -> "target"
-----------------------------------------------------------------------------
handleTypeFromText :: MisoString -> Maybe HandleType
handleTypeFromText = \case
  "source" -> Just SourceHandle
  "target" -> Just TargetHandle
  _        -> Nothing
-----------------------------------------------------------------------------
-- | A measured handle attached to a node; port of @Handle@ in
-- @types\/handles.ts@. @x@\/@y@ are relative to the node.
data Handle = Handle
  { hId       :: !(Maybe MisoString)
  , hNodeId   :: !NodeId
  , hX        :: !Double
  , hY        :: !Double
  , hPosition :: !Position
  , hType     :: !HandleType
  , hWidth    :: !Double
  , hHeight   :: !Double
  } deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
-- | User-declared handle on a 'Node' (@handles@ field); width\/height are
-- optional (defaulted to 1 by the store).
data NodeHandle = NodeHandle
  { nhId       :: !(Maybe MisoString)
  , nhX        :: !Double
  , nhY        :: !Double
  , nhPosition :: !Position
  , nhType     :: !HandleType
  , nhWidth    :: !(Maybe Double)
  , nhHeight   :: !(Maybe Double)
  } deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
nodeHandle :: Double -> Double -> Position -> HandleType -> NodeHandle
nodeHandle x y p t = NodeHandle Nothing x y p t Nothing Nothing
-----------------------------------------------------------------------------
type NodeId = MisoString
-----------------------------------------------------------------------------
-- | Optionally measured dimensions (@measured@ field).
data Measured = Measured
  { measuredWidth  :: !(Maybe Double)
  , measuredHeight :: !(Maybe Double)
  } deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
noMeasure :: Measured
noMeasure = Measured Nothing Nothing
-----------------------------------------------------------------------------
measuredToDimensions :: Measured -> Maybe Dimensions
measuredToDimensions (Measured (Just w) (Just h)) = Just (Dimensions w h)
measuredToDimensions _ = Nothing
-----------------------------------------------------------------------------
-- | Origin of a node relative to its position: @(0,0)@ top-left,
-- @(0.5,0.5)@ center, @(1,1)@ bottom-right.
data NodeOrigin = NodeOrigin
  { originX :: !Double
  , originY :: !Double
  } deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
defaultOrigin :: NodeOrigin
defaultOrigin = NodeOrigin 0 0
-----------------------------------------------------------------------------
-- | Framework independent node data structure; port of @NodeBase@.
-- @node@ is parameterized over the type of its user data.
data Node n = Node
  { nodeId             :: !NodeId
  , nodePosition       :: !XYPosition
  , nodeData           :: n
  , nodeType           :: !(Maybe MisoString)
  , nodeSourcePosition :: !(Maybe Position)
  , nodeTargetPosition :: !(Maybe Position)
  , nodeHidden         :: !Bool
  , nodeSelected       :: !Bool
  , nodeDragging       :: !Bool
  , nodeDraggable      :: !(Maybe Bool)
  , nodeSelectable     :: !(Maybe Bool)
  , nodeConnectable    :: !(Maybe Bool)
  , nodeDeletable      :: !(Maybe Bool)
  , nodeDragHandle     :: !(Maybe MisoString)
  , nodeWidth          :: !(Maybe Double)
  , nodeHeight         :: !(Maybe Double)
  , nodeInitialWidth   :: !(Maybe Double)
  , nodeInitialHeight  :: !(Maybe Double)
  , nodeParentId       :: !(Maybe NodeId)
  , nodeZIndex         :: !(Maybe Double)
  , nodeExtent         :: !(Maybe NodeExtent)
  , nodeExpandParent   :: !Bool
  , nodeAriaLabel      :: !(Maybe MisoString)
  , nodeOrigin         :: !(Maybe NodeOrigin)
  , nodeHandles        :: !(Maybe [NodeHandle])
  , nodeMeasured       :: !(Maybe Measured)
  , nodeResizing       :: !Bool
    -- ^ 'True' while the node is being resized via a resizer control
  } deriving (Show, Eq, Generic, Functor)
-----------------------------------------------------------------------------
-- | Node with default field values.
node :: NodeId -> XYPosition -> n -> Node n
node i p d = Node
  { nodeId = i
  , nodePosition = p
  , nodeData = d
  , nodeType = Nothing
  , nodeSourcePosition = Nothing
  , nodeTargetPosition = Nothing
  , nodeHidden = False
  , nodeSelected = False
  , nodeDragging = False
  , nodeDraggable = Nothing
  , nodeSelectable = Nothing
  , nodeConnectable = Nothing
  , nodeDeletable = Nothing
  , nodeDragHandle = Nothing
  , nodeWidth = Nothing
  , nodeHeight = Nothing
  , nodeInitialWidth = Nothing
  , nodeInitialHeight = Nothing
  , nodeParentId = Nothing
  , nodeZIndex = Nothing
  , nodeExtent = Nothing
  , nodeExpandParent = False
  , nodeAriaLabel = Nothing
  , nodeOrigin = Nothing
  , nodeHandles = Nothing
  , nodeMeasured = Nothing
  , nodeResizing = False
  }
-----------------------------------------------------------------------------
-- | Handles of a node grouped by type, as measured from the DOM.
data NodeHandleBounds = NodeHandleBounds
  { nhbSource :: !(Maybe [Handle])
  , nhbTarget :: !(Maybe [Handle])
  } deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
data NodeBounds = NodeBounds
  { nbX      :: !Double
  , nbY      :: !Double
  , nbWidth  :: !(Maybe Double)
  , nbHeight :: !(Maybe Double)
  } deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
-- | A user 'Node' enriched with runtime internals; port of
-- @InternalNodeBase@. The user node is kept whole in 'internalUserNode'
-- (TS keeps it at @internals.userNode@).
data InternalNode n = InternalNode
  { internalUser             :: !(Node n)
  , internalMeasured         :: !Measured
  , internalPositionAbsolute :: !XYPosition
  , internalZ                :: !Double
  , internalRootParentIndex  :: !(Maybe Int)
  , internalHandleBounds     :: !(Maybe NodeHandleBounds)
  , internalBounds           :: !(Maybe NodeBounds)
  } deriving (Show, Eq, Generic, Functor)
-----------------------------------------------------------------------------
-- | TS: @node.internals.userNode@.
internalUserNode :: InternalNode n -> Node n
internalUserNode = internalUser
-----------------------------------------------------------------------------
-- | Slice of a node captured when a drag starts; port of @NodeDragItem@.
data NodeDragItem = NodeDragItem
  { dragItemId               :: !NodeId
  , dragItemPosition         :: !XYPosition
  , dragItemDistance         :: !XYPosition
  , dragItemMeasured         :: !Dimensions
  , dragItemPositionAbsolute :: !XYPosition
  , dragItemExtent           :: !(Maybe NodeExtent)
  , dragItemParentId         :: !(Maybe NodeId)
  , dragItemOrigin           :: !(Maybe NodeOrigin)
  , dragItemExpandParent     :: !Bool
  , dragItemDragging         :: !Bool
  } deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
type NodeLookup n = Map NodeId (InternalNode n)
-----------------------------------------------------------------------------
type ParentLookup n = Map NodeId (Map NodeId (InternalNode n))
-----------------------------------------------------------------------------
-- | Request to re-measure a node; port of @InternalNodeUpdate@ (the DOM
-- element itself stays on the JS side of the bridge).
data InternalNodeUpdate = InternalNodeUpdate
  { internalUpdateId    :: !NodeId
  , internalUpdateForce :: !Bool
  } deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
type EdgeId = MisoString
-----------------------------------------------------------------------------
-- | Framework independent edge data structure; port of @EdgeBase@,
-- parameterized over the edge's user data.
data Edge e = Edge
  { edgeId               :: !EdgeId
  , edgeSource           :: !NodeId
  , edgeTarget           :: !NodeId
  , edgeType             :: !(Maybe MisoString)
  , edgeSourceHandle     :: !(Maybe MisoString)
  , edgeTargetHandle     :: !(Maybe MisoString)
  , edgeAnimated         :: !Bool
  , edgeHidden           :: !Bool
  , edgeDeletable        :: !(Maybe Bool)
  , edgeSelectable       :: !(Maybe Bool)
  , edgeData             :: !(Maybe e)
  , edgeSelected         :: !Bool
  , edgeMarkerStart      :: !(Maybe EdgeMarkerType)
  , edgeMarkerEnd        :: !(Maybe EdgeMarkerType)
  , edgeZIndex           :: !(Maybe Double)
  , edgeAriaLabel        :: !(Maybe MisoString)
  , edgeInteractionWidth :: !(Maybe Double)
  } deriving (Show, Eq, Generic, Functor)
-----------------------------------------------------------------------------
-- | Edge with default field values.
edge :: EdgeId -> NodeId -> NodeId -> Edge e
edge i s t = Edge
  { edgeId = i
  , edgeSource = s
  , edgeTarget = t
  , edgeType = Nothing
  , edgeSourceHandle = Nothing
  , edgeTargetHandle = Nothing
  , edgeAnimated = False
  , edgeHidden = False
  , edgeDeletable = Nothing
  , edgeSelectable = Nothing
  , edgeData = Nothing
  , edgeSelected = False
  , edgeMarkerStart = Nothing
  , edgeMarkerEnd = Nothing
  , edgeZIndex = Nothing
  , edgeAriaLabel = Nothing
  , edgeInteractionWidth = Nothing
  }
-----------------------------------------------------------------------------
-- | Style of the connection line drawn while connecting.
data ConnectionLineType
  = ConnectionLineBezier       -- ^ TS @\'default\'@
  | ConnectionLineStraight
  | ConnectionLineStep
  | ConnectionLineSmoothStep
  | ConnectionLineSimpleBezier
  deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
data MarkerType
  = MarkerArrow
  | MarkerArrowClosed
  deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
markerTypeToText :: MarkerType -> MisoString
markerTypeToText = \case
  MarkerArrow       -> "arrow"
  MarkerArrowClosed -> "arrowclosed"
-----------------------------------------------------------------------------
-- | Marker configuration on either end of an edge.
data EdgeMarker = EdgeMarker
  { markerType        :: !MarkerType
  , markerColor       :: !(Maybe MisoString)
  , markerWidth       :: !(Maybe Double)
  , markerHeight      :: !(Maybe Double)
  , markerUnits       :: !(Maybe MisoString)
  , markerOrient      :: !(Maybe MisoString)
  , markerStrokeWidth :: !(Maybe Double)
  } deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
edgeMarker :: MarkerType -> EdgeMarker
edgeMarker t = EdgeMarker t Nothing Nothing Nothing Nothing Nothing Nothing
-----------------------------------------------------------------------------
-- | TS: @string | EdgeMarker@ — either a reference to an existing SVG
-- marker id or an inline marker description.
data EdgeMarkerType
  = MarkerRef !MisoString
  | Marker !EdgeMarker
  deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
-- | An 'EdgeMarker' with the id it renders under.
data MarkerProps = MarkerProps
  { markerPropsId     :: !MisoString
  , markerPropsMarker :: !EdgeMarker
  } deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
-- | Resolved start\/end coordinates and positions of an edge.
data EdgePosition = EdgePosition
  { epSourceX        :: !Double
  , epSourceY        :: !Double
  , epTargetX        :: !Double
  , epTargetY        :: !Double
  , epSourcePosition :: !Position
  , epTargetPosition :: !Position
  } deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
type EdgeLookup e = Map EdgeId (Edge e)
-----------------------------------------------------------------------------
-- | Lookup of connections keyed by @nodeId@, @nodeId-type@ and
-- @nodeId-type-handleId@; values keyed by a per-connection key.
type ConnectionLookup = Map MisoString (Map MisoString HandleConnection)
-----------------------------------------------------------------------------
newtype BezierPathOptions = BezierPathOptions
  { bezierCurvature :: Double
  } deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
defaultBezierPathOptions :: BezierPathOptions
defaultBezierPathOptions = BezierPathOptions 0.25
-----------------------------------------------------------------------------
data SmoothStepPathOptions = SmoothStepPathOptions
  { smoothStepOffset       :: !Double
  , smoothStepBorderRadius :: !Double
  , smoothStepStepPosition :: !Double
  , smoothStepCenterX      :: !(Maybe Double)
  , smoothStepCenterY      :: !(Maybe Double)
  } deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
defaultSmoothStepPathOptions :: SmoothStepPathOptions
defaultSmoothStepPathOptions = SmoothStepPathOptions
  { smoothStepOffset = 20
  , smoothStepBorderRadius = 5
  , smoothStepStepPosition = 0.5
  , smoothStepCenterX = Nothing
  , smoothStepCenterY = Nothing
  }
-----------------------------------------------------------------------------
newtype StepPathOptions = StepPathOptions
  { stepOffset :: Double
  } deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
-- | A padding value with an optional unit (TS: @number | \'10px\' | \'10%\'@).
data PaddingWithUnit
  = PaddingRatio !Double     -- ^ bare number, fraction of the viewport
  | PaddingPx !Double        -- ^ @\"…px\"@
  | PaddingPercent !Double   -- ^ @\"…%\"@
  deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
-- | Padding around bounds when fitting the viewport.
data Padding
  = PaddingUniform !PaddingWithUnit
  | PaddingSides
    { paddingTop    :: !(Maybe PaddingWithUnit)
    , paddingRight  :: !(Maybe PaddingWithUnit)
    , paddingBottom :: !(Maybe PaddingWithUnit)
    , paddingLeft   :: !(Maybe PaddingWithUnit)
    , paddingX      :: !(Maybe PaddingWithUnit)
    , paddingY      :: !(Maybe PaddingWithUnit)
    }
  deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
defaultFitViewPadding :: Padding
defaultFitViewPadding = PaddingUniform (PaddingRatio 0.1)
-----------------------------------------------------------------------------
data Interpolation
  = InterpolateSmooth
  | InterpolateLinear
  deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
data FitViewOptions = FitViewOptions
  { fitViewPadding            :: !Padding
  , fitViewIncludeHiddenNodes :: !Bool
  , fitViewMinZoom            :: !(Maybe Double)
  , fitViewMaxZoom            :: !(Maybe Double)
  , fitViewDuration           :: !(Maybe Double)
  , fitViewInterpolate        :: !Interpolation
  , fitViewNodes              :: !(Maybe [NodeId])
  } deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
defaultFitViewOptions :: FitViewOptions
defaultFitViewOptions = FitViewOptions
  { fitViewPadding = defaultFitViewPadding
  , fitViewIncludeHiddenNodes = False
  , fitViewMinZoom = Nothing
  , fitViewMaxZoom = Nothing
  , fitViewDuration = Nothing
  , fitViewInterpolate = InterpolateSmooth
  , fitViewNodes = Nothing
  }
-----------------------------------------------------------------------------
data ViewportHelperOptions = ViewportHelperOptions
  { vhoDuration    :: !(Maybe Double)
  , vhoInterpolate :: !Interpolation
  } deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
defaultViewportHelperOptions :: ViewportHelperOptions
defaultViewportHelperOptions = ViewportHelperOptions Nothing InterpolateSmooth
-----------------------------------------------------------------------------
data SetCenterOptions = SetCenterOptions
  { setCenterZoom    :: !(Maybe Double)
  , setCenterOptions :: !ViewportHelperOptions
  } deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
defaultSetCenterOptions :: SetCenterOptions
defaultSetCenterOptions = SetCenterOptions Nothing defaultViewportHelperOptions
-----------------------------------------------------------------------------
data FitBoundsOptions = FitBoundsOptions
  { fitBoundsPadding :: !Double
  , fitBoundsOptions :: !ViewportHelperOptions
  } deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
-- | Port of @NodeDimensionChange.setAttributes@
-- (TS: @boolean | \'width\' | \'height\'@).
data SetAttributes
  = SetAttributesNone     -- ^ @false@ — only update @measured@
  | SetAttributesBoth     -- ^ @true@ — also set @width@ and @height@
  | SetAttributesWidth
  | SetAttributesHeight
  deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
-- | Ways a node can change; port of @NodeChange@ in @types\/changes.ts@.
-- Apply with 'Miso.Flow.Changes.applyNodeChanges'.
data NodeChange n
  = NodeDimensionChange NodeId (Maybe Dimensions) (Maybe Bool) SetAttributes
    -- ^ id, dimensions, resizing, setAttributes
  | NodePositionChange NodeId (Maybe XYPosition) (Maybe XYPosition) (Maybe Bool)
    -- ^ id, position, positionAbsolute, dragging
  | NodeSelectionChange NodeId Bool
    -- ^ id, selected
  | NodeRemoveChange NodeId
  | NodeAddChange (Node n) (Maybe Int)
    -- ^ item, index
  | NodeReplaceChange NodeId (Node n)
  deriving (Show, Eq, Generic, Functor)
-----------------------------------------------------------------------------
-- | Ways an edge can change; port of @EdgeChange@.
-- Apply with 'Miso.Flow.Changes.applyEdgeChanges'.
data EdgeChange e
  = EdgeSelectionChange EdgeId Bool
  | EdgeRemoveChange EdgeId
  | EdgeAddChange (Edge e) (Maybe Int)
  | EdgeReplaceChange EdgeId (Edge e)
  deriving (Show, Eq, Generic, Functor)
-----------------------------------------------------------------------------
data ControlLinePosition
  = ControlLineTop
  | ControlLineBottom
  | ControlLineLeft
  | ControlLineRight
  deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
data ControlPosition
  = ControlLine !ControlLinePosition
  | ControlTopLeft
  | ControlTopRight
  | ControlBottomLeft
  | ControlBottomRight
  deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
controlPositionToText :: ControlPosition -> MisoString
controlPositionToText = \case
  ControlLine ControlLineTop    -> "top"
  ControlLine ControlLineBottom -> "bottom"
  ControlLine ControlLineLeft   -> "left"
  ControlLine ControlLineRight  -> "right"
  ControlTopLeft     -> "top-left"
  ControlTopRight    -> "top-right"
  ControlBottomLeft  -> "bottom-left"
  ControlBottomRight -> "bottom-right"
-----------------------------------------------------------------------------
data ResizeControlVariant
  = ResizeControlLine
  | ResizeControlHandle
  deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
data ResizeControlDirection
  = ResizeHorizontal
  | ResizeVertical
  deriving (Show, Eq, Ord, Generic)
-----------------------------------------------------------------------------
data ResizeParams = ResizeParams
  { resizeX      :: !Double
  , resizeY      :: !Double
  , resizeWidth  :: !Double
  , resizeHeight :: !Double
  } deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
data ResizeParamsWithDirection = ResizeParamsWithDirection
  { resizeParams     :: !ResizeParams
  , resizeDirectionX :: !Double
  , resizeDirectionY :: !Double
  } deriving (Show, Eq, Generic)
-----------------------------------------------------------------------------
xyResizerHandlePositions :: [ControlPosition]
xyResizerHandlePositions =
  [ ControlTopLeft, ControlTopRight, ControlBottomLeft, ControlBottomRight ]
-----------------------------------------------------------------------------
xyResizerLinePositions :: [ControlLinePosition]
xyResizerLinePositions =
  [ ControlLineTop, ControlLineRight, ControlLineBottom, ControlLineLeft ]
-----------------------------------------------------------------------------
-- | Error codes as used by @errorMessages@ (e.g. @\"006\"@).
type ErrorCode = MisoString
-----------------------------------------------------------------------------
-- | Error callback: code and rendered message.
type OnError = ErrorCode -> MisoString -> IO ()
-----------------------------------------------------------------------------
-- JSON instances matching the canonical xyflow wire shapes; these are
-- what crosses the bridge between the miso runtime and @\@xyflow\/system@.
-----------------------------------------------------------------------------
instance ToJSON XYPosition where
  toJSON (XYPosition x y) = object [ "x" .= x, "y" .= y ]
-----------------------------------------------------------------------------
instance FromJSON XYPosition where
  parseJSON = withObject "XYPosition" $ \o ->
    XYPosition <$> o .: "x" <*> o .: "y"
-----------------------------------------------------------------------------
instance ToJSON Dimensions where
  toJSON (Dimensions w h) = object [ "width" .= w, "height" .= h ]
-----------------------------------------------------------------------------
instance FromJSON Dimensions where
  parseJSON = withObject "Dimensions" $ \o ->
    Dimensions <$> o .: "width" <*> o .: "height"
-----------------------------------------------------------------------------
instance ToJSON Rect where
  toJSON (Rect x y w h) =
    object [ "x" .= x, "y" .= y, "width" .= w, "height" .= h ]
-----------------------------------------------------------------------------
instance FromJSON Rect where
  parseJSON = withObject "Rect" $ \o ->
    Rect <$> o .: "x" <*> o .: "y" <*> o .: "width" <*> o .: "height"
-----------------------------------------------------------------------------
instance ToJSON Viewport where
  toJSON (Viewport x y zoom) =
    object [ "x" .= x, "y" .= y, "zoom" .= zoom ]
-----------------------------------------------------------------------------
instance FromJSON Viewport where
  parseJSON = withObject "Viewport" $ \o ->
    Viewport <$> o .: "x" <*> o .: "y" <*> (o .: "zoom" <|> o .: "k")
-----------------------------------------------------------------------------
instance ToJSON Position where
  toJSON = toJSON . positionToText
-----------------------------------------------------------------------------
instance FromJSON Position where
  parseJSON = withText "Position" $ \t ->
    maybe (fail "invalid position") pure (positionFromText t)
-----------------------------------------------------------------------------
instance ToJSON HandleType where
  toJSON = toJSON . handleTypeToText
-----------------------------------------------------------------------------
instance FromJSON HandleType where
  parseJSON = withText "HandleType" $ \t ->
    maybe (fail "invalid handle type") pure (handleTypeFromText t)
-----------------------------------------------------------------------------
instance ToJSON CoordinateExtent where
  toJSON (CoordinateExtent x1 y1 x2 y2) =
    toJSON [ [ x1, y1 ], [ x2, y2 ] ]
-----------------------------------------------------------------------------
instance FromJSON CoordinateExtent where
  parseJSON v = do
    pairs <- parseJSON v
    case pairs of
      [ [ x1, y1 ], [ x2, y2 ] ] -> pure (CoordinateExtent x1 y1 x2 y2)
      _ -> fail "invalid coordinate extent"
-----------------------------------------------------------------------------
instance ToJSON NodeExtent where
  toJSON ExtentParent = toJSON ("parent" :: MisoString)
  toJSON (ExtentCoordinates ce) = toJSON ce
-----------------------------------------------------------------------------
instance FromJSON NodeExtent where
  parseJSON v =
    (withText "NodeExtent" $ \t ->
       if t == "parent" then pure ExtentParent else fail "invalid extent") v
    <|> (ExtentCoordinates <$> parseJSON v)
-----------------------------------------------------------------------------
instance ToJSON NodeOrigin where
  toJSON (NodeOrigin x y) = toJSON [ x, y ]
-----------------------------------------------------------------------------
instance FromJSON NodeOrigin where
  parseJSON v = do
    xs <- parseJSON v
    case xs of
      [ x, y ] -> pure (NodeOrigin x y)
      _ -> fail "invalid node origin"
-----------------------------------------------------------------------------
instance ToJSON SnapGrid where
  toJSON (SnapGrid x y) = toJSON [ x, y ]
-----------------------------------------------------------------------------
instance FromJSON SnapGrid where
  parseJSON v = do
    xs <- parseJSON v
    case xs of
      [ x, y ] -> pure (SnapGrid x y)
      _ -> fail "invalid snap grid"
-----------------------------------------------------------------------------
instance ToJSON Handle where
  toJSON h = object
    [ "id" .= hId h
    , "nodeId" .= hNodeId h
    , "x" .= hX h
    , "y" .= hY h
    , "position" .= hPosition h
    , "type" .= hType h
    , "width" .= hWidth h
    , "height" .= hHeight h
    ]
-----------------------------------------------------------------------------
instance FromJSON Handle where
  parseJSON = withObject "Handle" $ \o ->
    Handle
      <$> o .:? "id"
      <*> o .: "nodeId"
      <*> o .: "x"
      <*> o .: "y"
      <*> o .: "position"
      <*> o .: "type"
      <*> o .: "width"
      <*> o .: "height"
-----------------------------------------------------------------------------
instance ToJSON NodeHandle where
  toJSON h = object
    [ "id" .= nhId h
    , "x" .= nhX h
    , "y" .= nhY h
    , "position" .= nhPosition h
    , "type" .= nhType h
    , "width" .= nhWidth h
    , "height" .= nhHeight h
    ]
-----------------------------------------------------------------------------
instance FromJSON NodeHandle where
  parseJSON = withObject "NodeHandle" $ \o ->
    NodeHandle
      <$> o .:? "id"
      <*> o .: "x"
      <*> o .: "y"
      <*> o .: "position"
      <*> o .: "type"
      <*> o .:? "width"
      <*> o .:? "height"
-----------------------------------------------------------------------------
instance ToJSON NodeHandleBounds where
  toJSON (NodeHandleBounds src tgt) =
    object [ "source" .= src, "target" .= tgt ]
-----------------------------------------------------------------------------
instance FromJSON NodeHandleBounds where
  parseJSON = withObject "NodeHandleBounds" $ \o ->
    NodeHandleBounds <$> o .:? "source" <*> o .:? "target"
-----------------------------------------------------------------------------
instance ToJSON Measured where
  toJSON (Measured w h) = object [ "width" .= w, "height" .= h ]
-----------------------------------------------------------------------------
instance FromJSON Measured where
  parseJSON = withObject "Measured" $ \o ->
    Measured <$> o .:? "width" <*> o .:? "height"
-----------------------------------------------------------------------------
instance ToJSON Connection where
  toJSON c = object
    [ "source" .= connectionSource c
    , "target" .= connectionTarget c
    , "sourceHandle" .= connectionSourceHandle c
    , "targetHandle" .= connectionTargetHandle c
    ]
-----------------------------------------------------------------------------
instance FromJSON Connection where
  parseJSON = withObject "Connection" $ \o ->
    Connection
      <$> o .: "source"
      <*> o .: "target"
      <*> o .:? "sourceHandle"
      <*> o .:? "targetHandle"
-----------------------------------------------------------------------------
instance ToJSON ConnectionMode where
  toJSON ConnectionModeStrict = toJSON ("strict" :: MisoString)
  toJSON ConnectionModeLoose = toJSON ("loose" :: MisoString)
-----------------------------------------------------------------------------
instance ToJSON PanOnDrag where
  toJSON (PanOnDrag b) = toJSON b
  toJSON (PanOnDragButtons bs) = toJSON bs
-----------------------------------------------------------------------------
instance ToJSON PanOnScrollMode where
  toJSON PanOnScrollFree = toJSON ("free" :: MisoString)
  toJSON PanOnScrollVertical = toJSON ("vertical" :: MisoString)
  toJSON PanOnScrollHorizontal = toJSON ("horizontal" :: MisoString)
-----------------------------------------------------------------------------
instance ToJSON ZIndexMode where
  toJSON ZIndexAuto = toJSON ("auto" :: MisoString)
  toJSON ZIndexBasic = toJSON ("basic" :: MisoString)
  toJSON ZIndexManual = toJSON ("manual" :: MisoString)
-----------------------------------------------------------------------------
instance ToJSON Interpolation where
  toJSON InterpolateSmooth = toJSON ("smooth" :: MisoString)
  toJSON InterpolateLinear = toJSON ("linear" :: MisoString)
-----------------------------------------------------------------------------
instance ToJSON ControlPosition where
  toJSON = toJSON . controlPositionToText
-----------------------------------------------------------------------------
instance ToJSON ResizeControlDirection where
  toJSON ResizeHorizontal = toJSON ("horizontal" :: MisoString)
  toJSON ResizeVertical = toJSON ("vertical" :: MisoString)
-----------------------------------------------------------------------------
instance ToJSON OnConnectStartParams where
  toJSON p = object
    [ "nodeId" .= onConnectStartNodeId p
    , "handleId" .= onConnectStartHandleId p
    , "handleType" .= onConnectStartHandleType p
    ]
-----------------------------------------------------------------------------
instance FromJSON OnConnectStartParams where
  parseJSON = withObject "OnConnectStartParams" $ \o ->
    OnConnectStartParams
      <$> o .:? "nodeId"
      <*> o .:? "handleId"
      <*> o .:? "handleType"
-----------------------------------------------------------------------------
-- | An optional pair: absent fields are omitted rather than sent as
-- @null@ — xyflow distinguishes @undefined@ (defaulted) from @null@,
-- e.g. @getDragItems@ tests @typeof draggable === 'undefined'@.
(.=?) :: ToJSON v => MisoString -> Maybe v -> Maybe (MisoString, Value)
k .=? mv = (k .=) <$> mv
-----------------------------------------------------------------------------
-- | Serialize a 'Node' for the JavaScript store. The user @data@ payload
-- deliberately stays in Haskell; everything the gesture system needs
-- (geometry, flags, handles) crosses the bridge. Optional fields are
-- omitted when unset.
instance ToJSON (Node n) where
  toJSON n = object $
    [ "id" .= nodeId n
    , "position" .= nodePosition n
    , "hidden" .= nodeHidden n
    , "selected" .= nodeSelected n
    , "dragging" .= nodeDragging n
    , "expandParent" .= nodeExpandParent n
    ]
    <> catMaybes
    [ "sourcePosition" .=? nodeSourcePosition n
    , "targetPosition" .=? nodeTargetPosition n
    , "draggable" .=? nodeDraggable n
    , "selectable" .=? nodeSelectable n
    , "connectable" .=? nodeConnectable n
    , "deletable" .=? nodeDeletable n
    , "dragHandle" .=? nodeDragHandle n
    , "width" .=? nodeWidth n
    , "height" .=? nodeHeight n
    , "initialWidth" .=? nodeInitialWidth n
    , "initialHeight" .=? nodeInitialHeight n
    , "parentId" .=? nodeParentId n
    , "zIndex" .=? nodeZIndex n
    , "extent" .=? nodeExtent n
    , "origin" .=? nodeOrigin n
    , "handles" .=? nodeHandles n
    , "measured" .=? nodeMeasured n
    , "type" .=? nodeType n
    ]
-----------------------------------------------------------------------------
-- | Serialize an 'Edge' for the JavaScript store (again without the user
-- @data@ payload); optional fields are omitted when unset.
instance ToJSON (Edge e) where
  toJSON e = object $
    [ "id" .= edgeId e
    , "source" .= edgeSource e
    , "target" .= edgeTarget e
    , "selected" .= edgeSelected e
    , "hidden" .= edgeHidden e
    ]
    <> catMaybes
    [ "sourceHandle" .=? edgeSourceHandle e
    , "targetHandle" .=? edgeTargetHandle e
    , "deletable" .=? edgeDeletable e
    , "selectable" .=? edgeSelectable e
    , "zIndex" .=? edgeZIndex e
    , "type" .=? edgeType e
    ]
-----------------------------------------------------------------------------
