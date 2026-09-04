/*
 * Golden fixture generator: runs the real @xyflow/system code (and the
 * framework packages' applyChanges / getSimpleBezierPath) over a grid of
 * inputs and dumps inputs + outputs to golden.json. The Haskell test
 * suite replays the inputs through the Miso.Flow ports and demands
 * identical results (paths byte-for-byte, numbers exactly).
 *
 * Regenerate with:  bun tests/golden/gen.ts
 */
import {
  getBezierPath,
  getBezierEdgeCenter,
  getSmoothStepPath,
  getStraightPath,
  getEdgeCenter,
  getViewportForBounds,
  getNodesBounds,
  pointToRendererPoint,
  rendererPointToPoint,
  snapPosition,
  clampPosition,
  getBoundsOfRects,
  getOverlappingArea,
  getMarkerId,
  createMarkerIds,
  addEdge,
  getEdgeId,
  getNodesInside,
  getConnectedEdges,
  getIncomers,
  getOutgoers,
  adoptUserNodes,
  infiniteExtent,
  type NodeBase,
  type EdgeBase,
  type InternalNodeBase,
  Position,
  MarkerType,
} from '../../xyflow/packages/system/src';
import { applyNodeChanges, applyEdgeChanges } from '../../xyflow/packages/react/src/utils/changes';

const positions = [Position.Left, Position.Top, Position.Right, Position.Bottom];

const coordSets = [
  { sx: 0, sy: 0, tx: 250, ty: 150 },
  { sx: 100.5, sy: -75.25, tx: -30.125, ty: 220.875 },
  { sx: 13.37, sy: 42.001, tx: 13.37, ty: 42.001 }, // degenerate: same point
];

/* ---------------- bezier ---------------- */
const bezier: unknown[] = [];
for (const sp of positions)
  for (const tp of positions)
    for (const c of coordSets)
      for (const curvature of [0.25, 0.5]) {
        const [path, labelX, labelY, offsetX, offsetY] = getBezierPath({
          sourceX: c.sx, sourceY: c.sy, sourcePosition: sp,
          targetX: c.tx, targetY: c.ty, targetPosition: tp,
          curvature,
        });
        bezier.push({ ...c, sp, tp, curvature, path, labelX, labelY, offsetX, offsetY });
      }

/* ---------------- smoothstep ---------------- */
const smoothstep: unknown[] = [];
for (const sp of positions)
  for (const tp of positions)
    for (const c of coordSets.slice(0, 2))
      for (const borderRadius of [0, 5, 12]) {
        const [path, labelX, labelY, offsetX, offsetY] = getSmoothStepPath({
          sourceX: c.sx, sourceY: c.sy, sourcePosition: sp,
          targetX: c.tx, targetY: c.ty, targetPosition: tp,
          borderRadius,
        });
        smoothstep.push({ ...c, sp, tp, borderRadius, offset: 20, stepPosition: 0.5, path, labelX, labelY, offsetX, offsetY });
      }
// offset / stepPosition / center variants
for (const variant of [
  { offset: 40, stepPosition: 0.5 },
  { offset: 20, stepPosition: 0.2 },
  { offset: 10, stepPosition: 0.8, centerX: 120 },
  { offset: 20, stepPosition: 0.5, centerY: -10 },
]) {
  const c = coordSets[1];
  const [path, labelX, labelY, offsetX, offsetY] = getSmoothStepPath({
    sourceX: c.sx, sourceY: c.sy, sourcePosition: Position.Right,
    targetX: c.tx, targetY: c.ty, targetPosition: Position.Left,
    borderRadius: 5, ...variant,
  });
  smoothstep.push({
    ...c, sp: Position.Right, tp: Position.Left, borderRadius: 5,
    offset: variant.offset, stepPosition: variant.stepPosition,
    centerX: variant.centerX, centerY: variant.centerY,
    path, labelX, labelY, offsetX, offsetY,
  });
}

/* ---------------- straight ---------------- */
const straight = coordSets.concat([{ sx: -5.5, sy: 3, tx: -5.5, ty: -800.001 }]).map((c) => {
  const [path, labelX, labelY, offsetX, offsetY] = getStraightPath({
    sourceX: c.sx, sourceY: c.sy, targetX: c.tx, targetY: c.ty,
  });
  return { ...c, path, labelX, labelY, offsetX, offsetY };
});

/* ---------------- simple bezier (framework packages) ---------------- */
function getControl(pos: Position, x1: number, y1: number, x2: number, y2: number): [number, number] {
  if (pos === Position.Left || pos === Position.Right) return [0.5 * (x1 + x2), y1];
  return [x1, 0.5 * (y1 + y2)];
}
const simplebezier: unknown[] = [];
for (const sp of positions)
  for (const tp of positions) {
    const c = coordSets[1];
    const [scx, scy] = getControl(sp, c.sx, c.sy, c.tx, c.ty);
    const [tcx, tcy] = getControl(tp, c.tx, c.ty, c.sx, c.sy);
    const [labelX, labelY, offsetX, offsetY] = getBezierEdgeCenter({
      sourceX: c.sx, sourceY: c.sy, targetX: c.tx, targetY: c.ty,
      sourceControlX: scx, sourceControlY: scy, targetControlX: tcx, targetControlY: tcy,
    });
    const path = `M${c.sx},${c.sy} C${scx},${scy} ${tcx},${tcy} ${c.tx},${c.ty}`;
    simplebezier.push({ ...c, sp, tp, path, labelX, labelY, offsetX, offsetY });
  }

/* ---------------- centers ---------------- */
const edgeCenter = coordSets.map((c) => {
  const [cx, cy, ox, oy] = getEdgeCenter({ sourceX: c.sx, sourceY: c.sy, targetX: c.tx, targetY: c.ty });
  return { ...c, cx, cy, ox, oy };
});

/* ---------------- geometry ---------------- */
const viewportForBounds = [
  { b: { x: 0, y: 0, width: 500, height: 300 }, w: 1000, h: 800, minZoom: 0.5, maxZoom: 2, padding: 0.1 },
  { b: { x: -200.5, y: 60, width: 1200, height: 90.75 }, w: 640, h: 480, minZoom: 0.25, maxZoom: 4, padding: 0.2 },
  { b: { x: 50, y: 50, width: 10, height: 10 }, w: 800, h: 600, minZoom: 0.5, maxZoom: 2, padding: 0 },
].map((c) => ({ ...c, out: getViewportForBounds(c.b, c.w, c.h, c.minZoom, c.maxZoom, c.padding) }));

const transforms: [number, number, number][] = [[0, 0, 1], [120.5, -40, 1.62018], [-3, 7, 0.25]];
const points = [{ x: 0, y: 0 }, { x: 155.25, y: -20.5 }, { x: -1000, y: 42 }];
const pointToRenderer = transforms.flatMap((t) =>
  points.map((p) => ({ p, t, out: pointToRendererPoint(p, t) })));
const pointToRendererSnapped = transforms.map((t) => ({
  p: points[1], t, snapGrid: [15, 10] as [number, number],
  out: pointToRendererPoint(points[1], t, true, [15, 10]),
}));
const rendererToPoint = transforms.flatMap((t) =>
  points.map((p) => ({ p, t, out: rendererPointToPoint(p, t) })));

const snap = [
  { p: { x: 7, y: 22 }, g: [15, 15] },
  { p: { x: -7.5, y: 22.49 }, g: [15, 10] },
  { p: { x: 0.5, y: -0.5 }, g: [1, 1] },
].map((c) => ({ ...c, out: snapPosition(c.p, c.g as [number, number]) }));

const clamp = [
  { p: { x: -50, y: 900 }, extent: [[0, 0], [500, 500]], dim: { width: 100, height: 40 } },
  { p: { x: 250, y: 250 }, extent: [[0, 0], [500, 500]], dim: { width: 100, height: 40 } },
  { p: { x: 480, y: 480 }, extent: [[0, 0], [500, 500]], dim: { width: 100, height: 40 } },
].map((c) => ({ ...c, out: clampPosition(c.p, c.extent as never, c.dim) }));

const rects = [
  { x: 0, y: 0, width: 100, height: 100 },
  { x: 50, y: 50, width: 100, height: 100 },
  { x: 300, y: -20.5, width: 10.25, height: 4 },
];
const boundsOfRects = [
  { a: rects[0], b: rects[1], out: getBoundsOfRects(rects[0], rects[1]) },
  { a: rects[0], b: rects[2], out: getBoundsOfRects(rects[0], rects[2]) },
];
const overlap = [
  { a: rects[0], b: rects[1], out: getOverlappingArea(rects[0], rects[1]) },
  { a: rects[0], b: rects[2], out: getOverlappingArea(rects[0], rects[2]) },
];

/* ---------------- markers ---------------- */
const markerEdges = [
  { id: 'e1', source: '1', target: '2', markerEnd: { type: MarkerType.ArrowClosed } },
  { id: 'e2', source: '2', target: '3', markerStart: { type: MarkerType.Arrow, color: '#f00', strokeWidth: 2 } },
  { id: 'e3', source: '3', target: '4', markerEnd: { type: MarkerType.ArrowClosed } }, // duplicate of e1's
  { id: 'e4', source: '4', target: '5', markerEnd: 'my-marker-ref' },
  { id: 'e5', source: '5', target: '6', markerEnd: { type: MarkerType.Arrow, width: 30, height: 20, markerUnits: 'userSpaceOnUse', orient: 'auto' } },
] as EdgeBase[];
const markers = {
  ids: createMarkerIds(markerEdges, { id: 'flow-a', defaultColor: '#888' }),
  single: [
    { m: { type: MarkerType.ArrowClosed }, flowId: 'x', out: getMarkerId({ type: MarkerType.ArrowClosed }, 'x') },
    { m: { type: MarkerType.Arrow, color: 'red', strokeWidth: 1.5 }, flowId: undefined, out: getMarkerId({ type: MarkerType.Arrow, color: 'red', strokeWidth: 1.5 }, undefined) },
  ],
};

/* ---------------- addEdge / getEdgeId ---------------- */
const baseEdges = [{ id: 'e1-2', source: '1', target: '2' }] as EdgeBase[];
const addEdgeCases = [
  { conn: { source: '2', target: '3', sourceHandle: null, targetHandle: null } },
  { conn: { source: '2', target: '3', sourceHandle: 'a', targetHandle: 'b' } },
  { conn: { source: '', target: '3', sourceHandle: null, targetHandle: null } }, // invalid
].map((c) => ({
  ...c,
  out: addEdge(c.conn, baseEdges).map((e) => e.id),
}));
const edgeIds = [
  { source: 'a', target: 'b', sourceHandle: null, targetHandle: null },
  { source: 'a', target: 'b', sourceHandle: 'h1', targetHandle: 'h2' },
].map((c) => ({ ...c, out: getEdgeId(c) }));

/* ---------------- graph over adopted nodes ---------------- */
const userNodes = [
  { id: '1', position: { x: 0, y: 0 }, data: {}, measured: { width: 100, height: 40 } },
  { id: '2', position: { x: 250, y: 150 }, data: {}, measured: { width: 120, height: 50 }, selected: true },
  { id: '3', position: { x: -80.5, y: 60 }, data: {}, measured: { width: 80, height: 30 }, zIndex: 7 },
  { id: 'p', position: { x: 400, y: 400 }, data: {}, measured: { width: 300, height: 200 } },
  { id: 'c1', position: { x: 20, y: 30 }, data: {}, parentId: 'p', measured: { width: 50, height: 20 } },
  { id: 'c2', position: { x: 10, y: 10 }, data: {}, parentId: 'p', origin: [0.5, 0.5], measured: { width: 40, height: 40 } },
] as NodeBase[];

const nodeLookup = new Map<string, InternalNodeBase>();
const parentLookup = new Map();
adoptUserNodes(userNodes, nodeLookup, parentLookup, {
  nodeOrigin: [0, 0],
  nodeExtent: infiniteExtent,
  elevateNodesOnSelect: true,
});
const adopt = [...nodeLookup.values()].map((n) => ({
  id: n.id,
  x: n.internals.positionAbsolute.x,
  y: n.internals.positionAbsolute.y,
  z: n.internals.z,
}));

const graphEdges = [
  { id: 'e1-2', source: '1', target: '2' },
  { id: 'e2-3', source: '2', target: '3' },
  { id: 'e1-3', source: '1', target: '3' },
] as EdgeBase[];
const graph = {
  nodesBounds: getNodesBounds(userNodes.slice(0, 3)),
  nodesInside: getNodesInside(nodeLookup, { x: -100, y: -10, width: 400, height: 260 }, [0, 0, 1], false).map((n) => n.id).sort(),
  nodesInsidePartial: getNodesInside(nodeLookup, { x: -100, y: -10, width: 400, height: 260 }, [0, 0, 1], true).map((n) => n.id).sort(),
  connected: getConnectedEdges([userNodes[1]], graphEdges).map((e) => e.id).sort(),
  incomers: getIncomers(userNodes[2], userNodes, graphEdges).map((n) => n.id).sort(),
  outgoers: getOutgoers(userNodes[0], userNodes, graphEdges).map((n) => n.id).sort(),
};

/* ---------------- applyChanges (framework packages) ---------------- */
const changeNodes = [
  { id: '1', position: { x: 0, y: 0 }, data: {} },
  { id: '2', position: { x: 100, y: 100 }, data: {}, selected: false },
  { id: '3', position: { x: 200, y: 200 }, data: {} },
];
const nodeChangeCases = [
  { changes: [{ id: '2', type: 'position', position: { x: 111, y: -5.5 }, dragging: true }] },
  { changes: [{ id: '1', type: 'dimensions', dimensions: { width: 90, height: 45 }, setAttributes: true, resizing: true }] },
  { changes: [{ id: '3', type: 'select', selected: true }, { id: '2', type: 'select', selected: true }] },
  { changes: [{ id: '2', type: 'remove' }] },
  { changes: [{ type: 'add', item: { id: '9', position: { x: 5, y: 5 }, data: {} }, index: 1 }] },
  { changes: [{ id: '1', type: 'replace', item: { id: '1', position: { x: -1, y: -2 }, data: {} } }] },
].map((c) => ({
  ...c,
  out: applyNodeChanges(c.changes as never, changeNodes as never).map((n: any) => ({
    id: n.id, x: n.position.x, y: n.position.y,
    selected: n.selected, width: n.width, height: n.height,
    measuredW: n.measured?.width, measuredH: n.measured?.height, resizing: n.resizing,
  })),
}));

const changeEdges = [
  { id: 'e1', source: '1', target: '2' },
  { id: 'e2', source: '2', target: '3', selected: true },
];
const edgeChangeCases = [
  { changes: [{ id: 'e1', type: 'select', selected: true }] },
  { changes: [{ id: 'e2', type: 'remove' }] },
  { changes: [{ type: 'add', item: { id: 'e9', source: '3', target: '1' } }] },
].map((c) => ({
  ...c,
  out: applyEdgeChanges(c.changes as never, changeEdges as never).map((e: any) => ({
    id: e.id, selected: e.selected,
  })),
}));

/* ---------------- write ---------------- */
const golden = {
  bezier, smoothstep, straight, simplebezier, edgeCenter,
  viewportForBounds, pointToRenderer, pointToRendererSnapped, rendererToPoint,
  snap, clamp, boundsOfRects, overlap,
  markers, addEdgeCases, edgeIds, adopt, graph,
  nodeChangeCases, edgeChangeCases,
};

await Bun.write(new URL('./golden.json', import.meta.url).pathname, JSON.stringify(golden, null, 1));
console.log('sections:', Object.entries(golden).map(([k, v]) => `${k}=${Array.isArray(v) ? v.length : 'obj'}`).join(' '));
