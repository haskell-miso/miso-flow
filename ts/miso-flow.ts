/*
 * miso-flow bridge: drives the imperative modules of @xyflow/system
 * (XYPanZoom, XYDrag, XYHandle, XYResizer, XYMinimap and the store
 * helpers) against a DOM subtree rendered by miso, and reports gesture
 * results back into the miso runtime through a set of callbacks.
 *
 * Everything crossing the Haskell <-> JS boundary is a JSON string.
 */
import {
  XYPanZoom,
  XYDrag,
  XYHandle,
  XYResizer,
  XYMinimap,
  type PanZoomInstance,
  type XYDragInstance,
  type XYResizerInstance,
  type XYMinimapInstance,
  adoptUserNodes,
  isInputDOMNode,
  updateConnectionLookup,
  updateNodeInternals,
  fitViewport,
  panBy as systemPanBy,
  infiniteExtent,
  initialConnection,
  ConnectionMode,
  PanOnScrollMode,
  type ConnectionState,
  type CoordinateExtent,
  type EdgeBase,
  type InternalNodeBase,
  type InternalNodeUpdate,
  type NodeBase,
  type NodeDragItem,
  type Transform,
  type Viewport,
} from '../xyflow/packages/system/src';

import * as system from '../xyflow/packages/system/src';

type Json = string;

export type MisoFlowCallbacks = {
  onViewport?: (json: Json) => void;
  onViewportStart?: (json: Json) => void;
  onViewportEnd?: (json: Json) => void;
  onNodeChanges?: (json: Json) => void;
  onConnectionUpdate?: (json: Json) => void;
  onConnectStart?: (json: Json) => void;
  onConnect?: (json: Json) => void;
  onConnectEnd?: (json: Json) => void;
  onNodeMouseDown?: (json: Json) => void;
  onUnselectNodesAndEdges?: (json: Json) => void;
  onPaneClick?: (json: Json) => void;
  onNodeDrag?: (json: Json) => void;
  onNodeDragStart?: (json: Json) => void;
  onNodeDragStop?: (json: Json) => void;
  onResizeChanges?: (json: Json) => void;
  onDimensions?: (json: Json) => void;
  onSelectionRect?: (json: Json) => void;
  onSelectionEnd?: (json: Json) => void;
  onReconnect?: (json: Json) => void;
  onResizeStart?: (json: Json) => void;
  onResizeEnd?: (json: Json) => void;
  onMinimapClick?: (json: Json) => void;
  onDeleteKey?: (json: Json) => void;
  /* synchronous: receives a Connection as JSON, returns a boolean */
  isValidConnection?: (json: Json) => boolean;
  onError?: (code: string, message: string) => void;
  onMoveStart?: (json: Json) => void;
  onMoveEnd?: (json: Json) => void;
};

export type MisoFlowOptions = {
  flowId: string;
  lib: string;
  minZoom: number;
  maxZoom: number;
  translateExtent: CoordinateExtent | null;
  nodeExtent: CoordinateExtent | null;
  nodeOrigin: [number, number];
  defaultViewport: Viewport;
  snapToGrid: boolean;
  snapGrid: [number, number];
  elevateNodesOnSelect: boolean;
  zIndexMode: 'auto' | 'basic' | 'manual';
  nodesDraggable: boolean;
  autoPanOnNodeDrag: boolean;
  autoPanOnConnect: boolean;
  autoPanSpeed: number;
  nodeDragThreshold: number;
  nodeClickDistance: number;
  selectNodesOnDrag: boolean;
  connectionMode: 'strict' | 'loose';
  connectionRadius: number;
  connectionDragThreshold: number;
  panOnDrag: boolean | number[];
  panOnScroll: boolean;
  panOnScrollMode: 'free' | 'vertical' | 'horizontal';
  panOnScrollSpeed: number;
  zoomOnScroll: boolean;
  zoomOnPinch: boolean;
  zoomOnDoubleClick: boolean;
  preventScrolling: boolean;
  paneClickDistance: number;
};

const defaultOptions: MisoFlowOptions = {
  flowId: '1',
  lib: 'miso',
  minZoom: 0.5,
  maxZoom: 2,
  translateExtent: null,
  nodeExtent: null,
  nodeOrigin: [0, 0],
  defaultViewport: { x: 0, y: 0, zoom: 1 },
  snapToGrid: false,
  snapGrid: [15, 15],
  elevateNodesOnSelect: true,
  zIndexMode: 'basic',
  nodesDraggable: true,
  autoPanOnNodeDrag: true,
  autoPanOnConnect: true,
  autoPanSpeed: 15,
  nodeDragThreshold: 1,
  nodeClickDistance: 0,
  selectNodesOnDrag: true,
  connectionMode: 'strict',
  connectionRadius: 20,
  connectionDragThreshold: 1,
  panOnDrag: true,
  panOnScroll: false,
  panOnScrollMode: 'free',
  panOnScrollSpeed: 0.5,
  zoomOnScroll: true,
  zoomOnPinch: true,
  zoomOnDoubleClick: true,
  preventScrolling: true,
  paneClickDistance: 0,
};

function parseViewportOptions(json?: Json) {
  if (!json) return undefined;
  const o = JSON.parse(json);
  return {
    duration: o.duration ?? undefined,
    interpolate: o.interpolate ?? undefined,
  };
}

function serializeHandle(h: system.Handle | null) {
  if (!h) return null;
  return {
    id: h.id ?? null,
    nodeId: h.nodeId,
    x: h.x,
    y: h.y,
    position: h.position,
    type: h.type,
    width: h.width,
    height: h.height,
  };
}

function serializeConnectionState(c: ConnectionState<InternalNodeBase>) {
  if (!c.inProgress) {
    return { inProgress: false };
  }
  return {
    inProgress: true,
    isValid: c.isValid,
    from: c.from,
    fromHandle: serializeHandle(c.fromHandle),
    fromPosition: c.fromPosition,
    fromNode: c.fromNode?.id ?? null,
    to: c.to,
    toHandle: serializeHandle(c.toHandle),
    toPosition: c.toPosition,
    toNode: c.toNode?.id ?? null,
    pointer: c.pointer,
  };
}

export class MisoFlowStore {
  domNode: HTMLDivElement;
  paneNode: HTMLDivElement;
  viewportNode: HTMLDivElement;
  options: MisoFlowOptions;
  callbacks: MisoFlowCallbacks;

  nodes: NodeBase[] = [];
  edges: EdgeBase[] = [];
  nodeLookup: Map<string, InternalNodeBase> = new Map();
  parentLookup: Map<string, Map<string, InternalNodeBase>> = new Map();
  edgeLookup: Map<string, EdgeBase> = new Map();
  connectionLookup: Map<string, Map<string, system.HandleConnection>> = new Map();

  transform: Transform = [0, 0, 1];
  width = 0;
  height = 0;
  connection: ConnectionState<InternalNodeBase> = initialConnection;

  panZoom: PanZoomInstance;
  minimap: XYMinimapInstance | null = null;
  minimapViewScale = 1;

  private dragInstances: Map<string, XYDragInstance> = new Map();
  private resizerInstances: Map<string, XYResizerInstance> = new Map();
  private nodeElements: Map<string, HTMLDivElement> = new Map();
  private resizeObserver: ResizeObserver | null = null;
  private containerObserver: ResizeObserver | null = null;
  private pendingMeasure: Map<string, InternalNodeUpdate> = new Map();
  private measureScheduled = false;
  private destroyed = false;
  private validateConnection: ((connection: system.Connection) => boolean) | undefined;
  private selectionKeyPressed = false;
  private multiKeyPressed = false;
  private selectionStart: { x: number; y: number } | null = null;
  private suppressPaneClick = false;

  constructor(domNode: HTMLDivElement, optionsJson: Json, callbacks: MisoFlowCallbacks) {
    this.domNode = domNode;
    this.options = { ...defaultOptions, ...(JSON.parse(optionsJson || '{}')) };
    this.callbacks = callbacks || {};

    const pane = domNode.querySelector(`.${this.options.lib}-flow__pane`);
    const viewport = domNode.querySelector('.xyflow__viewport');
    if (!pane || !viewport) {
      throw new Error('miso-flow: pane or viewport element not found');
    }
    this.paneNode = pane as HTMLDivElement;
    this.viewportNode = viewport as HTMLDivElement;

    const bounds = domNode.getBoundingClientRect();
    this.width = bounds.width;
    this.height = bounds.height;
    queueMicrotask(() => {
      this.callbacks.onDimensions?.(JSON.stringify({ width: this.width, height: this.height }));
    });

    this.transform = [
      this.options.defaultViewport.x,
      this.options.defaultViewport.y,
      this.options.defaultViewport.zoom,
    ];

    this.panZoom = XYPanZoom({
      domNode: this.paneNode,
      minZoom: this.options.minZoom,
      maxZoom: this.options.maxZoom,
      translateExtent: this.options.translateExtent ?? infiniteExtent,
      viewport: this.options.defaultViewport,
      onPanZoom: (_event, vp) => {
        this.callbacks.onViewport?.(JSON.stringify(vp));
      },
      onPanZoomStart: (_event, vp) => {
        this.callbacks.onViewportStart?.(JSON.stringify(vp));
      },
      onPanZoomEnd: (_event, vp) => {
        this.callbacks.onViewportEnd?.(JSON.stringify(vp));
      },
      onDraggingChange: () => undefined,
    });

    this.updatePanZoom();
    this.applyViewportStyle(this.transform);

    // track pane size
    if (typeof ResizeObserver !== 'undefined') {
      this.containerObserver = new ResizeObserver(() => {
        const b = this.domNode.getBoundingClientRect();
        this.width = b.width;
        this.height = b.height;
        this.callbacks.onDimensions?.(JSON.stringify({ width: this.width, height: this.height }));
      });
      this.containerObserver.observe(this.domNode);

      this.resizeObserver = new ResizeObserver((entries) => {
        for (const entry of entries) {
          const el = entry.target as HTMLDivElement;
          const id = el.getAttribute('data-id');
          if (id) {
            this.pendingMeasure.set(id, { id, nodeElement: el, force: true });
          }
        }
        this.scheduleMeasure();
      });
    }

    this.validateConnection = this.callbacks.isValidConnection
      ? (connection) => !!this.callbacks.isValidConnection!(JSON.stringify(connection))
      : undefined;

    this.paneNode.addEventListener('click', this.handlePaneClick);
    this.paneNode.addEventListener('mousedown', this.handleSelectionStart);
    // undo scroll events on the wrapper: browsers scroll overflow-hidden
    // containers to keep focused elements in view, which would shift the
    // whole canvas (same defense as the framework packages)
    this.domNode.addEventListener('scroll', this.handleContainerScroll);
    window.addEventListener('keydown', this.handleKeyChange);
    window.addEventListener('keyup', this.handleKeyChange);
    window.addEventListener('blur', this.handleWindowBlur);
  }

  /* ------------------------------------------------------------------ */
  /* selection box / modifier keys                                       */

  private handleKeyChange = (event: KeyboardEvent) => {
    const selection = event.shiftKey;
    const multi = event.metaKey || event.ctrlKey;
    if (selection !== this.selectionKeyPressed || multi !== this.multiKeyPressed) {
      this.selectionKeyPressed = selection;
      this.multiKeyPressed = multi;
      // shift disables pan-on-drag so the selection box can claim the gesture
      this.updatePanZoom();
    }
    if (
      event.type === 'keydown' &&
      (event.key === 'Backspace' || event.key === 'Delete') &&
      !isInputDOMNode(event)
    ) {
      this.callbacks.onDeleteKey?.(JSON.stringify({}));
    }
  };

  private handleContainerScroll = () => {
    this.domNode.scrollTo({ top: 0, left: 0, behavior: 'instant' });
  };

  private handleWindowBlur = () => {
    this.selectionKeyPressed = false;
    this.multiKeyPressed = false;
    this.updatePanZoom();
  };

  private handleSelectionStart = (event: MouseEvent) => {
    if (!this.selectionKeyPressed || event.button !== 0 || event.target !== this.paneNode) {
      return;
    }
    const bounds = this.domNode.getBoundingClientRect();
    this.selectionStart = { x: event.clientX - bounds.left, y: event.clientY - bounds.top };
    const onMove = (moveEvent: MouseEvent) => {
      if (!this.selectionStart) return;
      this.callbacks.onSelectionRect?.(JSON.stringify(this.selectionRect(moveEvent)));
    };
    const onUp = (upEvent: MouseEvent) => {
      window.removeEventListener('mousemove', onMove);
      window.removeEventListener('mouseup', onUp);
      if (this.selectionStart) {
        this.callbacks.onSelectionEnd?.(JSON.stringify(this.selectionRect(upEvent)));
        this.selectionStart = null;
        // the click belonging to this mouseup (if any) fires synchronously
        // after it; don't let the flag outlive the gesture and eat a later
        // legitimate pane click
        this.suppressPaneClick = true;
        setTimeout(() => {
          this.suppressPaneClick = false;
        }, 0);
        this.updatePanZoom();
      }
    };
    window.addEventListener('mousemove', onMove);
    window.addEventListener('mouseup', onUp);
    event.preventDefault();
  };

  private selectionRect(event: MouseEvent) {
    const bounds = this.domNode.getBoundingClientRect();
    const x = event.clientX - bounds.left;
    const y = event.clientY - bounds.top;
    const start = this.selectionStart!;
    return {
      x: Math.min(start.x, x),
      y: Math.min(start.y, y),
      width: Math.abs(x - start.x),
      height: Math.abs(y - start.y),
    };
  }

  /* ------------------------------------------------------------------ */
  /* viewport                                                            */

  private applyViewportStyle(transform: Transform) {
    this.transform = [transform[0], transform[1], transform[2]];
  }

  private updatePanZoom() {
    const o = this.options;
    this.panZoom.update({
      noWheelClassName: 'nowheel',
      noPanClassName: 'nopan',
      preventScrolling: o.preventScrolling,
      panOnScroll: o.panOnScroll,
      panOnDrag: o.panOnDrag,
      panOnScrollMode: o.panOnScrollMode as PanOnScrollMode,
      panOnScrollSpeed: o.panOnScrollSpeed,
      userSelectionActive: this.selectionKeyPressed || this.selectionStart !== null,
      zoomOnPinch: o.zoomOnPinch,
      zoomOnScroll: o.zoomOnScroll,
      zoomOnDoubleClick: o.zoomOnDoubleClick,
      zoomActivationKeyPressed: false,
      lib: o.lib,
      onTransformChange: (transform) => {
        this.applyViewportStyle(transform);
      },
      connectionInProgress: this.connection.inProgress,
      paneClickDistance: o.paneClickDistance,
    });
  }

  updateOptions(optionsJson: Json) {
    const next = JSON.parse(optionsJson || '{}');
    this.options = { ...this.options, ...next };
    if (next.minZoom !== undefined || next.maxZoom !== undefined) {
      this.panZoom.setScaleExtent([this.options.minZoom, this.options.maxZoom]);
    }
    if (next.translateExtent !== undefined) {
      this.panZoom.setTranslateExtent(this.options.translateExtent ?? infiniteExtent);
    }
    this.updatePanZoom();
  }

  getViewport(): Json {
    return JSON.stringify(this.panZoom.getViewport());
  }

  setViewport(viewportJson: Json, optionsJson?: Json) {
    const vp = JSON.parse(viewportJson);
    this.panZoom.setViewport(vp, parseViewportOptions(optionsJson));
  }

  syncViewport(viewportJson: Json) {
    this.panZoom.syncViewport(JSON.parse(viewportJson));
  }

  zoomIn(optionsJson?: Json) {
    this.panZoom.scaleBy(1.2, parseViewportOptions(optionsJson));
  }

  zoomOut(optionsJson?: Json) {
    this.panZoom.scaleBy(1 / 1.2, parseViewportOptions(optionsJson));
  }

  zoomTo(zoom: number, optionsJson?: Json) {
    this.panZoom.scaleTo(zoom, parseViewportOptions(optionsJson));
  }

  scaleBy(factor: number, optionsJson?: Json) {
    this.panZoom.scaleBy(factor, parseViewportOptions(optionsJson));
  }

  setCenter(x: number, y: number, optionsJson?: Json) {
    const o = JSON.parse(optionsJson || '{}');
    const nextZoom = typeof o.zoom === 'number' ? o.zoom : this.options.maxZoom;
    this.panZoom.setViewport(
      {
        x: this.width / 2 - x * nextZoom,
        y: this.height / 2 - y * nextZoom,
        zoom: nextZoom,
      },
      { duration: o.duration ?? undefined, interpolate: o.interpolate ?? undefined }
    );
  }

  fitBounds(boundsJson: Json, optionsJson?: Json) {
    const bounds = JSON.parse(boundsJson);
    const o = JSON.parse(optionsJson || '{}');
    const viewport = system.getViewportForBounds(
      bounds,
      this.width,
      this.height,
      this.options.minZoom,
      this.options.maxZoom,
      o.padding ?? 0.1
    );
    this.panZoom.setViewport(viewport, {
      duration: o.duration ?? undefined,
      interpolate: o.interpolate ?? undefined,
    });
  }

  fitView(optionsJson?: Json) {
    const o = JSON.parse(optionsJson || '{}');
    // fitViewport filters the lookup through getFitViewNodes itself
    fitViewport(
      {
        nodes: this.nodeLookup,
        width: this.width,
        height: this.height,
        panZoom: this.panZoom,
        minZoom: this.options.minZoom,
        maxZoom: this.options.maxZoom,
      },
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      o as any
    );
  }

  panBy(deltaJson: Json): void {
    const delta = JSON.parse(deltaJson);
    this.panByInternal(delta);
  }

  private panByInternal = (delta: system.XYPosition): Promise<boolean> => {
    return systemPanBy({
      delta,
      panZoom: this.panZoom,
      transform: this.transform,
      translateExtent: this.options.translateExtent ?? infiniteExtent,
      width: this.width,
      height: this.height,
    });
  };

  /* ------------------------------------------------------------------ */
  /* nodes / edges                                                       */

  setNodes(nodesJson: Json) {
    this.nodes = JSON.parse(nodesJson);
    adoptUserNodes(this.nodes, this.nodeLookup, this.parentLookup, {
      nodeOrigin: this.options.nodeOrigin,
      nodeExtent: this.options.nodeExtent ?? infiniteExtent,
      elevateNodesOnSelect: this.options.elevateNodesOnSelect,
      zIndexMode: this.options.zIndexMode,
      checkEquality: false,
    });
  }

  setEdges(edgesJson: Json) {
    this.edges = JSON.parse(edgesJson);
    updateConnectionLookup(this.connectionLookup, this.edgeLookup, this.edges);
  }

  /* ------------------------------------------------------------------ */
  /* measuring                                                           */

  observeNode(el: HTMLDivElement) {
    const id = el.getAttribute('data-id');
    if (!id) return;
    this.nodeElements.set(id, el);
    this.pendingMeasure.set(id, { id, nodeElement: el, force: true });
    this.resizeObserver?.observe(el);
    this.scheduleMeasure();
  }

  unobserveNode(el: HTMLDivElement) {
    const id = el.getAttribute('data-id');
    this.resizeObserver?.unobserve(el);
    if (id) {
      this.nodeElements.delete(id);
      this.pendingMeasure.delete(id);
    }
  }

  requestNodeMeasure(id: string) {
    const el = this.nodeElements.get(id);
    if (el) {
      this.pendingMeasure.set(id, { id, nodeElement: el, force: true });
      this.scheduleMeasure();
    }
  }

  private scheduleMeasure() {
    if (this.measureScheduled || this.destroyed) return;
    this.measureScheduled = true;
    requestAnimationFrame(() => {
      this.measureScheduled = false;
      this.measureNodes();
    });
  }

  private measureNodes() {
    if (this.pendingMeasure.size === 0) return;
    const updates = this.pendingMeasure;
    this.pendingMeasure = new Map();
    const { changes, updatedInternals } = updateNodeInternals(
      updates,
      this.nodeLookup,
      this.parentLookup,
      this.domNode,
      this.options.nodeOrigin,
      this.options.nodeExtent ?? infiniteExtent,
      this.options.zIndexMode
    );
    if (!updatedInternals || changes.length === 0) return;

    const wire = changes.map((change) => {
      if (change.type === 'dimensions') {
        const internal = this.nodeLookup.get(change.id);
        return {
          ...change,
          handleBounds: internal?.internals.handleBounds ?? null,
          positionAbsolute: internal?.internals.positionAbsolute ?? null,
        };
      }
      return change;
    });
    this.callbacks.onNodeChanges?.(JSON.stringify(wire));
  }

  /* ------------------------------------------------------------------ */
  /* dragging                                                            */

  attachNodeDrag(el: HTMLDivElement, nodeId: string) {
    let instance = this.dragInstances.get(nodeId);
    if (!instance) {
      instance = XYDrag({
        getStoreItems: () => this.dragStoreItems(),
        onNodeMouseDown: (id: string) => {
          // XYDrag builds its drag items from the lookup synchronously
          // right after this callback, before the Haskell round trip has
          // applied the selection change — mirror it here so a stale
          // selection can't drag other nodes along. Mousedown on an
          // already-selected node keeps the selection (group drag); a
          // multi-modifier adds instead of replacing.
          const multi = this.multiKeyPressed;
          const target = this.nodeLookup.get(id);
          if (target && !target.selected) {
            for (const internal of this.nodeLookup.values()) {
              internal.selected = multi
                ? internal.selected || internal.id === id
                : internal.id === id;
            }
            for (const n of this.nodes) {
              n.selected = multi ? n.selected || n.id === id : n.id === id;
            }
          }
          this.callbacks.onNodeMouseDown?.(JSON.stringify({ id, multi }));
        },
        onDragStart: (_e, _items, node) => {
          this.callbacks.onNodeDragStart?.(JSON.stringify({ id: node?.id ?? nodeId }));
        },
        onDragStop: (_e, _items, node) => {
          this.callbacks.onNodeDragStop?.(JSON.stringify({ id: node?.id ?? nodeId }));
        },
      });
      this.dragInstances.set(nodeId, instance);
    }
    this.updateNodeDrag(el, nodeId);
  }

  updateNodeDrag(el: HTMLDivElement, nodeId: string) {
    const instance = this.dragInstances.get(nodeId);
    if (!instance) return;
    const node = this.nodeLookup.get(nodeId);
    instance.update({
      domNode: el,
      nodeId,
      isSelectable: node?.selectable ?? true,
      noDragClassName: 'nodrag',
      handleSelector: node?.dragHandle ?? undefined,
      nodeClickDistance: this.options.nodeClickDistance,
    });
  }

  detachNodeDrag(nodeId: string) {
    this.dragInstances.get(nodeId)?.destroy();
    this.dragInstances.delete(nodeId);
  }

  private dragStoreItems() {
    return {
      nodes: this.nodes,
      nodeLookup: this.nodeLookup,
      edges: this.edges,
      nodeExtent: this.options.nodeExtent ?? infiniteExtent,
      snapGrid: this.options.snapGrid,
      snapToGrid: this.options.snapToGrid,
      nodeOrigin: this.options.nodeOrigin,
      multiSelectionActive: false,
      domNode: this.domNode,
      transform: this.transform,
      autoPanOnNodeDrag: this.options.autoPanOnNodeDrag,
      nodesDraggable: this.options.nodesDraggable,
      selectNodesOnDrag: this.options.selectNodesOnDrag,
      nodeDragThreshold: this.options.nodeDragThreshold,
      autoPanSpeed: this.options.autoPanSpeed,
      panBy: this.panByInternal,
      unselectNodesAndEdges: () => {
        this.callbacks.onUnselectNodesAndEdges?.(JSON.stringify({}));
      },
      onError: (code: string, message: string) => {
        this.callbacks.onError?.(code, message);
      },
      updateNodePositions: (
        dragItems: Map<string, NodeDragItem | InternalNodeBase>,
        dragging = false
      ) => {
        const changes: unknown[] = [];
        for (const [id, dragItem] of dragItems) {
          // keep the JS-side lookup fresh so ongoing drag math is stable
          const internal = this.nodeLookup.get(id);
          if (internal) {
            internal.position = dragItem.position;
            internal.internals.positionAbsolute = dragItem.internals.positionAbsolute;
            internal.dragging = dragging;
          }
          changes.push({
            id,
            type: 'position',
            position: dragItem.position,
            positionAbsolute: dragItem.internals.positionAbsolute,
            dragging,
          });
        }
        this.callbacks.onNodeChanges?.(JSON.stringify(changes));
      },
    };
  }

  /* ------------------------------------------------------------------ */
  /* connections (XYHandle)                                              */

  attachHandle(el: HTMLDivElement) {
    const down = (event: MouseEvent | TouchEvent) => {
      const isMouseTriggered = 'clientX' in event;
      const connectableStart = el.classList.contains('connectablestart');
      if (!connectableStart) return;
      if (isMouseTriggered && (event as MouseEvent).button !== 0) return;
      this.handlePointerDown(event, el);
    };
    el.addEventListener('mousedown', down as EventListener);
    el.addEventListener('touchstart', down as EventListener, { passive: true });
  }

  private handlePointerDown(
    event: MouseEvent | TouchEvent,
    handleEl: Element,
    override?: {
      nodeId: string;
      handleId: string | null;
      isTarget: boolean;
      edgeUpdaterType: 'source' | 'target';
      onConnect: (connection: system.Connection) => void;
    }
  ) {
    const nodeId = override ? override.nodeId : handleEl.getAttribute('data-nodeid');
    const handleId = override ? override.handleId : handleEl.getAttribute('data-handleid');
    const isTarget = override ? override.isTarget : handleEl.classList.contains('target');
    if (!nodeId) return;

    XYHandle.onPointerDown(event, {
      isValidConnection: this.validateConnection,
      edgeUpdaterType: override?.edgeUpdaterType,
      handleDomNode: handleEl as HTMLDivElement,
      autoPanOnConnect: this.options.autoPanOnConnect,
      connectionMode: this.options.connectionMode as ConnectionMode,
      connectionRadius: this.options.connectionRadius,
      domNode: this.domNode,
      nodeLookup: this.nodeLookup,
      lib: this.options.lib,
      isTarget,
      handleId,
      nodeId,
      flowId: this.options.flowId,
      panBy: this.panByInternal,
      cancelConnection: () => {
        this.connection = initialConnection;
        this.updatePanZoom();
        this.callbacks.onConnectionUpdate?.(
          JSON.stringify(serializeConnectionState(this.connection))
        );
      },
      onConnectStart: (_evt, params) => {
        this.callbacks.onConnectStart?.(JSON.stringify(params));
      },
      onConnect: (connection) => {
        if (override) {
          override.onConnect(connection);
        } else {
          this.callbacks.onConnect?.(JSON.stringify(connection));
        }
      },
      onConnectEnd: (_evt, finalState) => {
        this.callbacks.onConnectEnd?.(
          JSON.stringify({
            isValid: finalState.isValid,
            from: finalState.from,
            fromHandle: serializeHandle(finalState.fromHandle),
            fromPosition: finalState.fromPosition,
            fromNode: finalState.fromNode?.id ?? null,
            to: finalState.to,
            toHandle: serializeHandle(finalState.toHandle),
            toPosition: finalState.toPosition,
            toNode: finalState.toNode?.id ?? null,
          })
        );
      },
      updateConnection: (connection) => {
        const wasInProgress = this.connection.inProgress;
        this.connection = connection;
        if (!wasInProgress && connection.inProgress) {
          // disable pinch zooming while connecting
          this.updatePanZoom();
        }
        this.callbacks.onConnectionUpdate?.(
          JSON.stringify(serializeConnectionState(connection))
        );
      },
      getTransform: () => this.transform,
      getFromHandle: () =>
        this.connection.inProgress ? this.connection.fromHandle : null,
      autoPanSpeed: this.options.autoPanSpeed,
      dragThreshold: this.options.connectionDragThreshold,
    });
  }

  /* ------------------------------------------------------------------ */
  /* edge reconnection                                                   */

  attachReconnectAnchor(el: Element) {
    const down = (event: Event) => {
      const mouseEvent = event as MouseEvent | TouchEvent;
      if ('button' in mouseEvent && mouseEvent.button !== 0) return;
      const edgeId = el.getAttribute('data-edgeid');
      const anchorType = el.getAttribute('data-anchortype');
      if (!edgeId || !anchorType) return;
      const edge = this.edgeLookup.get(edgeId);
      if (!edge) return;
      // dragging the source anchor re-anchors the source end, so the
      // connection starts from the edge's (fixed) target handle — and
      // vice versa
      const opposite =
        anchorType === 'source'
          ? { nodeId: edge.target, handleId: edge.targetHandle ?? null, type: 'target' as const }
          : { nodeId: edge.source, handleId: edge.sourceHandle ?? null, type: 'source' as const };
      this.handlePointerDown(mouseEvent, el, {
        nodeId: opposite.nodeId,
        handleId: opposite.handleId,
        isTarget: opposite.type === 'target',
        edgeUpdaterType: opposite.type,
        onConnect: (connection) => {
          this.callbacks.onReconnect?.(JSON.stringify({ edgeId, connection }));
        },
      });
    };
    el.addEventListener('mousedown', down);
    el.addEventListener('touchstart', down, { passive: true });
  }

  /* ------------------------------------------------------------------ */
  /* resizer (XYResizer)                                                 */

  attachResizer(el: HTMLDivElement, nodeId: string, paramsJson: Json) {
    const key = nodeId + '/' + (el.getAttribute('data-resizer') || '');
    let instance = this.resizerInstances.get(key);
    if (!instance) {
      instance = XYResizer({
        domNode: el,
        nodeId,
        getStoreItems: () => ({
          nodeLookup: this.nodeLookup,
          transform: this.transform,
          snapGrid: this.options.snapGrid,
          snapToGrid: this.options.snapToGrid,
          nodeOrigin: this.options.nodeOrigin,
          paneDomNode: this.domNode,
        }),
        onChange: (change, childChanges) => {
          const changes: unknown[] = [];
          const positionChange = {
            x: change.x,
            y: change.y,
          };
          if (typeof positionChange.x === 'number' || typeof positionChange.y === 'number') {
            const node = this.nodeLookup.get(nodeId);
            changes.push({
              id: nodeId,
              type: 'position',
              position: {
                x: positionChange.x ?? node?.position.x ?? 0,
                y: positionChange.y ?? node?.position.y ?? 0,
              },
            });
          }
          if (typeof change.width === 'number' && typeof change.height === 'number') {
            changes.push({
              id: nodeId,
              type: 'dimensions',
              dimensions: { width: change.width, height: change.height },
              resizing: true,
              setAttributes: true,
            });
          }
          for (const childChange of childChanges) {
            changes.push({
              id: childChange.id,
              type: 'position',
              position: childChange.position,
            });
          }
          this.callbacks.onResizeChanges?.(JSON.stringify(changes));
        },
        onEnd: () => {
          this.callbacks.onResizeChanges?.(
            JSON.stringify([{ id: nodeId, type: 'dimensions', resizing: false }])
          );
        },
      });
      this.resizerInstances.set(key, instance);
    }
    this.updateResizer(nodeId, el, paramsJson);
  }

  updateResizer(nodeId: string, el: HTMLDivElement, paramsJson: Json) {
    const key = nodeId + '/' + (el.getAttribute('data-resizer') || '');
    const instance = this.resizerInstances.get(key);
    if (!instance) return;
    const p = JSON.parse(paramsJson || '{}');
    instance.update({
      controlPosition: p.controlPosition ?? 'bottom-right',
      boundaries: {
        minWidth: p.minWidth ?? 10,
        minHeight: p.minHeight ?? 10,
        maxWidth: p.maxWidth ?? Number.MAX_VALUE,
        maxHeight: p.maxHeight ?? Number.MAX_VALUE,
      },
      keepAspectRatio: p.keepAspectRatio ?? false,
      resizeDirection: p.resizeDirection ?? undefined,
      onResizeStart: () => {
        this.callbacks.onResizeStart?.(JSON.stringify({ id: nodeId }));
      },
      onResize: undefined,
      onResizeEnd: () => {
        this.callbacks.onResizeEnd?.(JSON.stringify({ id: nodeId }));
      },
      shouldResize: undefined,
    });
  }

  detachResizer(nodeId: string, el: HTMLDivElement) {
    const key = nodeId + '/' + (el.getAttribute('data-resizer') || '');
    this.resizerInstances.get(key)?.destroy();
    this.resizerInstances.delete(key);
  }

  /* ------------------------------------------------------------------ */
  /* minimap (XYMinimap)                                                 */

  attachMinimap(el: SVGSVGElement, paramsJson: Json) {
    if (!this.minimap) {
      this.minimap = XYMinimap({
        domNode: el,
        panZoom: this.panZoom,
        getTransform: () => this.transform,
        getViewScale: () => this.minimapViewScale,
      });
      el.addEventListener('click', (event) => {
        if (!this.minimap) return;
        const [x, y] = this.minimap.pointer(event) ?? [0, 0];
        this.callbacks.onMinimapClick?.(JSON.stringify({ x, y }));
      });
    }
    this.updateMinimap(paramsJson);
  }

  updateMinimap(paramsJson: Json) {
    if (!this.minimap) return;
    const p = JSON.parse(paramsJson || '{}');
    if (typeof p.viewScale === 'number') {
      this.minimapViewScale = p.viewScale;
    }
    this.minimap.update({
      translateExtent: this.options.translateExtent ?? infiniteExtent,
      width: this.width,
      height: this.height,
      inversePan: p.inversePan ?? false,
      zoomStep: p.zoomStep ?? 1,
      pannable: p.pannable ?? true,
      zoomable: p.zoomable ?? true,
    });
  }

  /* ------------------------------------------------------------------ */
  /* pane                                                                */

  private handlePaneClick = (event: MouseEvent) => {
    if (this.suppressPaneClick) {
      this.suppressPaneClick = false;
      return;
    }
    if (event.target === this.paneNode) {
      this.callbacks.onPaneClick?.(JSON.stringify({}));
    }
  };

  /* ------------------------------------------------------------------ */

  destroy() {
    this.destroyed = true;
    this.paneNode.removeEventListener('click', this.handlePaneClick);
    this.paneNode.removeEventListener('mousedown', this.handleSelectionStart);
    this.domNode.removeEventListener('scroll', this.handleContainerScroll);
    window.removeEventListener('keydown', this.handleKeyChange);
    window.removeEventListener('keyup', this.handleKeyChange);
    window.removeEventListener('blur', this.handleWindowBlur);
    this.panZoom.destroy();
    this.minimap?.destroy();
    for (const d of this.dragInstances.values()) d.destroy();
    for (const r of this.resizerInstances.values()) r.destroy();
    this.dragInstances.clear();
    this.resizerInstances.clear();
    this.resizeObserver?.disconnect();
    this.containerObserver?.disconnect();
  }
}

const misoFlowGlobal = {
  createStore(domNode: HTMLDivElement, optionsJson: Json, callbacks: MisoFlowCallbacks) {
    return new MisoFlowStore(domNode, optionsJson, callbacks);
  },
  system,
};

// eslint-disable-next-line @typescript-eslint/no-explicit-any
(globalThis as any).MisoFlow = misoFlowGlobal;

export default misoFlowGlobal;
