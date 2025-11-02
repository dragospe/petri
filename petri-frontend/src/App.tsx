import { useCallback, useRef, useState } from "react";
import {
  Background,
  ReactFlow,
  useNodesState,
  useEdgesState,
  addEdge,
  ReactFlowProvider,
  type Node,
  type OnConnect,
  type Edge,
  MarkerType,
  Controls,
  ControlButton,
  useReactFlow,
} from "@xyflow/react";

import "@xyflow/react/dist/style.css";
import { Place } from "./Nodes/Place";
import FloatingEdge from "./Edges/FloatingEdge";
import CustomConnectionLine from "./Edges/CustomConnectionLine";
import { Transition } from "./Nodes/Transition";
import "./index.css";
import Sidebar, { type SidebarMode } from "./Sidebar/Sidebar";
import MkPlaceButton from "./ControlButtons/MkPlace";
import MkTransitionButton from "./ControlButtons/MkTransition";
import { v4 as uuidv4 } from "uuid";


const connectionLineStyle = {
  stroke: "#b1b1b7",
};

const nodeTypes = {
  place: Place,
  transition: Transition,
};

const edgeTypes = {
  floating: FloatingEdge,
};

const defaultEdgeOptions = {
  markerEnd: {
    type: MarkerType.ArrowClosed,
    color: "#b1b1b7",
  },
};

const initialNodes: Node[] = [];

const nodeOrigin: [number, number] = [0.5, 0];

// Only one MkNodeMode can be active at time.
type MkNodeMode = "none" | "place" | "transition"


// Top level react-flow component
const MyFlow = () => {
  const reactFlowWrapper = useRef(null);

  // useReactFlow is a hook that gives us the ability to inspect various aspects of the flow itself.
  const { screenToFlowPosition, updateNodeData } = useReactFlow();


  const [nodes, setNodes, onNodesChange] = useNodesState<Node>(initialNodes);
  const [edges, setEdges, onEdgesChange] = useEdgesState<Edge>([]);
  const [mkNodeMode , setMkNodeMode] = useState<MkNodeMode>("none");
  const [sidebarMode, setSidebarMode] = useState<SidebarMode>({mode: "default"})




  const onNodeClick  = useCallback(
    (_, n) => {
      const setNodeTitle = (newTitle : string) => updateNodeData(n.id, {label: newTitle});

      setSidebarMode(
        ({mode}) => {
          if(mode === "default"){
            return {mode: "nodeInspect", node: n, setNodeTitle}
          }
          else {return {mode: "default"}}})}
      , [updateNodeData, setSidebarMode]);


  const onConnect: OnConnect = useCallback(
    (params) => setEdges((eds) => addEdge(params, eds)),
    [],
  );

  // Toggle "Add Place Mode" when you click on the `P` in the control panel
  const handleMkPlaceClick = useCallback(() => {
    setMkNodeMode((oldMode) => (oldMode === "place" ? "none" : "place"));
  }, []);

  // Toggle "Add Transition Mode" when you click on the `T` in the control panel
  const handleMkTransitionClick = useCallback(() => {
    setMkNodeMode((oldMode) =>
      oldMode === "transition" ? "none" : "transition",
    );
  }, []);


  // Depending on the mkNodeMode, click on an empty space in the pane should
  // either add a node or a transition
  const handlePaneClick = useCallback(
    (e) => {
      if (mkNodeMode !== "none") {
        const position = screenToFlowPosition({
          x: e.clientX,
          y: e.clientY,
        });

        const newPlace: Node = {
          id: uuidv4(),
          type: mkNodeMode,
          position: position,
          data: {},
        };

        setNodes((nds) => nds.concat(newPlace));
        console.log(nodes);
      }
    },
    [mkNodeMode, screenToFlowPosition, setNodes],
  );

  // Goal: highlight pre/post set of transitions, transitions into and out of for nodes, source/target for edge
  const handlePlaceMouseEnter = useCallback((_e, n) => {
     const edgesFrom = edges.filter(e => e.source === n)
     //const _edgesTo = edges.filter(e => e.target == n)
     const intoTransitions = edgesFrom.map(e => e.target)
     //const outOfTransitions = edgesTo.map(e => e.source)

     setNodes((nds) =>
       nds.map((n) =>{
         if (intoTransitions.includes(n.id))
         {return { ...n, className: n.className + "highlight"}}
         else {return {...n}};
       }
  ))}, [setNodes]);

  return (
    <div className="topLevel" style={{ height: "100vh", width: "100vw" }}>
      <div className="wrapper" ref={reactFlowWrapper}>
        <ReactFlow
          nodes={nodes}
          edges={edges}
          onNodesChange={onNodesChange}
          onEdgesChange={onEdgesChange}
          onConnect={onConnect}
          fitView
          fitViewOptions={{ padding: 2 }}
          onNodeClick={onNodeClick}
          nodeOrigin={nodeOrigin}
          nodeTypes={nodeTypes}
          onNodeMouseEnter={handlePlaceMouseEnter}
          edgeTypes={edgeTypes}
          defaultEdgeOptions={defaultEdgeOptions}
          connectionLineComponent={CustomConnectionLine}
          connectionLineStyle={connectionLineStyle}
          multiSelectionKeyCode={null}
          zoomActivationKeyCode={null}
          onPaneClick={handlePaneClick}
        >
          <Background />
          <Controls orientation="horizontal">
            <ControlButton onClick={handleMkPlaceClick}>
              <MkPlaceButton isActive={mkNodeMode === "place"} />
            </ControlButton>
            <ControlButton onClick={handleMkTransitionClick}>
              <MkTransitionButton isActive={mkNodeMode === "transition"} />
            </ControlButton>
          </Controls>
        </ReactFlow>
      </div>
      <Sidebar nodes={nodes} sidebarMode={sidebarMode} />
    </div>
  );
};

export default () => (
  <ReactFlowProvider>
    <MyFlow />
  </ReactFlowProvider>
);
