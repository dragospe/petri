import { ReactFlow, Background, Controls, type Edge } from "@xyflow/react";
import "@xyflow/react/dist/style.css";
import { useState, useCallback } from "react";
import { applyEdgeChanges, applyNodeChanges , addEdge} from "@xyflow/react";

const initialNodes = [
  {
    id: "n1",
    position: { x: 0, y: 0 },
    data: { label: "Node 1" },
    type: "input",
  },
  {
    id: "n2",
    position: { x: 100, y: 100 },
    data: { label: "Node 2" },
  },
];

const initialEdges : Array<Edge>= [];

export default function App() {
  const [nodes, setNodes] = useState(initialNodes);
  const [edges, setEdges] = useState(initialEdges);

  const onNodesChange = useCallback(
    (changes : any) =>
      setNodes((nodesSnapshot) => applyNodeChanges(changes, nodesSnapshot)),
    [],
  );
  const onEdgesChange = useCallback(
    (changes : any) =>
      setEdges((edgesSnapshot) => applyEdgeChanges(changes, edgesSnapshot)),
    [],
  );
  const onConnect = useCallback(
      (params : any) => setEdges((edgesSnapshot) => addEdge(params, edgesSnapshot)),
      [],
  );



    return (
        <div style={{ height: "100vh", width: "100vw" }}>
            <ReactFlow
                nodes={nodes}
                edges={edges}
                onNodesChange={onNodesChange}
                onEdgesChange={onEdgesChange}
                onConnect={onConnect}
              panOnScroll
              selectionOnDrag
              panOnDrag={[1,2]}
                fitView
            >
                <Background />
                <Controls />
            </ReactFlow>
        </div>
    );
}
