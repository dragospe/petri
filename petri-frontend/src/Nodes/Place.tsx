import {
  Handle,
  Position,
  useConnection,
  useEdges,
  useKeyPress,
  type Node,
  type NodeProps,
} from "@xyflow/react";


export type Place = Node<{ label : String }, "place">;

export function Place({ id, data }: NodeProps<Place>) {
  const connection = useConnection();
  const edges = useEdges();


    const isTarget =
        connection.inProgress
        // A Place can only be connected to a Transition and Vice Versa
        && connection.fromNode.type !== "place"
        // Only one edge can exist in each direction
        && !edges.some(e =>
          e.source == connection.fromNode.id &&
          e.target === id);
    const isCtrlOrMetaPressed = useKeyPress(["Control", "Meta"]);

    return (
        <div className="placeNode">
            <div className="placeNodeBody">
                <Handle
                    className="placeHandle"
                    position={Position.Right}
                    type="source"
                    isConnectable={isCtrlOrMetaPressed && !connection.inProgress}
                />
                <Handle
                    className="placeHandle"
                    position={Position.Left}
                    type="target"
                    isConnectableStart={false}
                    isConnectable={isTarget}
                />
                <div className="placeNodeLabel">
                  {data.label}
                </div>
            </div>
        </div>
    );
}

// We want the entire node surface to be connectable.
// https://reactflow.dev/examples/nodes/easy-connect
