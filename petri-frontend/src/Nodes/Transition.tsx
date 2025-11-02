import {
  Handle,
  Position,
  useConnection,
  useEdges,
  useKeyPress,
  type Node,
  type NodeProps,
} from "@xyflow/react";

export type Transition = Node<{ label: string }, "transition">;

export function Transition({ id, data }: NodeProps<Transition>) {
  const connection = useConnection();
  const edges = useEdges();
  const isTarget =
    connection.inProgress &&
    connection.fromNode.type !== "transition" &&
    !edges.some(
      (e) => e.source == connection.fromNode.id && e.target === id,
    );

  const isCtrlOrMetaPressed = useKeyPress(["Control", "Meta"]);
  return (
    <div className="transitionNode">
      <Handle
        type="source"
        position={Position.Top}
        className="transitionHandle"
        isConnectable={isCtrlOrMetaPressed && !connection.inProgress}
      />
      <div>{data.label}</div>
      <Handle
        type="target"
        position={Position.Bottom}
        className="transitionHandle"
        isConnectableStart={false}
        isConnectable={isTarget}
      />
    </div>
  );
}
