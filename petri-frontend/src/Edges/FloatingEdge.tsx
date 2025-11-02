import { BaseEdge, getStraightPath, useInternalNode, type Edge, type EdgeProps } from '@xyflow/react';

import { getEdgeParams } from './Utils.tsx'

type FloatingEdge = Edge<{id, source, target, markerEnd, style}, 'floating'>

function FloatingEdge({ id, source, target, markerEnd, style } : EdgeProps<FloatingEdge>)  {
  const sourceNode = useInternalNode(source);
  const targetNode = useInternalNode(target);

  if (!sourceNode || !targetNode) {
    return null;
  }

  const { sx, sy, tx, ty } = getEdgeParams(sourceNode, targetNode);

  const [path] = getStraightPath({
    sourceX: sx,
    sourceY: sy,
    targetX: tx,
    targetY: ty,
  });

  return (
    <BaseEdge
      id={id}
      className="react-flow__edge-path"
      path={path}
      markerEnd={markerEnd}
      style={style}
    />
  );
}

export default FloatingEdge;
