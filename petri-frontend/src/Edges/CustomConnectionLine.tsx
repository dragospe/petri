// See https://reactflow.dev/examples/nodes/easy-connect

import { getStraightPath, type ConnectionLineComponentProps } from '@xyflow/react';
import {type Place} from "../Nodes/Place"

function CustomConnectionLine({ fromX, fromY, toX, toY, connectionLineStyle } : ConnectionLineComponentProps<Place>) {
    const [edgePath] = getStraightPath({
    sourceX: fromX,
    sourceY: fromY,
    targetX: toX,
    targetY: toY,
  });

  return (
    <g>
      <path style={connectionLineStyle} fill="none" d={edgePath} />
    </g>
  );
}

export default CustomConnectionLine;
