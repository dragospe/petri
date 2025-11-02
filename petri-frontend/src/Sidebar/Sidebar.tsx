import { type Node } from "@xyflow/react";
import { useState } from "react";

function nodeEntry(node) {
  return <li key={node.id}>{node.data.label}</li>;
}

export type SidebarMode = {mode : "default"} | {mode : "nodeInspect", node : Node, setNodeTitle : (newTitle : string) => void};

function sidebar({ nodes, sidebarMode} : {nodes : Node[], sidebarMode: SidebarMode}) {

  // Contains a list of places and transitions by their title
  const defaultSidebar = () => {
    const places = nodes.filter((node: Node) => node.type === "place");
    const transitions = nodes.filter(
      (node: Node) => node.type === "transition",
    );
    return (
      <aside>
        <h1>Places</h1>
        <ul>{places.map(nodeEntry)}</ul>
        <h1>Transitions</h1>
        <ul>{transitions.map(nodeEntry)}</ul>
      </aside>
    );
  };

    const nodeInspectSiderbar = (node, onInput) => {
      const [localLabel, setLocalLabel] = useState(node.data.label);

      return(
          <div className="nodeInspector">
          <h1>Node: {localLabel}</h1>
          uuid: {node.id}
          <form>
              <label>Label:</label><br/>
              <input
                     onChange={(evt) => {
                              setLocalLabel(evt.target.value)
                              onInput(evt.target.value)}
                     }
                     value={localLabel}
              />
          </form>
          </div>

      )

  };


  switch (sidebarMode.mode) {
    case "default":
      return defaultSidebar();
    case "nodeInspect":
          return nodeInspectSiderbar(sidebarMode.node, sidebarMode.setNodeTitle)
  }
}

export default sidebar
