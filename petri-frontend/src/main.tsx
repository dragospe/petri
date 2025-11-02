
import { createRoot } from "react-dom/client";
import "./index.css";
import '@xyflow/react/dist/style.css';
import App from "./App.tsx";

createRoot(document.getElementById("root")!).render(
    <div style={{ height: "800", width: "800" }}>
            <App />
    </div>
);
