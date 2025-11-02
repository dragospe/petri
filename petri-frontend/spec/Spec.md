# Petri Frontend Spec


This document specifies the desired behavior of the frontend.

## Node Types

Two node types should exist: `Place` and `Transition`.

- `Place`s should:
   - be connectable only to `Transition`s 
   - appear as circles
   - be able to display a number in their center
   - be able to display a title beneath them

- `Transition`s should:
   - appear as rectangles
   - be connectable only to `Place`s
   - display a title beneath them
   
   
- Nodes can be moved by clicking and dragging


## Edges

Edges are the component that

- Edges should:
  - Be directed, with an arrow at exactly on end
  - Be able to carry a number and a title
  - Be able to be inverted
  
  
## Connections

Connections FROM a component can be initiated by `Ctrl + M1` clicking anywhere on that component.

## Viewport behavior

- Scrolling should zoom in or out
- Panning is `Ctrl + Drag` or `Middle Mouse + Drag`
- Libre-cad style shift-click 

