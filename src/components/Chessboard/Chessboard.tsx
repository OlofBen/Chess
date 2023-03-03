import "./Chessboard.css";
import Square from "../Square/Square";
import { startingPosition} from "./Paths";
import { MouseEvent } from "react";

const column = ["a", "b", "c", "d", "e", "f", "g", "h"];
const row = [1, 2, 3, 4, 5, 6, 7, 8].reverse();//h1 is the top left corner

export default function Chessboard() {
  let squares:JSX.Element[] = [];
  row.forEach((r) => {
    column.forEach((c) => {
      let image = startingPosition.get(c + r);
      squares.push(<Square key={c+r} row={r} column={c} image={image}/>);
    });
  });
  return <div 
    onMouseMove={e => movePiece(e)} 
    onMouseDown={e => grabPiece(e)} 
    onMouseUp={() => grabbedPiece = null}
    id="chessboard"
    >
      {squares}
    </div>;
}

let grabbedPiece: HTMLElement | null = null;

function grabPiece(e: MouseEvent): void {
  let target = e.target as HTMLElement;
  if (target.classList.contains("piece")) {
    grabbedPiece = target;
    movePieceToMouse(e, grabbedPiece);
  }
}

function movePiece(e: MouseEvent): void {
  if (grabbedPiece) {
    movePieceToMouse(e, grabbedPiece);
  }
}

function movePieceToMouse(mouse: MouseEvent ,piece: HTMLElement): void {
  const x = mouse.clientX - piece.offsetWidth / 2;
  const y = mouse.clientY - piece.offsetHeight / 2;
  piece.style.position = "absolute";
  piece.style.left = x + "px";
  piece.style.top = y + "px";
}

