import "./Chessboard.css";
import Square from "../Square/Square";
import { startingPosition} from "./Paths";

const column = ["a", "b", "c", "d", "e", "f", "g", "h"];
const row = [1, 2, 3, 4, 5, 6, 7, 8].reverse();//h1 is the top left corner

export default function Chessboard() {
  let squares:JSX.Element[] = [];
  row.forEach((r) => {
    column.forEach((c) => {
      let image = startingPosition.get(c + r);
      squares.push(<Square row={r} column={c} image={image}/>);
    });
  });
  return <div id="chessboard">{squares}</div>;
}
