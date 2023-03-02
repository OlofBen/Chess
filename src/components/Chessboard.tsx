import React from "react";
import "./Chessboard.css";

const vertical = ["a", "b", "c", "d", "e", "f", "g", "h"];
const horizontal = [1, 2, 3, 4, 5, 6, 7, 8].reverse();//h1 is the top left corner

export default function Chessboard() {
  let squares:JSX.Element[] = [];
  horizontal.forEach((h) => {
    vertical.forEach((v) => {
      let spuare = isWhite(squares.length) 
        ? <span className="square square-white">{v + h}</span> 
        : <span className="square square-black">{v + h}</span>
      
      squares.push(spuare);
    });
  });
  return <div id="chessboard">{squares}</div>;
}   

function isWhite(numberOfSqueres:number) {
  let startsOfRow = Math.floor(numberOfSqueres/8)
  return (numberOfSqueres + startsOfRow) % 2 === 0;
}
