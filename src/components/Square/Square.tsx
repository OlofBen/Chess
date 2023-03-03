import "./Square.css";
const letters = ["a", "b", "c", "d", "e", "f", "g", "h"];

function isWhite(row: number, col: string) {
    let colNumber = letters.indexOf(col.toLowerCase());
    return (row + colNumber) % 2 === 0; 
}

export default function Square(props: { row: number, column: string, image?: string}) {
    let className = isWhite(props.row, props.column) ? "square square-white" : "square square-black";
    return <div className={className}>
            {
            (props.image) && 
            <div style={{backgroundImage: `url(${props.image})`}} className="piece"></div>
            }
        </div>

}