import chariot.Client
import chariot.model.Event
import chariot.model.GameStateEvent
import chess.Board
import chariot.model.Enums.Status
import scala.util.Try
import engine.Engine

object LichessBot:
  val token : String = scala.io.Source.fromFile("token.txt").mkString.strip()
  val botClient = Client.auth(token).bot()
  val engine = Engine()

  @main
  def start() = 
    while true do
      println(Try{checkForEvents()}) // if we crash, we want to restart
      Thread.sleep(1000)


  def checkForEvents() =
    val eventStream = botClient.connect().stream()
    eventStream.forEach( event => 
      println(event)
      println("We got something!")
      event match
        case challenge : Event.ChallengeEvent => 
          println(s"Challenge: ${challenge.id}")
          println(botClient.acceptChallenge(challenge.id))
        case gameStart : Event.GameStartEvent =>
          println(s"Game: ${gameStart.id}")
          connectToGame(gameStart.id())
        case other => 
          println("We got something else!")
          println(other)
        
    )

    def connectToGame(id : String) = 
    
      botClient.chat(id, "I wish you a pleasant game!")
      val gameStream = botClient.connectToGame(id).stream()

      gameStream.forEach( event => 
        println(s"We got something in game ${id}!")
        println(event)
        event match
          case weShouldStart : GameStateEvent.Full => 
            println(s"Game: ${weShouldStart.id}")
            findMoveOnBoard(Board.startingPosition(), id)
          case chat : GameStateEvent.Chat => 
            println(s"Chat: ${chat.text}")
          case gameState : GameStateEvent.State => 
            val status = gameState.status()
            println(s"Status: ${status}")
            if status == Status.mate || status == Status.resign || status == Status.timeout || status == Status.stalemate then
              println("Game over!")
            else
              val currentBoard = 
                var board = Board.startingPosition()
                gameState.moveList().forEach( move => 
                  board = board.move(move)
                )
                board
              findMoveOnBoard(currentBoard, id)
                
      )

  def findMoveOnBoard(board : Board, id : String) = 
    val move = engine.bestMove(board, 4)
    val moveString = move.toString()
    botClient.move(id, moveString)

 





