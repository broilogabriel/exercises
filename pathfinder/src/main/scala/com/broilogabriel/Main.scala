package com.broilogabriel

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props, Terminated}
import com.broilogabriel.Action.{Finish, Move}
import com.broilogabriel.Terminator.WatchMe

import scala.collection.mutable.ArrayBuffer

/**
  * Created by broilogabriel on 4/27/2017.
  *
  * I choose akka mainly because of the parallelism provided by it's implementation and quick environment setup. I'm
  * pretty sure there is room for improvement in my solution and maybe akka is an overkill for this problem.
  *
  */
object Main extends App {

  val start = 0
  val tile = Tile(start / 10, start % 10)

  val actorSystem = ActorSystem.create("pathfinder")
  val action = actorSystem.actorOf(Props[Action], name = tile.name)
  val terminator = actorSystem.actorOf(Props[Terminator], name = "terminator")
  terminator ! WatchMe(action)
  action ! Move(start, tile)

}

object Rules {
  val BOARD_SIZE = 10
  val TOTAL_TILES: Int = math.pow(BOARD_SIZE, 2).toInt
  val DIAGONAL_DISTANCE = 2
  val HOR_VER_DISTANCE = 3
}

/**
  * Represents the tiles in the board, it has helper methods to find the next moves based on the rules defined by
  * [[Rules]]
  *
  * @param x horizontal coordinate in the board
  * @param y vertical coordinate in the board
  */
case class Tile(x: Int, y: Int) {

  val name = s"${x}_$y"
  val isValid: Boolean = x >= 0 && x < Rules.BOARD_SIZE && y >= 0 && y < Rules.BOARD_SIZE

  def availableMoves(path: Seq[Tile]): Seq[Tile] = {
    moves().filter(tile => tile.isValid && !path.contains(tile))
  }

  def moves(): Seq[Tile] = diagonal() ++ hor_ver()

  def hor_ver(distance: Int = Rules.HOR_VER_DISTANCE): Seq[Tile] = {
    Seq(
      Tile(x + distance, y), Tile(x - distance, y),
      Tile(x, y + distance), Tile(x, y - distance)
    )
  }

  def diagonal(distance: Int = Rules.DIAGONAL_DISTANCE): Seq[Tile] = {
    Seq(
      Tile(x + distance, y + distance), Tile(x + distance, y - distance),
      Tile(x - distance, y + distance), Tile(x - distance, y - distance)
    )
  }

}

/**
  * Action messages used by the [[Action]] actor
  */
object Action {

  case class Move(startingPoint: Int, tile: Tile, path: Seq[Tile] = Seq.empty, badPaths: Seq[Seq[Tile]] = Seq.empty)

  case class Finish(startingPoint: Int, path: Seq[Tile])

}

/**
  * This actor is responsible for moving the pawn across the board.
  *
  * The workflow for this class starts with a [[Move]] action, every Move contains a [[Tile]] associated and a few
  * other information responsible for finding a path to move though all board without using a tile more than once.
  *
  * In case of an invalid path a bad path route is added in the [[Move]] message to prevent to take that path again.
  *
  * Once it reaches the maximum steps to complete the board a [[Finish]] message is sent and it prints the board with
  * all tiles and the order the pawn moved across the board. Eg:  9 - [0|0] === <order> - [<x>|<y>]
  */
class Action extends Actor with ActorLogging {

  def receive(): Actor.Receive = {

    case move: Move =>
      // includes the move in the received path
      val path = move.path :+ move.tile
      // filter the available moves and list the number of moves for later use
      val moves = move.tile.availableMoves(path)
        .filter(t => !move.badPaths.contains(Seq(move.tile, t)))
        .map(tile => (tile.availableMoves(path :+ tile).size + 1, tile))
      if (moves.isEmpty) {
        if (path.size == Rules.TOTAL_TILES) {
          val ref = context.system.actorOf(Props[Action], name = s"Finished-${move.startingPoint}")
          context.actorSelection(context.system / "terminator") ! WatchMe(ref)
          // sends Finish message to print the board
          ref ! Finish(move.startingPoint, path)
          context.stop(self)
        } else {
          // bad path identified, will send a message to move back the pawn and try another path
          val badPath = path.slice(path.size - 2, path.size)
          val alternative = path.diff(badPath)
          self ! move.copy(tile = alternative.last, path = alternative, badPaths = move.badPaths :+ badPath)
        }
      } else {
        // identify the tile with less moves available and send a move action to move the pawn
        val tilesWithLessMoves = moves.filter(_ == moves.minBy(_._1)).map(_._2)
        tilesWithLessMoves.foreach {
          tile =>
            val ref = context.actorOf(Props[Action], name = tile.name)
            context.actorSelection(context.system / "terminator") ! WatchMe(ref)
            ref ! move.copy(tile = tile, path = path)
        }
      }

    case finished: Finish =>
      log.info(s"Starting Point ${finished.startingPoint} Done: ${finished.path}")
      // format the path for printing in the console
      finished.path.zipWithIndex.map {
        case (t, i) => ((t.x, t.y), i)
      }.sorted.grouped(Rules.BOARD_SIZE).foreach {
        list => log.info(list.map(v => f"${v._2}%2d - [${v._1._1}%d|${v._1._2}%d]").mkString("| ", " | ", " |"))
      }
      val count = finished.startingPoint + 1
      // creates another Action actor to move the pawn across the board from the next starting point
      if (count < Rules.TOTAL_TILES) {
        val ref = context.system.actorOf(Props[Action], name = count.toString)
        context.actorSelection(context.system / "terminator") ! WatchMe(ref)
        val tile = Tile(count / 10, count % 10)
        ref ! Move(count, tile)
      } else {
        // ends the execution when all 100 tiles were already executed
        context.system.terminate()
      }
      context.stop(self)

  }

}


object Terminator {

  case class WatchMe(ref: ActorRef)

}

/**
  * Actor responsible for monitoring other actors and terminate the context.
  *
  * Obs. it has to be improved, I'm losing some references and I had to kill the context using some brute force
  */
class Terminator extends Actor with ActorLogging {

  val watched: ArrayBuffer[ActorRef] = ArrayBuffer.empty[ActorRef]

  final def receive: Actor.Receive = {
    case WatchMe(ref) =>
      context.watch(ref)
      watched += ref
    case Terminated(ref) =>
      watched -= ref
      if (watched.isEmpty) {
        context.system.terminate()
      }
  }
}
