import cats._

import scala.annotation.tailrec
import scala.io.Source
import util.{Failure, Try, Using}
import scala.collection.immutable._
import scala.collection.mutable
import java.net.URL
import scala.util.matching.Regex

object coordinate {
  type X = Int
  type Y = Int

  case class Coordinate(x: X, y: Y)

  trait WithCoordinate {
    def coordinate: Coordinate
  }
}

object roverCommands {

  import coordinate._

  sealed trait Command {
    override def toString: String = this match {
      case Forward => "fw"
      case ClockRotate => "cr"
      case CounterClockRotate => "ccr"
      case GoTo(coordinate) => s"goto ${coordinate.x},${coordinate.y}"
    }

  }

  object Command {
    def fromString(string: String): Command = {
      val coordinateRe: Regex = "goto (\\d+),(\\d+)".r
      string match {
        case "fw" => Forward
        case "cr" => ClockRotate
        case "ccr" => CounterClockRotate
        case coordinateRe(x, y) => GoTo(Coordinate(x.toInt, y.toInt))
      }
    }
  }

  case object Forward extends Command

  case class GoTo(coordinate: Coordinate) extends Command

  case object ClockRotate extends Command

  case object CounterClockRotate extends Command

}


object squares {

  import direction._

  sealed trait SquareArt {
    def toString: String
  }

  case object Rock extends SquareArt {
    override def toString: String = "X"
  }


  case object Plain extends SquareArt {
    override def toString: String = "."
  }

  case object RoverTrace extends SquareArt {
    override def toString: String = "o"
  }

  case object RoverNorth extends SquareArt with WithDirection {
    def direction: Direction = North

    override def toString: String = "^"
  }

  case object RoverEast extends SquareArt with WithDirection {
    def direction: Direction = East

    override def toString: String = ">"
  }

  case object RoverWest extends SquareArt with WithDirection {
    def direction: Direction = West

    override def toString: String = "<"
  }

  case object RoverSouth extends SquareArt with WithDirection {
    def direction: Direction = South

    override def toString: String = "\u234C"
  }


  object SquareArt {
    def apply(string: String): SquareArt = string match {
      case "X" => Rock
      case "." => Plain
      case "o" => RoverTrace
      case ">" => RoverEast
      case "<" => RoverWest
      case "^" => RoverNorth
      case "\u234C" => RoverSouth
      case _ => throw new Exception("unknown square")
    }
  }

}


object direction {

  import roverCommands._

  trait WithDirection {
    def direction: Direction
  }

  sealed trait Direction {
    def cost(newDirection: Direction): Int = moveInstructions(newDirection).size

    def moveInstructions(newDirection: Direction): List[Command]
  }

  case object South extends Direction {

    def moveInstructions(newDirection: Direction): List[Command] = newDirection match {
      case North => List(ClockRotate, ClockRotate, Forward)
      case East => List(ClockRotate, Forward)
      case West => List(CounterClockRotate, Forward)
      case South => List(Forward)
    }
  }

  case object North extends Direction {

    def moveInstructions(newDirection: Direction): List[Command] = newDirection match {
      case North => List(Forward)
      case East => List(CounterClockRotate, Forward)
      case West => List(ClockRotate, Forward)
      case South => List(ClockRotate, ClockRotate, Forward)
    }
  }

  case object East extends Direction {

    def moveInstructions(newDirection: Direction): List[Command] = newDirection match {
      case North => List(ClockRotate, Forward)
      case East => List(Forward)
      case West => List(ClockRotate, ClockRotate, Forward)
      case South => List(CounterClockRotate, Forward)
    }
  }

  case object West extends Direction {

    def moveInstructions(newDirection: Direction): List[Command] = newDirection match {
      case North => List(CounterClockRotate, Forward)
      case East => List(ClockRotate, ClockRotate, Forward)
      case West => List(Forward)
      case South => List(ClockRotate, Forward)
    }
  }


}

import squares._
import direction._
import coordinate._
import roverCommands._


case class RoverState(coordinate: Coordinate, direction: Direction) {
  def asSquareArt(): SquareArt = direction match {
    case South => RoverSouth
    case North => RoverNorth
    case East => RoverEast
    case West => RoverWest
  }
}

object drawer {
  implicit class Drawer(f: Field) {
    def draw(): Unit = {
      f.field.foreach(row =>
        println(row.fold("")((ac, el) => ac + el.toString))
      )
    }

    println("")
  }
}

class Field(val field: Vector[Vector[SquareArt]]) {

  lazy val yMaxOffest: Int = field.size - 1

  lazy val xMaxOffset: Int = field(0).size - 1

  final case class Neighbors(
                              val north: Option[Coordinate],
                              val south: Option[Coordinate],
                              val east: Option[Coordinate],
                              val west: Option[Coordinate]
                            ) {
    def asMap(): Map[Coordinate, Direction] = {
      Map.from(
        (north match {
          case Some(c) => Set(c -> North)
          case None => Set.empty[(Coordinate, Direction)]
        })
          ++ (south match {
          case Some(c) => Set(c -> South)
          case None => Set.empty[(Coordinate, Direction)]
        })
          ++ (east match {
          case Some(c) => Set(c -> East)
          case None => Set.empty[(Coordinate, Direction)]
        })
          ++ (west match {
          case Some(c) => Set(c -> East)
          case None => Set.empty[(Coordinate, Direction)]
        })
      )
    }
  }

  object Neighbors {
    def apply(coordinate: Coordinate): Neighbors = {
      val (x, y) = (coordinate.x, coordinate.y)
      val northY: Y = if (y == 0) yMaxOffest else y - 1
      val southY: Y = if (y == yMaxOffest) 0 else y + 1
      val westX: X = if (x == 0) xMaxOffset else x - 1
      val eastX: X = if (x == xMaxOffset) 0 else x + 1


      Neighbors(if (field(northY)(x) == Rock) None else Some(Coordinate(x, northY)),
        if (field(southY)(x) == Rock) None else Some(Coordinate(x, southY)),
        if (field(y)(eastX) == Rock) None else Some(Coordinate(eastX, y)),
        if (field(y)(westX) == Rock) None else Some(Coordinate(westX, y)))
    }
  }

  def move(from: RoverState, to: RoverState): Try[Field] = Try {
    field(to.coordinate.y)(to.coordinate.x) match {
      case Rock => throw new Exception(s"Emergency stop to avoid rock collision at ${to.coordinate.x},${to.coordinate.y}")
      case _ =>
        val f: Vector[Vector[SquareArt]] = field.updated(from.coordinate.y, field(from.coordinate.y).updated(from.coordinate.x, RoverTrace))
        val out: Vector[Vector[SquareArt]] = f.updated(to.coordinate.y, f(to.coordinate.y).updated(to.coordinate.x, to.asSquareArt()))
        Field(out)
    }
  }


  lazy val edges: Map[Coordinate, Neighbors] = (for {
    (row, y: Y) <- field.zipWithIndex
    (_, x: X) <- row.zipWithIndex
  } yield (Coordinate(x, y), Neighbors(Coordinate(x, y)))).toMap


  def heuristic(squareCoordinate: Coordinate, destCoordinate: Coordinate): Eval[Double] = Eval.later {
    val (x, y) = (squareCoordinate.x, squareCoordinate.y)
    val (destX, destY) = (destCoordinate.x, destCoordinate.y)
    List(
      (x - destX, y - destY),
      (x - destX, y - destY + yMaxOffest),
      (x - destX, y - destY - yMaxOffest),
      (x - destX + xMaxOffset, y - destY),
      (x - destX - xMaxOffset, y - destY),
      (x - destX + xMaxOffset, y - destY - yMaxOffest),
      (x - destX - xMaxOffset, y - destY - yMaxOffest),
      (x - destX + xMaxOffset, y - destY + yMaxOffest),
      (x - destX - xMaxOffset, y - destY + yMaxOffest))
      .map { case (x, y) => math.sqrt(x * x + y * y) }
      .max
  }

  trait WithAStarMetrics extends Ordered[WithAStarMetrics] {
    def hCost: Double

    def gCost: Double

    def fCost: Double = hCost + gCost

    override def compare(that: WithAStarMetrics): Int = this.fCost compare that.fCost
  }

  private case class AstarRoverOnField(coordinate: Coordinate, direction: Direction, hCost: Double, gCost: Double)
    extends WithDirection
      with WithCoordinate
      with WithAStarMetrics


  def reconstructPath(cameFrom: Map[Coordinate, AstarRoverOnField], current: AstarRoverOnField): List[Command] = {
    @tailrec
    def iter(roverOnField: AstarRoverOnField, acc: List[Command]): List[Command] =
      cameFrom.get(roverOnField.coordinate) match {
        case Some(oldRoverOnField) => iter(
          oldRoverOnField,
          oldRoverOnField.direction.moveInstructions(roverOnField.direction) ++ acc
        )
        case None => acc
      }

    iter(current, List[Command]())
  }

  def astar(roverOnField: RoverState, destinationCoordinate: Coordinate): Option[List[Command]] = {

    val openQueue: mutable.PriorityQueue[AstarRoverOnField] = mutable.PriorityQueue[AstarRoverOnField](
      AstarRoverOnField(
        roverOnField.coordinate,
        roverOnField.direction,
        this.heuristic(roverOnField.coordinate, destinationCoordinate).value,
        0
      )
    )


    val gScore: mutable.Map[Coordinate, Double] = mutable.Map(roverOnField.coordinate -> 0.toDouble)
    val fScore: mutable.Map[Coordinate, Double] = mutable.Map(
      roverOnField.coordinate -> this.heuristic(roverOnField.coordinate, destinationCoordinate).value
    )


    val cameFrom = mutable.Map[Coordinate, AstarRoverOnField]()

    while (openQueue.nonEmpty) {
      val current = openQueue.dequeue()
      if (current.coordinate == destinationCoordinate) {
        return Some(reconstructPath(cameFrom.toMap, current))
      }
      val nbr = Neighbors(current.coordinate).asMap()
      val candidateNbrs = nbr.keys.toSet
      candidateNbrs.foreach { nbrCoordinate =>
        val nbrDirection = nbr(nbrCoordinate)
        val nextAstarRover = AstarRoverOnField(
          nbrCoordinate,
          nbrDirection,
          this.heuristic(current.coordinate, nbrCoordinate).value,
          current.direction.cost(nbrDirection).toDouble + gScore.getOrElse(current.coordinate, 0.toDouble)
        )
        // tentative_gScore is the distance from start to the neighbor through current
        val tentative_gScore = gScore(current.coordinate) + nextAstarRover.gCost
        if (tentative_gScore < gScore.getOrElse(nextAstarRover.coordinate, Double.PositiveInfinity)) {
          // This path to neighbor is better than any previous one. Record it!
          cameFrom(nbrCoordinate) = current
          gScore(nbrCoordinate) = tentative_gScore
          fScore(nbrCoordinate) = gScore(nbrCoordinate) + nextAstarRover.hCost
          if (!openQueue.toQueue.contains(nextAstarRover)) {
            openQueue.enqueue(nextAstarRover)
          }
        }
      }
    }
    None
  }


}

object Field {

  import drawer._

  def apply(field: Vector[Vector[SquareArt]]): Field = {
    val d = new Field(field)
    d.draw()
    d
  }


  def fromFile(path: String): Try[Field] =
    Using(Source.fromFile(path)) {
      _.getLines()
        .map(_.split("").map(SquareArt.apply).toVector)
        .toVector
    }.map(Field.apply)

  def fromUrl(url: URL): Try[Field] = {
    Using(Source.fromURL(url)) {
      _.getLines()
        .map(_.split("").map(SquareArt.apply).toVector)
        .toVector
    }.map(Field.apply)
  }
}


object RoverService {
  def initalPosition(field: Field): Option[RoverService] = (for {
    (row, y) <- field.field.zipWithIndex
    (square, x) <- row.zipWithIndex
    if square.isInstanceOf[WithDirection]
  } yield square match {
    case Rock => None
    case Plain => None
    case RoverTrace => None
    case RoverNorth => Some(RoverState(Coordinate(x, y), North))
    case RoverEast => Some(RoverState(Coordinate(x, y), East))
    case RoverWest => Some(RoverState(Coordinate(x, y), West))
    case RoverSouth => Some(RoverState(Coordinate(x, y), South))
  })
    .dropWhile(_.isEmpty)
    .take(1)
    .head
    .map(new RoverService(_, field))
}


case class RoverService(roverState: RoverState, field: Field) {

  @tailrec
  final def runManyCommands(rs: RoverService, cmdLeft: List[Command]): RoverService = cmdLeft match {
    case head :: cmds => runManyCommands(rs.flatMap(head), cmds)
    case _ => this
  }

  def flatMap(cmd: Command): RoverService = {
    cmd match {
      case GoTo(coordinate) => field.astar(roverState, coordinate) match {
        case Some(commands) => runManyCommands(this, commands)
        case None =>
          println("Unreachable destination")
          this
      }
      case _ => sendSimpleCommand(cmd)
      match {
        case Some(to) => RoverService(to, field.move(roverState, to).get)
        case None => this
      }
    }
  }

  def sendSimpleCommand(cmd: Command): Option[RoverState] = {
    println(cmd)
    cmd match {
      case Forward => roverState.direction match {
        case North => field.edges(roverState.coordinate).north match {
          case Some(coordinate) => Some(RoverState(coordinate, roverState.direction))
          case None =>
            println("Unreachable terrain")
            None
        }
        case East => field.edges(roverState.coordinate).east match {
          case Some(coordinate) => Some(RoverState(coordinate, roverState.direction))
          case None =>
            println("Unreachable terrain")
            None
        }
        case West => field.edges(roverState.coordinate).west match {
          case Some(coordinate) => Some(RoverState(coordinate, roverState.direction))
          case None =>
            println("Unreachable terrain")
            None
        }
        case South => field.edges(roverState.coordinate).south match {
          case Some(coordinate) => Some(RoverState(coordinate, roverState.direction))
          case None =>
            println("Unreachable terrain")
            None
        }
      }
      case ClockRotate => Some(roverState.direction match {
        case North => RoverState(roverState.coordinate, East)
        case East => RoverState(roverState.coordinate, South)
        case South => RoverState(roverState.coordinate, West)
        case West => RoverState(roverState.coordinate, North)
      })
      case CounterClockRotate => Some(roverState.direction match {
        case North => RoverState(roverState.coordinate, West)
        case East => RoverState(roverState.coordinate, North)
        case South => RoverState(roverState.coordinate, East)
        case West => RoverState(roverState.coordinate, South)
      })
    }
  }
}


