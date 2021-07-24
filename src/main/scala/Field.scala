import cats._

import scala.annotation.tailrec
import scala.io.Source
import util.{Try, Using}
import scala.collection.immutable._
import scala.collection.mutable
import scala.collection.mutable.PriorityQueue
import scala.util.matching.Regex

sealed trait Square

object Square {
  def apply(string: String): Square = {
    case "X" => Rock
    case "." => Plain
    case "5" => RoverShadow
    case ">" => RoverEast
    case "<" => RoverWest
    case "^" => RoverNorth
    case "\u234C" => RoverSouth
    case _ => throw new Exception("unknown square")
  }

  def unapply(sq: Square): String = {
    case Rock => "X"
    case Plain => "."
    case RoverShadow => "5"
    case RoverEast => ">"
    case RoverWest => "<"
    case RoverNorth => "^"
    case RoverSouth => "\u234C"
  }
}

sealed trait Rover {
  def direction: Direction
}


case object Rock extends Square

case object Plain extends Square

case object RoverShadow extends Square

sealed trait Direction {
  def cost(newDirection: Direction): Int
}

case object South extends Direction {
  def cost(newDirection: Direction): Int = newDirection match {
    case South => 1
    case North => 3
    case East => 2
    case West => 2
  }
}

case object North extends Direction {
  def cost(newDirection: Direction): Int = newDirection match {
    case South => 3
    case North => 1
    case East => 2
    case West => 2
  }
}

case object East extends Direction {
  def cost(newDirection: Direction): Int = newDirection match {
    case South => 2
    case North => 2
    case East => 1
    case West => 3
  }
}

case object West extends Direction {
  def cost(newDirection: Direction): Int = newDirection match {
    case South => 2
    case North => 2
    case East => 3
    case West => 1
  }
}

case object RoverNorth extends Square with Rover {
  def direction: Direction = North
}

case object RoverEast extends Square with Rover {
  def direction: Direction = East
}

case object RoverWest extends Square with Rover {
  def direction: Direction = West
}

case object RoverSouth extends Square with Rover {
  def direction: Direction = South
}


type X = Int
type Y = Int

case class Coordinate(x: X, y: Y)

class Neighbors(
                 val north: Option[Coordinate],
                 val south: Option[Coordinate],
                 val east: Option[Coordinate],
                 val west: Option[Coordinate]
               ) {
  def asMap(): Map[Coordinate, Direction] = {
    Map.from(List(
      north match { case Some(c) => c -> North },
      south match { case Some(c) => c -> South },
      east match { case Some(c) => c -> East },
      west match { case Some(c) => c -> West }
    ))
  }


  class Field(val field: Vector[Vector[Square]]) {

    lazy val ySize: Int = field.size

    lazy val xSize: Int = field(0).size

    def draw(): Unit = {
      field.foreach(
        _.foreach(e => println(e))
      )
    }

    def move(from: Coordinate, to: Coordinate, rover: Rover): Field = {
      field.updated(from.x, field(from.x).updated(from.y, Plain))
      field.updated(to.x, field(to.x).updated(to.y, rover))
      new Field(field)
    }


    def neighbors(coordinate: Coordinate): Neighbors = {
      val (x, y) = (coordinate.x, coordinate.y)
      val northY: Y = if (y == 0) ySize else y + 1
      val southY: Y = if (y == ySize) 0 else y - 1
      val eastX: X = if (y == 0) xSize else y + 1
      val westX: X = if (y == xSize) 0 else y - 1


      new Neighbors(if (field(x)(northY) == Rock) None else Some(Coordinate(x, northY)),
        if (field(x)(southY) == Rock) None else Some(Coordinate(x, southY)),
        if (field(eastX)(y) == Rock) None else Some(Coordinate(eastX, y)),
        if (field(westX)(y) == Rock) None else Some(Coordinate(westX, y)))
    }


    lazy val edges: Map[Coordinate, Neighbors] = (for {
      (row, y: Y) <- field.zipWithIndex
      (_, x: X) <- row.zipWithIndex
    } yield (Coordinate(x, y), neighbors(Coordinate(x, y)))).toMap


    def heuristic(squareCoordinate: Coordinate, destCoordinate: Coordinate): Eval[Double] = Eval.later {
      val (x, y) = (squareCoordinate.x, squareCoordinate.y)
      val (destX, destY) = (destCoordinate.x, destCoordinate.y)
      List(
        (x - destX, y - destY),
        (x - destX, y - destY + ySize),
        (x - destX, y - destY - ySize),
        (x - destX + xSize, y - destY),
        (x - destX - xSize, y - destY),
        (x - destX + xSize, y - destY - ySize),
        (x - destX - xSize, y - destY - ySize),
        (x - destX + xSize, y - destY + ySize),
        (x - destX - xSize, y - destY + ySize))
        .map { case (x, y) => math.sqrt(x * x + y * y) }
        .max
    }

    class CoordinateOrdered(
                             val coordinate: Coordinate,
                             val h: Double,
                             val gCost: Double
                           ) extends Ordered[CoordinateOrdered] {
      def fScore(): Double = h + gCost

      override def compare(that: CoordinateOrdered): Int = this.fScore() compare that.fScore()
    }

    object CoordinateOrdered {
      def apply(coordinate: Coordinate, origin: Direction, destinationCoordinate: Coordinate, rover: Rover) = new CoordinateOrdered(
        coordinate,
        heuristic(coordinate, destinationCoordinate).value,
        rover.direction.cost(origin)
      )
    }


    def reconstructPath(cameFrom: Map[Coordinate, Coordinate], current: Coordinate): List[Coordinate] = {
      @tailrec
      def iter(el: Coordinate, acc: List[Coordinate]): List[Coordinate] =
        cameFrom.get(el) match {
          case Some(newCurrent) => iter(newCurrent, newCurrent :: acc)
          case None => acc
        }

      iter(current, List[Coordinate]())
    }

    def astar(
               rover: Rover,
               startCoordinate: Coordinate,
               destinationCoordinate: Coordinate
             ): Option[List[Coordinate]] = {

      val openQueue: mutable.PriorityQueue[CoordinateOrdered] = PriorityQueue[CoordinateOrdered]


      val gScore: mutable.Map[Coordinate, Double] = mutable.Map(startCoordinate -> 0.toDouble)
      val fScore: mutable.Map[Coordinate, Double] = mutable.Map(startCoordinate -> 0.toDouble)


      val cameFrom = mutable.Map[Coordinate, Coordinate]()

      while (openQueue.nonEmpty) {
        val current = openQueue.dequeue()
        val coordinate = current.coordinate
        if (coordinate == destinationCoordinate) {
          return Some(reconstructPath(cameFrom.toMap, coordinate))
        }
        val nbr = neighbors(coordinate).asMap()
        nbr.keys.foreach { el =>
          val co = CoordinateOrdered(el, nbr(el), destinationCoordinate, rover)
          val tentative_gScore = co.gCost
          if (tentative_gScore < gScore(co.coordinate)) {
            cameFrom(el) = coordinate
            gScore(el) = tentative_gScore
            fScore(el) = gScore(el) + co.h
            if (openQueue.toQueue.contains(co)) {
              openQueue.enqueue(co)
            }
          }

        }
      }
      None
    }


  }

  object Field {
    def apply(field: Vector[Vector[Square]]): Field = {
      val d = new Field(field)
      d.draw()
      d
    }


    def fromFile(path: String): Try[Field] =
      Using(Source.fromFile(path)) {
        _.getLines()
          .map(_.split("").map(Square.apply).toVector)
          .toVector
      }.map(Field.apply)
  }

  sealed trait Command {
    override def toString: String = {
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

  object RoverService {
    def initalPosition(field: Field): RoverService = (for {
      (row, y) <- field.field.zipWithIndex
      (square, x) <- row.zipWithIndex
      if square.isInstanceOf[Rover]
    } yield new RoverService(square match {
      case RoverNorth => RoverNorth
      case RoverEast => RoverEast
      case RoverWest => RoverWest
      case RoverSouth => RoverSouth
    }, Coordinate(x, y), field, List())).take(1).head
  }


  class RoverService(rover: Rover, rc: Coordinate, field: Field, command: List[Command]) {

    def moveNorth(): Try[RoverService] =
      rover match {
        case RoverNorth =>
          sendCommand(Forward)
        case RoverEast =>
          sendCommand(CounterClockRotate)
          sendCommand(Forward)
        case RoverWest =>
          sendCommand(ClockRotate)
          sendCommand(Forward)
        case RoverSouth =>
          sendCommand(ClockRotate)
          sendCommand(ClockRotate)
          sendCommand(Forward)
      }

    def moveSouth(): Try[RoverService] =
      rover match {
        case RoverNorth =>
          sendCommand(ClockRotate)
          sendCommand(ClockRotate)
          sendCommand(Forward)
        case RoverEast =>
          sendCommand(ClockRotate)
          sendCommand(Forward)
        case RoverWest =>
          sendCommand(CounterClockRotate)
          sendCommand(Forward)
        case RoverSouth =>
          sendCommand(Forward)
      }

    def moveEast(): Try[RoverService] =
      rover match {
        case RoverNorth =>
          sendCommand(ClockRotate)
          sendCommand(Forward)
        case RoverEast =>
          sendCommand(Forward)
        case RoverWest =>
          sendCommand(ClockRotate)
          sendCommand(ClockRotate)
          sendCommand(Forward)
        case RoverSouth =>
          sendCommand(CounterClockRotate)
          sendCommand(Forward)
      }

    def moveWest(): Try[RoverService] =
      rover match {
        case RoverNorth =>
          sendCommand(CounterClockRotate)
          sendCommand(Forward)
        case RoverEast =>
          sendCommand(ClockRotate)
          sendCommand(ClockRotate)
          sendCommand(Forward)
        case RoverWest =>
          sendCommand(Forward)
        case RoverSouth =>
          sendCommand(ClockRotate)
          sendCommand(Forward)
      }


    def sendCommand(cmd: Command): Try[RoverService] = Try(cmd match {
      case GoTo(coordinate) =>
        field.astar(rover, rc, coordinate)
      case Forward => rover match {
        case RoverNorth => field.edges(rc).north match {
          case Some(coordinate) => (rover, coordinate)
          case None => throw new Exception("Emergency stop")
        }
        case RoverEast => field.edges(rc).east match {
          case Some(coordinate) => (rover, coordinate)
          case None => throw new Exception("Emergency stop")
        }
        case RoverWest => field.edges(rc).west match {
          case Some(coordinate) => (rover, coordinate)
          case None => throw new Exception("Emergency stop")
        }
        case RoverSouth => field.edges(rc).south match {
          case Some(coordinate) => (rover, coordinate)
          case None => throw new Exception("Emergency stop")
        }
      }
      case ClockRotate => rover match {
        case RoverNorth => (RoverEast, rc)
        case RoverEast => (RoverSouth, rc)
        case RoverSouth => (RoverWest, rc)
        case RoverWest => (RoverNorth, rc)
      }
      case ClockRotate => rover match {
        case RoverNorth => (RoverWest, rc)
        case RoverEast => (RoverSouth, rc)
        case RoverSouth => (RoverEast, rc)
        case RoverWest => (RoverNorth, rc)
      }
    }).map(roverCoordinatesTuple => {
      field.move(rc, roverCoordinatesTuple._2, roverCoordinatesTuple._1)
      new RoverService(roverCoordinatesTuple._1, roverCoordinatesTuple._2, field, command :: cmd)
    })
  }
}


class App extends App {
  def
}

