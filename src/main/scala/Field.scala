import cats._

import scala.annotation.tailrec
import scala.io.Source
import util.{Failure, Success, Try, Using}
import scala.collection.immutable._
import java.net.URL
import scala.util.matching.Regex

object coordinate {
  // Base types for the field coordinates
  type X = Int
  type Y = Int

  case class Coordinate(x: X, y: Y)

  trait WithCoordinate {
    def coordinate: Coordinate
  }
}

object roverCommands {
  // Rover Commands

  //goto requires coordinates

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
  // All unique squares that can exist in the field

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
  // 4 direction rover could be facing

  import roverCommands._

  sealed trait Direction {
    // Cost of moving, used as part of G in A* calculation. Each command has equal cost.
    def cost(newDirection: Direction): Int = moveInstructions(newDirection).size

    // List of instruction required to one square in newDirection
    def moveInstructions(newDirection: Direction): List[Command]
  }

  trait WithDirection {
    def direction: Direction
  }

  case object South extends Direction {

    def moveInstructions(newDirection: Direction): List[Command] = newDirection match {
      case North => List(ClockRotate, ClockRotate, Forward)
      case East => List(CounterClockRotate, Forward)
      case West => List(ClockRotate, Forward)
      case South => List(Forward)
    }
  }

  case object North extends Direction {

    def moveInstructions(newDirection: Direction): List[Command] = newDirection match {
      case North => List(Forward)
      case East => List(ClockRotate, Forward)
      case West => List(CounterClockRotate, Forward)
      case South => List(ClockRotate, ClockRotate, Forward)
    }
  }

  case object East extends Direction {

    def moveInstructions(newDirection: Direction): List[Command] = newDirection match {
      case North => List(CounterClockRotate, Forward)
      case East => List(Forward)
      case West => List(ClockRotate, ClockRotate, Forward)
      case South => List(ClockRotate, Forward)
    }
  }

  case object West extends Direction {

    def moveInstructions(newDirection: Direction): List[Command] = newDirection match {
      case North => List(ClockRotate, Forward)
      case East => List(ClockRotate, ClockRotate, Forward)
      case West => List(Forward)
      case South => List(CounterClockRotate, Forward)
    }
  }


}

import squares._
import direction._
import coordinate._
import roverCommands._

/**
 *
 * @param coordinate - current coordinates
 * @param direction - where rover is facing
 *
 * art
 */
case class RoverState(coordinate: Coordinate, direction: Direction) {
  lazy val art: SquareArt = direction match {
    case South => RoverSouth
    case North => RoverNorth
    case East => RoverEast
    case West => RoverWest
  }
}

object drawer {
  // Helper class to render the field state in console
  implicit class Drawer(f: Field) {
    def draw(): Unit = {
      f.field.foreach(row =>
        println(row.fold("")((ac, el) => ac + el.toString))
      )
    }

    println("")
  }
}

/** Holds the current off all squares. Has to be rectangular in shape.
 *
 *  @param field - tensor with the squares
 *
 **/
case class Field(field: Vector[Vector[SquareArt]]) {

  private lazy val yMaxOffset: Int = field.size - 1

  private  lazy val xMaxOffset: Int = field(0).size - 1

  /** Square neighbors commute destination
   *
   * @param north - Going north will move rover to coordinate, None is for not commutable
   * @param south - Going south will move rover to coordinate, None is for not commutable
   * @param east - Going east will move rover to coordinate, None is for not commutable
   * @param west - Going west will move rover to coordinate, None is for not commutable
   */

  private final case class Neighbors(
                              north: Option[Coordinate],
                              south: Option[Coordinate],
                              east: Option[Coordinate],
                              west: Option[Coordinate]
                            ) {
    /** Used by A*
     * @return - map from coordinates to their origin direction
     */
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
          case Some(c) => Set(c -> West)
          case None => Set.empty[(Coordinate, Direction)]
        })
      )
    }
  }

  private object Neighbors {
    /** Factory for making neighbor nodes.
     *  Teleportation is handled here.
     *
     * @param coordinate - commute origin
     * @return neighbor nodes
     */
    def apply(coordinate: Coordinate): Neighbors = {
      val (x, y) = (coordinate.x, coordinate.y)
      val northY: Y = if (y == 0) yMaxOffset else y - 1
      val southY: Y = if (y == yMaxOffset) 0 else y + 1
      val westX: X = if (x == 0) xMaxOffset else x - 1
      val eastX: X = if (x == xMaxOffset) 0 else x + 1


      Neighbors(if (field(northY)(x) == Rock) None else Some(Coordinate(x, northY)),
        if (field(southY)(x) == Rock) None else Some(Coordinate(x, southY)),
        if (field(y)(eastX) == Rock) None else Some(Coordinate(eastX, y)),
        if (field(y)(westX) == Rock) None else Some(Coordinate(westX, y)))
    }
  }

  /** Changes field state by moving the rover
   *  Leaves trail where rover was.
   *
   * @param from initial rover state
   * @param to destination rover state
   * @throws Exception if `to` is a [[Rock]]
   * @return new filed wrapped in Try.
   */
  def move(from: RoverState, to: RoverState): Try[Field] = Try {
    field(to.coordinate.y)(to.coordinate.x) match {
      case Rock => throw new Exception(s"Emergency stop to avoid rock collision at ${to.coordinate.x},${to.coordinate.y}")
      case _ =>
        val f: Vector[Vector[SquareArt]] = field.updated(from.coordinate.y, field(from.coordinate.y).updated(from.coordinate.x, RoverTrace))
        val out: Vector[Vector[SquareArt]] = f.updated(to.coordinate.y, f(to.coordinate.y).updated(to.coordinate.x, to.art))
        Field(out)
    }
  }

  /** Commute graph from [[Coordinate]] to its' [[Neighbors]]
   */
  lazy val edges: Map[Coordinate, Neighbors] = (for {
    (row, y: Y) <- field.zipWithIndex
    (_, x: X) <- row.zipWithIndex
  } yield (Coordinate(x, y), Neighbors(Coordinate(x, y)))).toMap

  /** h in A*
   * In addition to euclidean space, also calculates projections over 4 edges and 4 corners. Takes the minimum.
   * Memorized with [[Eval]]
   *
   * @param squareCoordinate origin square
   * @param destCoordinate end point in A*
   * @return
   */

  private def heuristic(squareCoordinate: Coordinate, destCoordinate: Coordinate): Eval[Double] = Eval.later {
    val (x, y) = (squareCoordinate.x, squareCoordinate.y)
    val (destX, destY) = (destCoordinate.x, destCoordinate.y)
    val h = List(
      (x - destX, y - destY),
      (x - destX, y - (yMaxOffset - destY)),
      (x - destX, (yMaxOffset - y) - destY),
      ((xMaxOffset - x) - destX, y - destY),
      (x - (xMaxOffset - destX), y - destY),
      ((xMaxOffset - x) - destX, (yMaxOffset - y) - destY),
      (x - (xMaxOffset - destX), (yMaxOffset - y) - destY),
      ((xMaxOffset - x) - destX, y - (yMaxOffset - destY)),
      (x - (xMaxOffset - destX), y - (yMaxOffset - destY))
    )
      .map { case (x, y) => math.sqrt(x * x + y * y) }
      .min
    h
  }

  /** A* cost machinery
   *  hCost - heuristics from node to destination
   *  gCost - cost of reaching this node
   *  fCost - sum of above, cheapest fCost nodes evaluated first. Hence ascending order via compare.
   */
  private trait WithAStarMetrics extends Ordered[WithAStarMetrics] {
    def hCost: Double

    def gCost: Double

    def fCost: Double = hCost + gCost

    override def compare(that: WithAStarMetrics): Int = that.fCost compare this.fCost
  }

  /** A* graph node
   *
   *  Avoids duplication in queue by using custom hashCode and equals based on direction and coordinate.
   *
   * @param coordinate - coordinate on field
   * @param direction - where rover is facing
   * @param hCost - heuristics from node to destination
   * @param gCost - cost of reaching this node
   */
  private case class AstarRoverState(coordinate: Coordinate, direction: Direction, hCost: Double, gCost: Double)
    extends WithDirection
      with WithCoordinate
      with WithAStarMetrics {

    override def equals(obj: Any): Boolean = obj match {
      case that: AstarRoverState => this.coordinate == that.coordinate & this.direction == that.direction
      case _ => false
    }

    override def hashCode(): Y = coordinate.hashCode() ^ direction.hashCode()

  }

  /** A* path collection. Unravels A* commute map starting from destination
   *
   * @param cameFrom full commute map that A* before it found the solution.
   * @param current Would be destination all the time, not that it is not just the coordinate but also direction.
   * @return List of [[Command]] to reach the destination
   */
  private def reconstructPath(cameFrom: Map[AstarRoverState, AstarRoverState], current: AstarRoverState): List[Command] = {
    @tailrec
    def iter(astarRoverState: AstarRoverState, acc: List[Command], acc2: List[Coordinate]): (List[Command], List[Coordinate]) =
      cameFrom.get(astarRoverState) match {
        case Some(oldRoverOnField) =>
          iter(
            oldRoverOnField,
            oldRoverOnField.direction.moveInstructions(astarRoverState.direction) ++ acc,
            astarRoverState.coordinate :: acc2
          )
        case None => (acc, acc2)
      }
    // List[Coordinate] is there for debug purposes
    val z = iter(current, List[Command](), List[Coordinate]())
    z._1
  }

  /** A* search algorithm [[https://en.wikipedia.org/wiki/A*_search_algorithm]]
   *
   * Beware of mutable structures. Possible improvement would be cats-effect Priority queue and parallel node search.
   *
   * @param roverState initial rover state, coordinate and direction it's facing
   * @param destinationCoordinate search destination
   * @return Some list of [[Command]] to reach the destination. Or None if destination is unreachable.
   */
  def astar(roverState: RoverState, destinationCoordinate: Coordinate): Option[List[Command]] = {

    import scala.collection.mutable

    val initRoverState = AstarRoverState(
      roverState.coordinate,
      roverState.direction,
      this.heuristic(roverState.coordinate, destinationCoordinate).value,
      0
    )

    val openQueue: mutable.PriorityQueue[AstarRoverState] = mutable.PriorityQueue[AstarRoverState](initRoverState)


    val gScore: mutable.Map[AstarRoverState, Double] = mutable.Map(initRoverState -> 0.toDouble)
    val fScore: mutable.Map[AstarRoverState, Double] = mutable.Map(
      initRoverState -> this.heuristic(roverState.coordinate, destinationCoordinate).value
    )


    val cameFrom = mutable.Map[AstarRoverState, AstarRoverState]()

    while (openQueue.nonEmpty) {
      val current = openQueue.dequeue()
      if (current.coordinate == destinationCoordinate) {
        return Some(reconstructPath(cameFrom.toMap, current))
      }
      val nbr = Neighbors(current.coordinate).asMap()
      val candidateNbrs = nbr.keys.toSet
      candidateNbrs.foreach { nbrCoordinate =>
        val nbrDirection = nbr(nbrCoordinate)
        val neighbor = AstarRoverState(
          nbrCoordinate,
          nbrDirection,
          this.heuristic(current.coordinate, destinationCoordinate).value,
          current.direction.cost(nbrDirection).toDouble + gScore.getOrElse(current, 0.toDouble)
        )
        // tentative_gScore is the distance from start to the neighbor through current
        val tentative_gScore = gScore(current) + neighbor.gCost
        if (tentative_gScore < gScore.getOrElse(neighbor, Double.PositiveInfinity)) {
          // This path to neighbor is better than any previous one. Record it!
          cameFrom(neighbor) = current
          gScore(neighbor) = tentative_gScore
          fScore(neighbor) = gScore(neighbor) + neighbor.hCost
          if (!openQueue.toQueue.contains(neighbor)) {
            openQueue.enqueue(neighbor)
          }
        }
      }
    }
    None
  }


}

object Field {
  /** Validates that field is rectangular in shape and bigger than 1 square.
   *
   * @param field squares tensor
   * @return Try with squares tensor or Exception
   */
  private def validateSize(field: Vector[Vector[SquareArt]]): Try[Vector[Vector[SquareArt]]] = {
    if (field.size > 1) {
      if (field.forall(_.size == field.head.size))
        Success(field)
      else
        Failure(new Exception("Fields rows vary in size"))
    } else {
      Failure(new Exception("field is too small"))
    }
  }

  /** Loads field from [[URL]]
   *
   * @param url project resource URL
   * @return squares tensor wrapped in try with validation exception
   */
  def fromUrl(url: URL): Try[Field] = {
    Using(Source.fromURL(url)) {
      _.getLines()
        .map(_.split("").map(SquareArt.apply).toVector)
        .toVector
    }
      .flatMap(validateSize)
      .map(Field.apply)
  }
}

/** Issues commands to rover updating the [[Field]].
 * Commands need to be chained, since [[Field.move]] will return new state after each command
 *  {{{
 *     roverService
 *     .singleCommand(CounterClockRotate)
 *     .singleCommand(CounterClockRotate)
 *     .singleCommand(Forward)
 *  }}}
 *
 * @param roverState currect rover state, coordinate and direction
 * @param field squares tensor
 */
case class RoverService(roverState: RoverState, field: Field) {
  def singleCommand(cmd: Command): RoverService = {
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

  @tailrec
  final def runManyCommands(rs: RoverService, cmdLeft: List[Command]): RoverService = cmdLeft match {
    case head :: cmds => runManyCommands(rs.singleCommand(head), cmds)
    case _ => rs
  }

  /**
   * @param cmd rover command
   * @return updated rover state and command execution
   */
  private def sendSimpleCommand(cmd: Command): Option[RoverState] = {
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


object RoverService {
  /** Makes [[RoverService]] from the [[Field]], by finding rover on it.
   *
   * @param field square tensor
   * @return Try wrapping [[RoverService]] if rover was found, otherwise exception
   */
  def initialPosition(field: Field): Option[RoverService] = (for {
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

