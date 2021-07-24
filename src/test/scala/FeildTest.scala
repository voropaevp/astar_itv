import collection.mutable.Stack
import org.scalatest._
import flatspec._
import Field._
import coordinate.Coordinate
import matchers._

import scala.collection.immutable.Vector

class FieldSpec extends AnyFlatSpec with should.Matchers {

  import squares._
  import direction._
  import roverCommands._

  lazy val free_2_2_1_1: Vector[Vector[SquareArt]] = Vector(
    Vector(RoverEast, Plain),
    Vector(Plain, Plain)
  )

  lazy val free_2_2_2_2: Vector[Vector[SquareArt]] = Vector(
    Vector(RoverTrace, RoverTrace),
    Vector(Plain, RoverSouth)
  )

  lazy val field10 = Field.fromUrl(getClass.getResource("/rocks_10x10_2x2.txt")).get
  lazy val roverService10 = RoverService.initalPosition(field10).get

  lazy val field: Field = Field.fromUrl(getClass.getResource("/free_2x2_1x1.txt")).get

  lazy val roverService: RoverService = RoverService.initalPosition(field).get

  "Field" should "read from file" in {
    field.field should be(free_2_2_1_1)
  }

  "Rover" should "be at 0,0" in {
    roverService.roverState.coordinate should be(Coordinate(0, 0))
  }

  "Rover" should "be facing East" in {
    roverService.roverState.direction should be(East)
  }

  "Rover" should "go the 1,1" in {
    roverService
      .flatMap(Forward)
      .flatMap(ClockRotate)
      .flatMap(Forward)
      .field.field should be(free_2_2_2_2)
  }

  "Rover" should "go the 1,1 via portals" in {
    roverService
      .flatMap(CounterClockRotate)
      .flatMap(CounterClockRotate)
      .flatMap(Forward)
      .flatMap(CounterClockRotate)
      .flatMap(Forward)
      .field.field should be(Vector(
      Vector(RoverTrace, RoverTrace),
      Vector(Plain, RoverSouth)
    ))
  }

  "Rover" should "not go over rocks" in {
    roverService10
      .flatMap(Forward)
      .field.field should be(field10.field)
  }

  "Rover" should "find best path" in {
    roverService10
      .flatMap(GoTo(Coordinate(6,6)))
      .field.field should be(field10.field)
  }



}
