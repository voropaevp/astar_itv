import org.scalatest._
import flatspec._
import coordinate.Coordinate
import matchers._

trait RoverSpec {
  lazy val field2: Field = Field.fromUrl(getClass.getResource("/free_2x2_1x1.txt")).get

  lazy val roverService2: RoverService = RoverService.initialPosition(field2).get
  lazy val field2A22: Field = Field.fromUrl(getClass.getResource("/free_2x2_1x1_2x2.txt")).get

  lazy val field10: Field = Field.fromUrl(getClass.getResource("/rocks_10x10_2x2.txt")).get
  lazy val roverService10: RoverService = RoverService.initialPosition(field10).get

  lazy val field10A99: Field = Field.fromUrl(getClass.getResource("/rocks_10x10_2x2_9_9.txt")).get
  lazy val field10A66: Field = Field.fromUrl(getClass.getResource("/rocks_10x10_2x2_6_6.txt")).get

}

class RoverTest extends AnyFlatSpec with should.Matchers with RoverSpec {

  import squares._
  import direction._
  import roverCommands._

  "Field" should "read from file" in {
    field2.field should be(Vector(
      Vector(RoverEast, Plain),
      Vector(Plain, Plain)
    ))
  }

  "Rover" should "be at 0,0" in {
    roverService2.roverState.coordinate should be(Coordinate(0, 0))
  }

  "Rover" should "be facing East" in {
    roverService2.roverState.direction should be(East)
  }

  "Rover" should "go the 1,1" in {
    roverService2
      .flatMap(Forward)
      .flatMap(ClockRotate)
      .flatMap(Forward)
      .field.field should be(field2A22)
  }

  "Rover" should "go the 1,1 via portals" in {
    roverService2
      .flatMap(CounterClockRotate)
      .flatMap(CounterClockRotate)
      .flatMap(Forward)
      .flatMap(CounterClockRotate)
      .flatMap(Forward)
      .field.field should be(field2A22)
  }

  "Rover" should "not go over rocks" in {
    roverService10
      .flatMap(Forward)
      .field.field should be(field10.field)
  }

  "Rover" should "find best path over north portal" in {
    val f = roverService10
      .flatMap(GoTo(Coordinate(6, 6)))
      .field

    f.field should be(field10A66.field)
  }

  "Rover" should "find best path on over both north and west portal" in {
    val f = roverService10
      .flatMap(GoTo(Coordinate(9, 9)))
      .field

    f.field should be(field10A99.field)
  }

  "Rover" should "not find path to unreachable location" in {
    val f = roverService10
      .flatMap(GoTo(Coordinate(2, 7)))
      .field
    f.field should be(field10.field)
  }
}
