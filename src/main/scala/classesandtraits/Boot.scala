package classesandtraits

import classesandtraits.Boot.Cube.SideCount

object Helper {
  trait Pow[A] {
    def ^(exponent: A): A
  }

  implicit class PowDouble(input: Double) extends Pow[Double] {
    def ^(exponent: Double)  = {
      Math.pow(input, exponent)
    }
  }
}

object Boot extends App {
  import classesandtraits.Helper._
  /*
  Homework

   Add additional 2D shapes such as triangle and square.

   In addition to the 2D shapes classes, add also 3D shapes classes
   (origin, point, sphere, cube, cuboid, 3D triangle - you can add
   others if you think they are a good fit).

   Add method `area` to 2D shapes.

   Add methods `surfaceArea` and `volume` to 3D shapes.

   If some of the implementation involves advanced math, it is OK
   to skip it (leave unimplemented), the primary intent of this
   exercise is modelling using case classes and traits, and not math.
   */

  sealed trait Coordinates
  case class Coordinates2D(x: Double, y: Double) extends Coordinates
  case class Coordinates3D(x: Double, y: Double, z: Double) extends Coordinates

  sealed trait Located[C <: Coordinates] {
    def coords: Array[C]
  }

  sealed trait Bounded {
    def minCoords: Array[Coordinates]
    def maxCoords: Array[Coordinates]
  }

  sealed trait Shape[C <: Coordinates] extends Located[C] with Bounded

  sealed trait Shape2D extends Shape[Coordinates2D] {
    def area: Double
  }

  sealed trait Shapes3D extends Shape[Coordinates3D] {
    def surfaceArea: Double
    def volume: Double
  }

  sealed trait Movable[+T <: Shape[C], C <: Coordinates] {
    def move(coordinates: C): Shape[C]
  }

  final case class Point(coordinates: Coordinates2D) extends Shape[Coordinates2D] with Movable[Point, Coordinates2D] {
    override def move(moveTo: Coordinates2D): Shape[Coordinates2D] = Point(Coordinates2D(coordinates.x + moveTo.x, coordinates.y + moveTo.y))

    override def minCoords: Array[Coordinates] = Array(coordinates)

    override def maxCoords: Array[Coordinates] = Array(coordinates)

    override def coords: Array[Coordinates2D] = Array(coordinates)
  }

  final case class Circle(coordinates: Coordinates2D, radius: Double) extends Shape2D with Movable[Circle, Coordinates2D] {
    override def area: Double = math.Pi * radius * radius

    override def move(moveTo: Coordinates2D): Shape2D = Circle(Coordinates2D(coordinates.x + moveTo.x, coordinates.y + moveTo.y), radius)

    override def coords: Array[Coordinates2D] = Array(coordinates)

    override def minCoords: Array[Coordinates] = Array(coordinates)

    override def maxCoords: Array[Coordinates] = Array(coordinates)
  }

  final case class Rectangle(coordinates: Coordinates2D, sideA: Double, sideB: Double) extends Shape2D with Movable[Rectangle, Coordinates2D] {
    override def area: Double = sideA * sideB

    override def move(moveTo: Coordinates2D): Shape2D = Rectangle(
      Coordinates2D(coordinates.x + moveTo.x, coordinates.y  + moveTo.y),
      sideA,
      sideB
    )

    override def coords: Array[Coordinates2D] = {
      val firstPoint = coordinates
      val secondPoint = firstPoint.copy(x = coordinates.x + sideA)
      val thirdPoint = secondPoint.copy(y = secondPoint.y + sideB)
      val fourthPoint = firstPoint.copy(y = secondPoint.y + sideB)
      Array(firstPoint, secondPoint, thirdPoint, fourthPoint)
    }

    override def minCoords: Array[Coordinates] = {
      val minX = coords.map(coord => coord.x).min
      val minY = coords.map(coord => coord.y).min
      Array(minX, minY)

    }

    override def maxCoords: Array[Coordinates] = {
      val maxX = coords.map(coord => coord.x).max
      val maxY = coords.map(coord => coord.y).max
      Array(maxX, maxY)
    }
  }

  object Cube {
    val SideCount = 6
  }

  final case class Cube(coordinates: Coordinates3D, side: Double) extends Shapes3D with Movable[Rectangle, Coordinates3D] {

    override def surfaceArea: Double = SideCount * side ^ 2

    override def volume: Double = side ^ 3

    override def move(moveTo: Coordinates3D): Shape[Coordinates3D] = Cube(
      Coordinates3D(coordinates.x + moveTo.x, coordinates.y + moveTo.y, coordinates.z + moveTo.z),
      side
    )

    override def coords: Array[Coordinates3D] = {
      val firstPoint = coordinates
      val secondPoint = firstPoint.copy(x = coordinates.x + side)
      val thirdPoint = secondPoint.copy(y = secondPoint.y + side)
      val fourthPoint = firstPoint.copy(y = secondPoint.y + side)

      val fifthPoint = firstPoint.copy(z = firstPoint.z + side)
      val sixthPoint = secondPoint.copy(z = secondPoint.z + side)
      val seventhPoint = thirdPoint.copy(z = thirdPoint.z + side)
      val eightPoint = fourthPoint.copy(z = fourthPoint.z + side)
      Array(firstPoint, secondPoint, thirdPoint, fourthPoint, fifthPoint, sixthPoint, seventhPoint, eightPoint)

    }

    override def minCoords: Array[Coordinates] = {
      val minX = coords.map(coord => coord.x).min
      val minY = coords.map(coord => coord.y).min
      val minZ = coords.map(coord => coord.z).min
      Array(minX, minY, minZ)
    }

    override def maxCoords: Array[Coordinates] = {
      val maxX = coords.map(coord => coord.x).max
      val maxY = coords.map(coord => coord.y).max
      val maxZ = coords.map(coord => coord.z).max
      Array(maxX, maxY, maxZ)
    }
  }


}
