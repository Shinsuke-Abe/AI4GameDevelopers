trait Coordinate {
  val x: Int
  val y: Int

  def nextCoordinate(target: Coordinate): Coordinate

  def nextValue(source: Int, target: Int): Int
}

case class Prey(x: Int, y: Int) extends Coordinate {
  override def nextCoordinate(target: Coordinate): Coordinate = {
    Prey(nextValue(this.x, target.x), nextValue(this.y, target.y))
  }

  override def nextValue(source: Int, target: Int) = {
    if(source > target) {
      source + 1
    } else if(source < target) {
      source - 1
    } else {
      // TODO 逃走時にイコールの場合は何か動いた方が良い
      source
    }
  }
}

case class Predator(x: Int, y: Int) extends Coordinate {
  override def nextCoordinate(target: Coordinate): Coordinate = {
    Predator(nextValue(this.x, target.x), nextValue(this.y, target.y))
  }

  override def nextValue(source: Int, target: Int) = {
    if(source > target) {
      source - 1
    } else if(source < target) {
      source + 1
    } else {
      source
    }
  }
}

val prey = Prey(10, 1)
val predator = Predator(9, 3)

val movedPredator = predator.nextCoordinate(prey)
prey.nextCoordinate(movedPredator)
