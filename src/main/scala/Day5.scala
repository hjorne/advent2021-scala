case class Point(x: Int, y: Int)
case class Line(r1: Point, r2: Point):
  def isStraight: Boolean = r1.x == r2.x || r1.y == r2.y

@main
def day5 =
  val lines              = parse5
  val overlappingPoints  = countOverlapping(lines.filter(_.isStraight))
  val overlappingPoints2 = countOverlapping(lines)
  assert(overlappingPoints == 6666)
  assert(overlappingPoints2 == 19081)

def countOverlapping(lines: Seq[Line]): Int =
  lines
    .flatMap(enumerate)
    .groupBy(identity)
    .values
    .map(_.length)
    .count(_ >= 2)

def enumerate(line: Line): Seq[Point] =
  val Line(Point(x1, y1), Point(x2, y2)) = line
  val dx                                 = (x2 - x1).sign
  val dy                                 = (y2 - y1).sign
  val xs = LazyList.iterate(x1)(_ + dx).takeWhile(_ != x2 + dx).toVector
  val ys = LazyList.iterate(y1)(_ + dy).takeWhile(_ != y2 + dy).toVector
  if xs.isEmpty then ys.map(Point(x1, _))
  else if ys.isEmpty then xs.map(Point(_, y1))
  else xs.zip(ys).map { case (x, y) ⇒ Point(x, y) }

def parse5: Seq[Line] =
  val line = raw"(\d+),(\d+) -> (\d+),(\d+)".r
  getInput(5).map {
    _ match
      case line(x1, y1, x2, y2) ⇒
        Line(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
  }
