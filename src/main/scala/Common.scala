import scala.io.Source

def getInput(i: Int): Vector[String] =
  Source.fromFile(s"input/day$i.txt").getLines.toVector
