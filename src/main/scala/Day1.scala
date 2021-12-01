@main
def day1 =
  val depths               = getInput(1).map(_.toInt)
  val numIncreased         = findNumIncreased(depths)
  val windowedDepths       = depths.sliding(3).map(_.sum).toVector
  val windowedNumIncreased = findNumIncreased(windowedDepths)

  assert(numIncreased == 1583)
  assert(windowedNumIncreased == 1627)

def findNumIncreased(depths: Vector[Int]): Int =
  depths.zip(depths.tail).count { case (first, second) ⇒ second > first }
