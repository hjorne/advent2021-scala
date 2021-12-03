import Integer.parseInt
@main
def day3 =
  val input            = getInput(3).map(_.toVector.map(_.asDigit))
  val (gamma, epsilon) = findGammaEpsilon(input)
  val oxygen           = findOxygenCO2(input, (zeros, ones) ⇒ zeros > ones)
  val CO2              = findOxygenCO2(input, (zeros, ones) ⇒ zeros <= ones)

  assert(gamma * epsilon == 4139586)
  assert(oxygen * CO2 == 1800151)

def findMax(nums: Vector[Int]): (Int, Int) =
  nums.foldLeft((0, 0)) {
    case ((zeros, ones), 0) ⇒ (zeros + 1, ones)
    case ((zeros, ones), 1) ⇒ (zeros, ones + 1)
  }

def findGammaEpsilon(input: Vector[Vector[Int]]): (Int, Int) =
  val (gammaStr, epsilonStr) =
    input.transpose
      .map(findMax)
      .foldLeft(("", "")) { case ((gamma, epsilon), (zeros, ones)) ⇒
        if ones > zeros then (gamma + '1', epsilon + '0')
        else (gamma + '0', epsilon + '1')
      }
  (parseInt(gammaStr, 2), parseInt(epsilonStr, 2))

def findOxygenCO2(
    input: Vector[Vector[Int]],
    filter: (Int, Int) ⇒ Boolean
): Int =
  val remaining = input.transpose.foldLeft(input.indices.toSet) {
    case (rem, _) if rem.size == 1 ⇒ rem
    case (rem, nums) ⇒
      val (zeros, ones) =
        findMax {
          nums.zipWithIndex
            .filter { case (_, i) ⇒ rem.contains(i) }
            .map(_._1)
        }
      val cut = nums.zipWithIndex.collect {
        case (0, j) if !filter(zeros, ones) ⇒ j
        case (1, j) if filter(zeros, ones)  ⇒ j
      }
      rem -- cut
  }
  parseInt(input(remaining.head).mkString, 2)
