enum Dir:
  case Forward(len: Int)
  case Down(len: Int)
  case Up(len: Int)

@main
def day2 =
  val input                 = parse(getInput(2))
  val (horizontal1, depth1) = navigateWithoutAim(input)
  val (horizontal2, depth2) = navigateWithAim(input)

  assert(horizontal1 * depth1 == 1728414)
  assert(horizontal2 * depth2 == 1765720035)

def navigateWithoutAim(input: Vector[Dir]): (Int, Int) =
  input.foldLeft((0, 0)) { case ((horizontal, depth), dir) ⇒
    dir match
      case Dir.Forward(len) ⇒ (horizontal + len, depth)
      case Dir.Down(len)    ⇒ (horizontal, depth + len)
      case Dir.Up(len)      ⇒ (horizontal, depth - len)
  }

def navigateWithAim(input: Vector[Dir]): (Int, Int) =
  val (horizontal, depth, _) = input.foldLeft((0, 0, 0)) {
    case ((horizontal, depth, aim), dir) ⇒
      dir match
        case Dir.Forward(len) ⇒ (horizontal + len, depth + aim * len, aim)
        case Dir.Down(len)    ⇒ (horizontal, depth, aim + len)
        case Dir.Up(len)      ⇒ (horizontal, depth, aim - len)
  }
  (horizontal, depth)

def parse(lines: Vector[String]): Vector[Dir] =
  val forward = raw"forward (\d+)".r
  val down    = raw"down (\d+)".r
  val up      = raw"up (\d+)".r
  lines.map {
    _ match
      case forward(len) ⇒ Dir.Forward(len.toInt)
      case down(len)    ⇒ Dir.Down(len.toInt)
      case up(len)      ⇒ Dir.Up(len.toInt)
  }
