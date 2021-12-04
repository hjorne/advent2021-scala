type Board = Vector[Vector[Int]]

@main
def day4 =
  val (values, boards)      = parse4
  val winningBoardsTimeline = getWinningBoardsTimeline(values, boards)
  val sortedWinningIndices  = winningBoardsTimeline.keys.toVector.sorted
  val firstBoard      = winningBoardsTimeline(sortedWinningIndices.head).head
  val lastBoard       = winningBoardsTimeline(sortedWinningIndices.last).head
  val valuesWhenFirst = values.take(sortedWinningIndices.head)
  val valuesWhenLast  = values.take(sortedWinningIndices.last)
  val firstBoardScore = scoreBoard(valuesWhenFirst, firstBoard)
  val lastBoardScore  = scoreBoard(valuesWhenLast, lastBoard)

  assert(firstBoardScore == 46920)
  assert(lastBoardScore == 12635)

def getWinningBoardsTimeline(
    values: Vector[Int],
    boards: Vector[Board]
): Map[Int, Set[Board]] =
  values.indices.foldLeft(Map.empty) { case (winningBoardsTimeline, i) ⇒
    val calledValues        = values.take(i).toSet
    val previouslyWonBoards = winningBoardsTimeline.values.flatten.toSet
    val newlyWonBoards = boards.filter { board ⇒
      isWinningBoard(calledValues, board) &&
      !previouslyWonBoards.contains(board)
    }.toSet
    if newlyWonBoards.nonEmpty then winningBoardsTimeline + (i → newlyWonBoards)
    else winningBoardsTimeline
  }

def isWinningBoard(calledValues: Set[Int], board: Board): Boolean =
  board.exists(_.forall(calledValues.contains)) ||
    board.transpose.exists(_.forall(calledValues.contains))

def scoreBoard(calledValues: Vector[Int], board: Board): Int =
  val calledValuesSet = calledValues.toSet
  board.flatten.filterNot(calledValuesSet.contains).sum * calledValues.last

def parse4: (Vector[Int], Vector[Board]) =
  val input  = getInputString(4).split("\n\n").toList
  val values = input.head.split(",").toVector.map(_.toInt)

  val numRegex = raw"(\d+)".r
  val boards = input.tail.map {
    numRegex.findAllIn(_).map(_.toInt).toVector.grouped(5).toVector
  }.toVector

  (values, boards)
