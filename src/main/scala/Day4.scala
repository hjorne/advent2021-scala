type Board = Seq[Seq[Int]]

@main
def day4 =
  val (values, boards)      = parse4
  val winningBoardsTimeline = getWinningBoardsTimeline(values, boards)
  val firstBoardScore       = winningBoardsTimeline.head
  val lastBoardScore        = winningBoardsTimeline.last

  assert(firstBoardScore == 46920)
  assert(lastBoardScore == 12635)

def getWinningBoardsTimeline(
    values: Seq[Int],
    boards: Seq[Board]
): Seq[Int] =
  values.indices
    .foldLeft(Seq.empty[(Int, Board)]) { case (winningBoardsTimeline, i) ⇒
      val previouslyWonBoards = winningBoardsTimeline.map(_._2).toSet
      val newlyWonBoards = boards
        .filter { board ⇒
          isWinningBoard(values.take(i).toSet, board) &&
          !previouslyWonBoards.contains(board)
        }
        .map(board ⇒ (scoreBoard(values.take(i), board), board))
        .toSet
      if newlyWonBoards.nonEmpty then winningBoardsTimeline ++ newlyWonBoards
      else winningBoardsTimeline
    }
    .map(_._1)

def isWinningBoard(calledValues: Set[Int], board: Board): Boolean =
  board.exists(_.forall(calledValues.contains)) ||
    board.transpose.exists(_.forall(calledValues.contains))

def scoreBoard(calledValues: Seq[Int], board: Board): Int =
  val calledValuesSet = calledValues.toSet
  board.flatten.filterNot(calledValuesSet.contains).sum * calledValues.last

def parse4: (Seq[Int], Seq[Board]) =
  val input  = getInputString(4).split("\n\n").toList
  val values = input.head.split(",").toVector.map(_.toInt)

  val numRegex = raw"(\d+)".r
  val boards = input.tail.map {
    numRegex.findAllIn(_).map(_.toInt).toVector.grouped(5).toVector
  }.toVector

  (values, boards)
