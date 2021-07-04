import scala.annotation.tailrec

object BowlingScore {

  def calculateScoreFrame(frame: (Int, Int)): Int = {
    frame._1 + frame._2
  }

  def isSquare(frame: (Int, Int)) : Boolean = {
    frame match {
      case frame if (10 == frame._1) => false
      case frame if (10 == frame._1 + frame._2) => true
      case _ => false
    }
  }

  def isStrike(frame: (Int, Int)) : Boolean = {
    frame._1 match {
      case 10 => true
      case _ => false
    }
  }

  def calculateScoreSquare(frame1: (Int, Int), frame2: (Int, Int)): Int =
  {
    frame1._1 + frame1._2 + frame2._1
  }

  def calculateScoreStrike(frame: (Int, Int), frame2: (Int, Int), frame3: (Int, Int)) : Int = {
    frame2._1 match {
      case 10 => frame._1 + frame._2 + frame2._1 + frame3._1
      case _ => frame._1 + frame._2 + frame2._1 + frame2._2
    }
  }

  def calculateAllScoreFrame(listOfFrames: List[(Int, Int)]) : Int  = {

    @tailrec
    def recCalculateScore(listOf10: List[(Int, Int)], listOfFrames: List[(Int, Int)], turn: Int, score: Int): Int = {
      listOf10 match {
        case Nil => score
        case h::t if isStrike(h) =>
            recCalculateScore(t, listOfFrames, turn+1, score + calculateScoreStrike(h, listOfFrames(turn+1), listOfFrames(turn+2)))
        case h::t if isSquare(h) =>
            recCalculateScore(t, listOfFrames, turn+1, score + calculateScoreSquare(h, listOfFrames(turn+1)))
        case h::t =>
            recCalculateScore(t, listOfFrames, turn+1, score + calculateScoreFrame(h))
      }
    }

    recCalculateScore(listOfFrames.slice(0,10), listOfFrames, 0, 0)
  }
}

