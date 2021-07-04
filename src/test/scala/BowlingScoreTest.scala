import org.scalatest._

class BowlingScoreTest extends FlatSpec {
  val listOfFrames: List[(Int, Int)] = List(
    (6, 3), (8, 2),
    (10, 0), (10, 0),
    (7, 2), (0, 0),
    (4, 1), (10, 0),
    (8, 1), (10, 0),
    (5, 5), (0, 7)
  )

  val listOfBestFrames: List[(Int, Int)] = List(
    (10, 0), (10, 0),
    (10, 0), (10, 0),
    (10, 0), (10, 0),
    (10, 0), (10, 0),
    (10, 0), (10, 0),
    (10, 0), (10, 0)
  )

  "a simple frame" should "be the sum of both throws" in {
    val res = BowlingScore.calculateScoreFrame((6, 3))
    assert(9 == res)
  }

  "A square" should "be when the sum of throws are equals to 10" in {
    val res = BowlingScore.isSquare((8, 2))
    assert(res == true)
    val res2 = BowlingScore.isSquare((7, 1))
    assert(res2 == false)
  }

  it should "not be if the first throw is 10" in {
    val res = BowlingScore.isSquare((10, 0))
    assert(res == false)
  }

  "A strike" should "be when the first throw is 10" in {
    val res = BowlingScore.isStrike((10, 0))
    assert(res == true)
    val res2 = BowlingScore.isStrike((1, 9))
    assert(res2 == false)
    val res3 = BowlingScore.isStrike((1, 0))
    assert(res3 == false)
  }

  "A square score" should "be the sum of the current frame more the next one" in {
    val res = BowlingScore.calculateScoreSquare((7, 3), (3, 6))
    assert(res == 13)
  }

  "A strike score" should "be the sum of the current frame more the two next one" in {
    val res = BowlingScore.calculateScoreStrike((10, 0), (7, 3), (3,0))
    assert(res == 20)
    val res2 = BowlingScore.calculateScoreStrike((10, 0), (10, 0), (8,1))
    assert(res2 == 28)
  }

  "The sum of all score" should "be the sum of all kind of frame score" in {
    val res = BowlingScore.calculateAllScoreFrame(listOfFrames)
    assert (res == 137)
  }

  it should "be 300 in the best case" in {
    val res = BowlingScore.calculateAllScoreFrame(listOfBestFrames)
    assert (res == 300)
  }
}

// other way to do some unit test
class BowlingScoreTestFunSuite extends FunSuite{
  val listOfFrames: List[(Int,Int,Int)] = List(
    (1, 6, 3), (2, 8, 2),
    (3, 10, 0), (4, 10, 0),
    (5, 7, 2), (6, 0, 0),
    (7, 4, 1), (8, 10, 0),
    (9, 8, 1), (10, 10, 0),
    (11, 5, 5), (12, 0, 7),
  )

  test("a simple frame should be the sum of both throws") {
    val res = BowlingScore.calculateScoreFrame((6,3))
    assert(9 == res)
  }
}
