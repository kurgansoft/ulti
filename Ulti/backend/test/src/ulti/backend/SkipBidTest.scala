package ulti.backend

import org.scalatest.funsuite.AnyFunSuite
import ulti.shared.abstract0._

class SkipBidTest extends AnyFunSuite {
  test("good1") {
    val ulti = InnerUltiRouteGenerator.step(4)
    assert(ulti.currentPlayer == UltiPlayer1)
    assert(ulti.phase == BiddingPhase)
    val ulti2 = InnerUltiRouteGenerator.step(5)
    assert(ulti2.currentPlayer == UltiPlayer2)
    assert(ulti2.phase == BiddingPhase)
    val ulti3 = InnerUltiRouteGenerator.step(6)
    assert(ulti3.currentPlayer == UltiPlayer2)
    assert(ulti3.phase == PlayingPhase)
  }
}
