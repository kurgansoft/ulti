package risk.backend

import org.scalatest.funsuite.AnyFunSuite
import ulti.backend.InnerUlti
import ulti.shared.abstract0._

class InnerUltiGenerationTest extends AnyFunSuite {
  test("generation test") {
    val seeds = List(1L, 322L, 1024L, 5044L, 9433L, 654L, 7921L, 87654L, 93219432L, 10864837829L)

    for (seed <- seeds) {
      val iu = InnerUlti.generate(UltiPlayer1, seed)
      iu.invariant()
      assert(iu.playerHands(UltiPlayer1).size == 12)
      assert(iu.playerHands(UltiPlayer2).size == 10)
      assert(iu.playerHands(UltiPlayer3).size == 10)
      assert(iu.round == 1)
      assert(!iu.talonOnTheTable)

      assert(iu.currentPlayer == UltiPlayer1)
    }

    for (seed <- seeds) {
      val iu = InnerUlti.generate(UltiPlayer2, seed)
      iu.invariant()
      assert(iu.playerHands(UltiPlayer1).size == 10)
      assert(iu.playerHands(UltiPlayer2).size == 12)
      assert(iu.playerHands(UltiPlayer3).size == 10)
      assert(!iu.talonOnTheTable)
      assert(iu.round == 1)

      assert(iu.currentPlayer == UltiPlayer2)
    }

    for (seed <- seeds) {
      val iu = InnerUlti.generate(UltiPlayer3, seed)
      iu.invariant()
      assert(iu.playerHands(UltiPlayer1).size == 10)
      assert(iu.playerHands(UltiPlayer2).size == 10)
      assert(iu.playerHands(UltiPlayer3).size == 12)
      assert(!iu.talonOnTheTable)
      assert(iu.round == 1)

      assert(iu.currentPlayer == UltiPlayer3)
    }
  }
}
