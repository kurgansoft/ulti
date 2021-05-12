package risk.backend

import gbge.backend.OK
import org.scalatest.funsuite.AnyFunSuite
import ulti.backend.InnerUlti
import ulti.shared.{BidSimple, PickUpTalon, Red, SubmitBid}
import ulti.shared.abstract0._

class SubmitBidTest extends AnyFunSuite {

  val u1 = InnerUlti.generate(UltiPlayer1, 1L)

  test("good") {
    val (u2, result) = u1.reduce(SubmitBid(
      BidSimple,
      u1.playerHands(UltiPlayer1).slice(0,2)
    ), UltiPlayer1)
    assert(result == OK)
    assert(u2.talonOnTheTable)
    assert(u2.currentBid.contains(BidSimple))
    assert(u2.currentPlayer == UltiPlayer2)
    assert(u2.playerHands(UltiPlayer1).size == 10)
    assert(u2.talon.size == 2)
    assert(u2.currentBidWinner.contains(UltiPlayer1))
  }

  test("good2") {
    val (u2, _) = u1.reduce(SubmitBid(
      BidSimple,
      u1.playerHands(UltiPlayer1).slice(0,2)
    ), UltiPlayer1)
    val (u3, _) = u2.reduce(PickUpTalon, UltiPlayer2)
    val (u4, result) = u3.reduce(SubmitBid(
      Red(BidSimple),
      u1.playerHands(UltiPlayer1).slice(0,2)
    ), UltiPlayer2)
    assert(result == OK)
    assert(u4.currentPlayer == UltiPlayer3)
  }

  test("wrong invoker") {
    val (u2, result) = u1.reduce(SubmitBid(
      BidSimple,
      u1.playerHands(UltiPlayer1).slice(0,2)
    ), UltiPlayer2)
    assert(u2 == u1)
  }

  test("incorrect talon") {
    val (u2, result) = u1.reduce(SubmitBid(
      BidSimple,
      u1.playerHands(UltiPlayer2).slice(0,2)
    ), UltiPlayer1)
    assert(u2 == u1)
  }

}
