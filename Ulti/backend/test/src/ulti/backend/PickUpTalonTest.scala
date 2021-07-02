package ulti.backend

import gbge.backend.{GeneralFailure, OK}
import org.scalatest.funsuite.AnyFunSuite
import ulti.backend.InnerUlti
import ulti.shared._
import ulti.shared.abstract0._

class PickUpTalonTest extends AnyFunSuite {

  val u1 = InnerUlti.generate(UltiPlayer1, 1L)
  val u2 = u1.reduce(SubmitBid(
    BidSimple,
    u1.playerHands(UltiPlayer1).slice(0,2)
  ), UltiPlayer1)._1

  test("good") {
    val (u3, result) = u2.reduce(PickUpTalon, UltiPlayer2)

    assert(result == OK)
    assert(u3.playerHands(UltiPlayer2).size == 12)
    assert(u3.currentPlayer == UltiPlayer2)
  }

  test("only the currentPlayer can pick up the talon") {
    val (u3, result) = u2.reduce(PickUpTalon, UltiPlayer1)

    assert(result.isInstanceOf[GeneralFailure])
    assert(u3 == u2)
  }

  test("Can't pick up talon if it is not there") {
    val (u3, result) = u1.reduce(PickUpTalon, UltiPlayer1)
    assert(result.isInstanceOf[GeneralFailure])
    assert(u3 == u1)
  }
}
