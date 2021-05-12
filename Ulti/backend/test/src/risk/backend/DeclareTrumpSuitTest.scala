package risk.backend

import gbge.backend.GeneralFailure
import org.scalatest.funsuite.AnyFunSuite
import ulti.shared.abstract0._
import ulti.shared.{Acorns, DeclareTrumpSuit}

class DeclareTrumpSuitTest extends AnyFunSuite {

  val u = InnerUltiRouteGenerator.step2(10)

  test("good") {
    val u2 = u.reduce(DeclareTrumpSuit(Acorns), UltiPlayer2)._1
    assert(u2.trumpSuit.contains(Acorns))
  }

  test("bad") {
    val (u2, result) = u.reduce(DeclareTrumpSuit(Acorns), UltiPlayer1)
    assert(result.isInstanceOf[GeneralFailure])
    println(result.asInstanceOf[GeneralFailure].message)
    assert(u2 == u)
  }
}