package ulti.backend

import gbge.backend.GeneralFailure
import org.scalatest.funsuite.AnyFunSuite
import ulti.shared._
import ulti.shared.abstract0._

class DeclareMarriagesTest extends AnyFunSuite {
  val u = InnerUltiRouteGenerator.step2(11)
  val u2 = u.copy(playerHands =
    u.playerHandLen(UltiPlayer1).modify(_ - Card(Leaves, VOver) + Card(Hearts, V8))(u.playerHands))
    .copy(playerHands = u.playerHandLen(UltiPlayer2).modify(_ - Card(Hearts, V8) + Card(Leaves, VOver))(u.playerHands))

  assert(u2.declaredMarriages.keys.isEmpty)

  val (u3, result) = u2.reduce(DeclareMarriages(Set(Leaves)), UltiPlayer2)

  assert(u3.declaredMarriages(UltiPlayer2).size == 1)

  val (u4, result2) = u2.reduce(DeclareMarriages(Set(Hearts)), UltiPlayer2)
  assert(result2.isInstanceOf[GeneralFailure])
  assert(u4 == u2)

  test("good") {
    val sorted1 = u2.playerHands(UltiPlayer1).toList.sortWith((c1,c2) => {
      implicit val adu = Acorns
      if (c1.suit == c2.suit)
        c1.value < c2.value
      else
        c1.suit.toString < c2.suit.toString
    })

    val sorted2 = u2.playerHands(UltiPlayer2).toList.sortWith((c1,c2) => {
      implicit val adu = Acorns
      if (c1.suit == c2.suit)
        c1.value < c2.value
      else
        c1.suit.toString < c2.suit.toString
    })

    val sorted3 = u2.playerHands(UltiPlayer3).toList.sortWith((c1,c2) => {
      implicit val adu = Acorns
      if (c1.suit == c2.suit)
        c1.value < c2.value
      else
        c1.suit.toString < c2.suit.toString
    })

    println("player1:")
    for (card <- sorted1)
      println(card)

    println("--------")
    println("player2:")
    for (card <- sorted2)
      println(card)

    println("--------")
    println("player3:")
    for (card <- sorted3)
      println(card)
  }

}
