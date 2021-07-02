package ulti.backend

import gbge.backend.{OK, Success}
import ulti.backend.InnerUlti
import ulti.shared._
import ulti.shared.abstract0._

object InnerUltiRouteGenerator {

  val u1 = InnerUlti.generate(UltiPlayer1, 1L)

  val route1: List[(UltiAction, UltiPlayer)] = List(
    (SubmitBid(BidSimple, u1.playerHands(UltiPlayer1).slice(0,2)), UltiPlayer1),
    (PickUpTalon, UltiPlayer2),
    (SubmitBid(Red(BidSimple), u1.playerHands(UltiPlayer1).slice(0,2)), UltiPlayer2),
    (SkipBid, UltiPlayer3),
    (SkipBid, UltiPlayer1),
    (SkipBid, UltiPlayer2),
  )

  val route2: List[(UltiAction, UltiPlayer)] = List(
    (SubmitBid(BidSimple, u1.playerHands(UltiPlayer1).slice(0,2)), UltiPlayer1),
    (PickUpTalon, UltiPlayer2),
    (SubmitBid(Red(BidSimple), u1.playerHands(UltiPlayer1).slice(0,2)), UltiPlayer2),
    (SkipBid, UltiPlayer3),
    (SkipBid, UltiPlayer1),
    (PickUpTalon, UltiPlayer2),
    (SubmitBid(BidUlti, u1.playerHands(UltiPlayer1).slice(0,2)), UltiPlayer2),
    (SkipBid, UltiPlayer3),
    (SkipBid, UltiPlayer1),
    (SkipBid, UltiPlayer2),
    (DeclareTrumpSuit(Acorns), UltiPlayer2)
  )

  val gameOverRoute = route2 ++ List(
    (PlayCard(Card(Bells, VAce)), UltiPlayer2),
    (PlayCard(Card(Bells, V9)), UltiPlayer3),
    (PlayCard(Card(Bells, V7)), UltiPlayer1),

    (PlayCard(Card(Hearts, VOver)), UltiPlayer2),
    (PlayCard(Card(Hearts, VAce)), UltiPlayer3),
    (PlayCard(Card(Hearts, VKing)), UltiPlayer1),

    (PlayCard(Card(Bells, VOver)), UltiPlayer3),
    (PlayCard(Card(Bells, V10)), UltiPlayer1),
    (PlayCard(Card(Bells, VUnder)), UltiPlayer2),

    (PlayCard(Card(Bells, VKing)), UltiPlayer1),
    (PlayCard(Card(Acorns, V8)), UltiPlayer2),
    (PlayCard(Card(Acorns, VAce)), UltiPlayer3),

    (PlayCard(Card(Hearts, VUnder)), UltiPlayer3),
    (PlayCard(Card(Acorns, V9)), UltiPlayer1),
    (PlayCard(Card(Hearts, V10)), UltiPlayer2),

    (PlayCard(Card(Bells, V8)), UltiPlayer1),
    (PlayCard(Card(Acorns, VOver)), UltiPlayer2),
    (PlayCard(Card(Acorns, VKing)), UltiPlayer3),

    (PlayCard(Card(Leaves, V9)), UltiPlayer3),
    (PlayCard(Card(Leaves, VOver)), UltiPlayer1),
    (PlayCard(Card(Leaves, VKing)), UltiPlayer2),

    (PlayCard(Card(Hearts, V8)), UltiPlayer2),
    (PlayCard(Card(Acorns, V10)), UltiPlayer3),
    (PlayCard(Card(Acorns, VUnder)), UltiPlayer1),

    (PlayCard(Card(Leaves, V10)), UltiPlayer3),
    (PlayCard(Card(Leaves, V7)), UltiPlayer1),
    (PlayCard(Card(Leaves, V8)), UltiPlayer2),

    (PlayCard(Card(Acorns, V7)), UltiPlayer3),
    (PlayCard(Card(Leaves, VUnder)), UltiPlayer1),
    (PlayCard(Card(Hearts, V9)), UltiPlayer2),

  )

  def step(number: Int): InnerUlti = {
    route1.slice(0, number).foldLeft(u1)((iu, pair) => {
      val temp = iu.reduce(pair._1, pair._2)
      assert(temp._2.isInstanceOf[Success])
      temp._1
    })
  }

  def step2(number: Int): InnerUlti = {
    route2.slice(0, number).foldLeft(u1)((iu, pair) => {
      val temp = iu.reduce(pair._1, pair._2)
      assert(temp._2.isInstanceOf[Success])
      temp._1
    })
  }

  def step3(number: Int): InnerUlti = {
    gameOverRoute.slice(0, number).foldLeft(u1)((iu, pair) => {
      val temp = iu.reduce(pair._1, pair._2)
      assert(temp._2.isInstanceOf[Success])
      temp._1
    })
  }

  val gameOverState: InnerUlti = gameOverRoute.foldLeft(u1)((iu, pair) => {
    val temp = iu.reduce(pair._1, pair._2)
    assert(temp._2.isInstanceOf[Success])
    temp._1
  })
}
