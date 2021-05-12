package risk.backend

import gbge.backend.{ExecuteEffect, Player}
import gbge.shared.IN_PROGRESS
import org.scalatest.funsuite.AnyFunSuite
import ulti.backend.{InnerUlti, Ulti}
import ulti.shared._
import ulti.shared.abstract0._

class ReadyForNextRoundTest extends AnyFunSuite {

  val generateInnerUltiInGameOverPhase: InnerUlti = {
    var iu = InnerUlti.generate(UltiPlayer1, 1L)
    iu = iu.reduce(SubmitBid(BidSimple, Set(Card(Hearts, V7), Card(Bells, V7))), UltiPlayer1)._1
    iu = iu.reduce(SkipBid, UltiPlayer2)._1
    iu = iu.reduce(SkipBid, UltiPlayer3)._1
    iu = iu.reduce(SkipBid, UltiPlayer1)._1
    iu = iu.reduce(DeclareTrumpSuit(Acorns), UltiPlayer1)._1

    assert(iu.phase == PlayingPhase)

    iu = iu.reduce(PlayCard(Card(Leaves, VAce)), UltiPlayer1)._1
    iu = iu.reduce(PlayCard(Card(Leaves, V8)), UltiPlayer2)._1
    iu = iu.reduce(PlayCard(Card(Leaves, V9)), UltiPlayer3)._1

    iu = iu.reduce(PlayCard(Card(Leaves, VOver)), UltiPlayer1)._1
    iu = iu.reduce(PlayCard(Card(Leaves, VKing)), UltiPlayer2)._1
    iu = iu.reduce(PlayCard(Card(Leaves, V10)), UltiPlayer3)._1

    iu = iu.reduce(PlayCard(Card(Hearts, VAce)), UltiPlayer3)._1
    iu = iu.reduce(PlayCard(Card(Hearts, VKing)), UltiPlayer1)._1
    iu = iu.reduce(PlayCard(Card(Hearts, V8)), UltiPlayer2)._1

    iu = iu.reduce(PlayCard(Card(Acorns, VAce)), UltiPlayer3)._1
    iu = iu.reduce(PlayCard(Card(Acorns, V9)), UltiPlayer1)._1
    iu = iu.reduce(PlayCard(Card(Acorns, V8)), UltiPlayer2)._1

    iu = iu.reduce(PlayCard(Card(Acorns, V10)), UltiPlayer3)._1
    iu = iu.reduce(PlayCard(Card(Acorns, VUnder)), UltiPlayer1)._1
    iu = iu.reduce(PlayCard(Card(Acorns, VOver)), UltiPlayer2)._1

    iu = iu.reduce(PlayCard(Card(Acorns, VKing)), UltiPlayer3)._1
    iu = iu.reduce(PlayCard(Card(Bells, V8)), UltiPlayer1)._1
    iu = iu.reduce(PlayCard(Card(Hearts, V9)), UltiPlayer2)._1

    iu = iu.reduce(PlayCard(Card(Acorns, V7)), UltiPlayer3)._1
    iu = iu.reduce(PlayCard(Card(Leaves, V7)), UltiPlayer1)._1
    iu = iu.reduce(PlayCard(Card(Bells, VUnder)), UltiPlayer2)._1

    iu = iu.reduce(PlayCard(Card(Bells, V9)), UltiPlayer3)._1
    iu = iu.reduce(PlayCard(Card(Bells, V10)), UltiPlayer1)._1
    iu = iu.reduce(PlayCard(Card(Bells, VAce)), UltiPlayer2)._1

    iu = iu.reduce(PlayCard(Card(Hearts, V10)), UltiPlayer2)._1
    iu = iu.reduce(PlayCard(Card(Hearts, VUnder)), UltiPlayer3)._1
    iu = iu.reduce(PlayCard(Card(Bells, VKing)), UltiPlayer1)._1

    iu = iu.reduce(PlayCard(Card(Hearts, VOver)), UltiPlayer2)._1
    iu = iu.reduce(PlayCard(Card(Bells, VOver)), UltiPlayer3)._1
    iu = iu.reduce(PlayCard(Card(Leaves, VUnder)), UltiPlayer1)._1
    iu
  }

  val ultiInGameOverPhase = Ulti(IN_PROGRESS, Some(generateInnerUltiInGameOverPhase))

  val player1 = Player(1, "Chuck Norris", "123", false, Some(1))
  val player2 = Player(2, "Bruce Lee", "456", false, Some(2))
  val player3 = Player(3, "Steven Seagull", "789", false, Some(3))

  test("if innerUlti is not in GameOverPhase; just fail") {
    val ulti = Ulti(IN_PROGRESS, Some(InnerUlti.generate(UltiPlayer1, 1L)))

    val players: List[Option[Player]] = List(
      None,
      Some(Player(1, "Chuck Norris", "123", isAdmin = false, role = None)),
      Some(Player(1, "Chuck Norris", "123", isAdmin = false, role = Some(1))),
      Some(Player(1, "Chuck Norris", "123", isAdmin = false, role = Some(2))),
      Some(Player(1, "Chuck Norris", "123", isAdmin = false, role = Some(3))),
      Some(Player(1, "Chuck Norris", "123", isAdmin = false, role = Some(167))),
    )

    for (player <- players) {
      val ulti2 = ulti.reduce(ReadyForNextRound, player)._1.asInstanceOf[Ulti]
      assert(ulti2 == ulti)
    }
  }

  test("if innerUlti is in GameOverPhase -> switch flip") {
    val res = ultiInGameOverPhase.reduce(ReadyForNextRound, Some(player1))
    val ulti2 = res._1.asInstanceOf[Ulti]
    assert(ulti2.readyForNextRound(UltiPlayer1))
    assert(!ulti2.readyForNextRound(UltiPlayer2))
    assert(!ulti2.readyForNextRound(UltiPlayer3))

    val res2 = ulti2.reduce(ReadyForNextRound, Some(player2))
    val ulti3 = res2._1.asInstanceOf[Ulti]
    assert(ulti3.readyForNextRound(UltiPlayer1))
    assert(ulti3.readyForNextRound(UltiPlayer2))
    assert(!ulti3.readyForNextRound(UltiPlayer3))
  }

  test("After all three switches are flipped in GameOverPhase -> transition to a new Biddgin Phase.") {
    var res = ultiInGameOverPhase.reduce(ReadyForNextRound, Some(player1))
    res = res._1.reduce(ReadyForNextRound, Some(player2))
    res = res._1.reduce(ReadyForNextRound, Some(player3))
    val ulti2 = res._1.asInstanceOf[Ulti]

    assert(ulti2.readyForNextRound.values.forall(_ == false))

    val effectToBeExecuted: ExecuteEffect = res._2.asInstanceOf[ExecuteEffect]
    assert(effectToBeExecuted.effect != null)

  }
}