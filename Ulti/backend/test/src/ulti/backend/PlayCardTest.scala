package ulti.backend

import gbge.backend.{Failure, GeneralFailure, OK}
import org.scalatest.funsuite.AnyFunSuite
import ulti.shared._
import ulti.shared.abstract0._

class PlayCardTest extends AnyFunSuite {

  val iu = InnerUltiRouteGenerator.step3(14)
  val step1 = iu.reduce(PlayCard(Card(Hearts, V10)), UltiPlayer2)

  test("good") {
    assert(iu.round == 2)
    assert(step1._2 == OK)
    val step2 = step1._1.reduce(PlayCard(Card(Hearts, VAce)), UltiPlayer3)
    assert(step2._2 == OK)
    val step3 = step2._1.reduce(PlayCard(Card(Hearts, VKing)), UltiPlayer1)
    assert(step3._2 == OK)

    assert(step3._1.round == 3)
  }

  test("wrong player") {
    val wrongStep1 = step1._1.reduce(PlayCard(Card(Hearts, VAce)), UltiPlayer1)
    val wrongStep2 = step1._1.reduce(PlayCard(Card(Hearts, VAce)), UltiPlayer2)

    assert(wrongStep1._2.isInstanceOf[GeneralFailure])
    println(wrongStep1._2.asInstanceOf[GeneralFailure].message)
    assert(wrongStep2._2.isInstanceOf[Failure])
    println(wrongStep2._2.asInstanceOf[GeneralFailure].message)
  }

  test("given card is not in hand") {
    val wrongStep1 = step1._1.reduce(PlayCard(Card(Hearts, V7)), UltiPlayer3)
    val wrongStep2 = step1._1.reduce(PlayCard(Card(Acorns, V8)), UltiPlayer3)

    assert(wrongStep1._2.isInstanceOf[GeneralFailure])
    println(wrongStep1._2.asInstanceOf[GeneralFailure].message)
    assert(wrongStep2._2.isInstanceOf[Failure])
    println(wrongStep2._2.asInstanceOf[GeneralFailure].message)
  }

  test("Suit must be followed") {
    val wrongStep1 = step1._1.reduce(PlayCard(Card(Acorns, VAce)), UltiPlayer3)

    assert(wrongStep1._2.isInstanceOf[GeneralFailure])
    println(wrongStep1._2.asInstanceOf[GeneralFailure].message)
  }

  test("A higher valued card must be played if possible.") {
    val wrongStep1 = step1._1.reduce(PlayCard(Card(Hearts, VUnder)), UltiPlayer3)

    assert(wrongStep1._2.isInstanceOf[GeneralFailure])
    println(wrongStep1._2.asInstanceOf[GeneralFailure].message)
  }
}
