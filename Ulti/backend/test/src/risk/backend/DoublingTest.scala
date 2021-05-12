package risk.backend

import gbge.backend.GeneralFailure
import org.scalatest.funsuite.AnyFunSuite
import ulti.backend.InnerUlti
import ulti.shared._
import ulti.shared.abstract0._
import ulti.shared.contracts.{ContractSilentDurchmars, ContractSimple, ContractUlti}

class DoublingTest extends AnyFunSuite {
  test("You can't double in the bidding phase") {
    val iu = InnerUlti.generate(UltiPlayer1, 1L)
    for (player <- List(UltiPlayer1, UltiPlayer2, UltiPlayer3)) {
      for (contract <- Helper.allThePossibleContracts) {
        val res = iu.reduce(DoubleContract(contract), player)
        assert(res._2.isInstanceOf[GeneralFailure])
        val failureMessage = res._2.asInstanceOf[GeneralFailure].message
        assert(failureMessage == InnerUlti.FailureMessages.doublingOutsideTheFirstRound)
      }
    }
  }

  test("You can't double in the GameOverPhase") {
    val iu = InnerUltiRouteGenerator.gameOverState
    assert(iu.phase == GameOverPhase)
    for (player <- List(UltiPlayer1, UltiPlayer2, UltiPlayer3)) {
      for (contract <- Helper.allThePossibleContracts) {
        val res = iu.reduce(DoubleContract(contract), player)
        assert(res._2.isInstanceOf[GeneralFailure])
        val failureMessage = res._2.asInstanceOf[GeneralFailure].message
        assert(failureMessage == InnerUlti.FailureMessages.doublingOutsideTheFirstRound)
      }
    }
  }

  test("You may only double in the first round") {
    val iu = InnerUltiRouteGenerator.step3(14)
    assert(iu.phase == PlayingPhase)
    assert(iu.round == 2)

    for (player <- List(UltiPlayer1, UltiPlayer2, UltiPlayer3)) {
      for (contract <- Helper.allThePossibleContracts) {
        val res = iu.reduce(DoubleContract(contract), player)
        assert(res._2.isInstanceOf[GeneralFailure])
        val failureMessage = res._2.asInstanceOf[GeneralFailure].message
        assert(failureMessage == InnerUlti.FailureMessages.doublingOutsideTheFirstRound)
      }
    }
  }

  test("The contract you are trying to double is non-existent") {
    val iu = InnerUltiRouteGenerator.step3(12)
    assert(iu.phase == PlayingPhase)
    assert(iu.round == 1)
    for (contract <- Helper.allThePossibleContracts.toSet -- iu.contracts(iu.currentBidWinner.get).map(_.contract)) {
      val res = iu.reduce(DoubleContract(contract), iu.currentPlayer)
      assert(res._2.isInstanceOf[GeneralFailure])
      val failureMessage = res._2.asInstanceOf[GeneralFailure].message
      assert(failureMessage == InnerUlti.FailureMessages.doublingNonExistentContract)
    }
  }

  test("Silent contracts can't be doubled.") {
    val iu = InnerUltiRouteGenerator.step3(12)
    val res = iu.reduce(DoubleContract(ContractSilentDurchmars(Acorns)), iu.currentPlayer)
    val failureMessage = res._2.asInstanceOf[GeneralFailure].message
    assert(failureMessage == InnerUlti.FailureMessages.doublingSilentContract)
  }

  test("It is the defenders turn to double this contract.") {
    val iu = InnerUltiRouteGenerator.step3(12)
    println(iu.contracts(iu.currentBidWinner.get).map(_.contract))
    val res = iu.reduce(DoubleContract(ContractSimple(Acorns)), iu.currentBidWinner.get)
    val failureMessage = res._2.asInstanceOf[GeneralFailure].message
    assert(failureMessage == InnerUlti.FailureMessages.defendersTurnToDouble)
  }

  test("This is the defenders turn to double this contract, and you are one of the defenders; still you may only double in your turn.") {
    val iu = InnerUltiRouteGenerator.step3(12)
    val res = iu.reduce(DoubleContract(ContractUlti(Acorns)), UltiPlayer1)
    val failureMessage = res._2.asInstanceOf[GeneralFailure].message
    assert(failureMessage == InnerUlti.FailureMessages.defendersTurnButNotYourTurn)
  }

  test("This is the soloist turn to double this contract.") {
    val iu = InnerUltiRouteGenerator.step3(12)
    val res = iu.reduce(DoubleContract(ContractUlti(Acorns)), UltiPlayer3)
    val x = res._1.contractsOfTheBidWinnerLen.get(res._1.contracts)
    val iu2 = res._1
    val res2 = iu2.reduce(DoubleContract(ContractUlti(Acorns)), UltiPlayer3)
    val failureMessage = res2._2.asInstanceOf[GeneralFailure].message
    assert(failureMessage == InnerUlti.FailureMessages.soloistTurnToDouble)
  }

  test("Soloist may not double further, once the block is lifted.") {
    val iu = InnerUltiRouteGenerator.step3(12)
    val res = iu.reduce(DoubleContract(ContractUlti(Acorns)), UltiPlayer3)
    val res2 = res._1.reduce(DoneWithDoubling, iu.currentBidWinner.get)
    val res3 = res2._1.reduce(DoubleContract(ContractUlti(Acorns)), iu.currentBidWinner.get)
    val failureMessage = res3._2.asInstanceOf[GeneralFailure].message
    assert(InnerUlti.FailureMessages.soloistHasLiftedTheBlockAlready == failureMessage)
  }

//  test("blockIsLifted flag DOES NOT revert back to false, once a card is played") {
//    val iu = InnerUltiRouteGenerator.step3(12)
//    val res = iu.reduce(DoubleContract(ContractUlti(Acorns)), UltiPlayer3)
//    val res2 = res._1.reduce(DoneWithDoubling, iu.currentBidWinner.get)
//    val iu2 = res2._1
//    assert(iu2.blockIsLifted)
//    val res3 = iu2.reduce(PlayCard(Card(Bells, VOver)), UltiPlayer3)
//    val iu3 = res3._1
//    assert(iu3.cardsOnTheTable.size == 2)
//    assert(!iu3.blockIsLifted)
//  }

  test("blockIsLifted flag reverts back to false if the current player doubles another bid") {
    val iu = InnerUltiRouteGenerator.step3(12)
    val res = iu.reduce(DoubleContract(ContractUlti(Acorns)), UltiPlayer3)
    val res2 = res._1.reduce(DoneWithDoubling, iu.currentBidWinner.get)
    val iu2 = res2._1
    assert(iu2.blockIsLifted)

    val res3 = iu2.reduce(DoubleContract(ContractSimple(Acorns)), UltiPlayer3)
    val iu3 = res3._1
    assert(!iu3.blockIsLifted)
  }
  // Individual contra cases

  test("You cannot go over Fedak Sari") {
    println("todo")
  }

}
