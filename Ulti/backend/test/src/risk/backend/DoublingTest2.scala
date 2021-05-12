package risk.backend

import gbge.backend.GeneralFailure
import org.scalatest.funsuite.AnyFunSuite
import ulti.backend.InnerUlti
import ulti.shared.{Acorns, DoubleContract}
import ulti.shared.abstract0._
import ulti.shared.contracts.{ContractSimple, ContractUlti}

class DoublingTest2 extends AnyFunSuite {
  test("Simple is doubled in joint.") {
    val iu = InnerUltiRouteGenerator.step3(12)
    val res2 = iu.reduce(DoubleContract(ContractSimple(Acorns)), UltiPlayer3)
    val c2 = res2._1.contractsOfTheBidWinner

    val doubledSimple = c2.find(_.contract.isInstanceOf[ContractSimple]).get
    for (up <- UltiPlayer.allThePlayers) {
      assert(doubledSimple.contraLevel.levelForPlayer(up) == 1)
    }
  }

  test("Ulti is doubled individually.") { // Also everything else?
    val iu = InnerUltiRouteGenerator.step3(12)
    val res2 = iu.reduce(DoubleContract(ContractUlti(Acorns)), UltiPlayer3)
    val c2 = res2._1.contractsOfTheBidWinner

    val doubledUlti = c2.find(_.contract.isInstanceOf[ContractUlti]).get

    println(doubledUlti.contraLevel.levelForPlayer(UltiPlayer3))
    println("currentBidWinner: " + iu.currentBidWinner.get)
    assert(doubledUlti.contraLevel.levelForPlayer(UltiPlayer3) == 1)
    assert(doubledUlti.contraLevel.levelForPlayer(iu.currentBidWinner.get) == 0)
    assert(doubledUlti.contraLevel.levelForPlayer(UltiPlayer1) == 0)
  }

  test("countering Simple until Fedak Sari") {
    val iu = InnerUltiRouteGenerator.step3(12)

    val res_level1 = iu.reduce(DoubleContract(ContractSimple(Acorns)), UltiPlayer3) // contra -> 1
    val doubledSimpleLevel1 = res_level1._1.contractsOfTheBidWinner.find(_.contract.isInstanceOf[ContractSimple]).get
    for (up <- UltiPlayer.allThePlayers) {
      assert(doubledSimpleLevel1.contraLevel.levelForPlayer(up) == 1)
    }

    val res_level2 = res_level1._1.reduce(DoubleContract(ContractSimple(Acorns)), iu.currentBidWinner.get) // re-contra -> 2
    val doubledSimpleLevel2 = res_level2._1.contractsOfTheBidWinner.find(_.contract.isInstanceOf[ContractSimple]).get
    for (up <- UltiPlayer.allThePlayers) {
      assert(doubledSimpleLevel2.contraLevel.levelForPlayer(up) == 2)
    }

    val res_level3 = res_level2._1.reduce(DoubleContract(ContractSimple(Acorns)), UltiPlayer3) // sub-contra -> 3
    val doubledSimpleLevel3 = res_level3._1.contractsOfTheBidWinner.find(_.contract.isInstanceOf[ContractSimple]).get
    for (up <- UltiPlayer.allThePlayers) {
      assert(doubledSimpleLevel3.contraLevel.levelForPlayer(up) == 3)
    }

    val res_level4 = res_level3._1.reduce(DoubleContract(ContractSimple(Acorns)), iu.currentBidWinner.get) // mord-contra -> 4
    val doubledSimpleLevel4 = res_level4._1.contractsOfTheBidWinner.find(_.contract.isInstanceOf[ContractSimple]).get
    for (up <- UltiPlayer.allThePlayers) {
      assert(doubledSimpleLevel4.contraLevel.levelForPlayer(up) == 4)
    }

    val res_level5 = res_level4._1.reduce(DoubleContract(ContractSimple(Acorns)), UltiPlayer3) // hirsch-contra -> 5
    val doubledSimpleLevel5 = res_level5._1.contractsOfTheBidWinner.find(_.contract.isInstanceOf[ContractSimple]).get
    for (up <- UltiPlayer.allThePlayers) {
      assert(doubledSimpleLevel5.contraLevel.levelForPlayer(up) == 5)
    }

    val res_level6 = res_level5._1.reduce(DoubleContract(ContractSimple(Acorns)), iu.currentBidWinner.get) // Fedák Sári -> 6
    val doubledSimpleLevel6 = res_level6._1.contractsOfTheBidWinner.find(_.contract.isInstanceOf[ContractSimple]).get
    for (up <- UltiPlayer.allThePlayers) {
      assert(doubledSimpleLevel6.contraLevel.levelForPlayer(up) == 6)
    }

    //Cannot go over Fedák Sári

    for (up <- UltiPlayer.allThePlayers) {
      val overFedakSari = res_level6._1.reduce(DoubleContract(ContractSimple(Acorns)), up)
      val failureMessage = overFedakSari._2.asInstanceOf[GeneralFailure].message
      assert(failureMessage == InnerUlti.FailureMessages.maximumContraLevelAlreadyReached)
    }
  }

  test("countering Ulti until Fedak Sari") {
    val iu = InnerUltiRouteGenerator.step3(12)

    val res_level1 = iu.reduce(DoubleContract(ContractUlti(Acorns)), UltiPlayer3) // contra -> 1
    val doubledUltiLevel1 = res_level1._1.contractsOfTheBidWinner.find(_.contract.isInstanceOf[ContractUlti]).get
    assert(doubledUltiLevel1.contraLevel.levelForPlayer(UltiPlayer3) == 1)
    assert(doubledUltiLevel1.contraLevel.levelForPlayer(iu.currentBidWinner.get) == 0)
    assert(doubledUltiLevel1.contraLevel.levelForPlayer(UltiPlayer1) == 0)

    val res_level2 = res_level1._1.reduce(DoubleContract(ContractUlti(Acorns)), iu.currentBidWinner.get) // re-contra -> 2
    val doubledUltiLevel2 = res_level2._1.contractsOfTheBidWinner.find(_.contract.isInstanceOf[ContractUlti]).get
    assert(doubledUltiLevel2.contraLevel.levelForPlayer(UltiPlayer3) == 2)
    assert(doubledUltiLevel2.contraLevel.levelForPlayer(iu.currentBidWinner.get) == 0)
    assert(doubledUltiLevel2.contraLevel.levelForPlayer(UltiPlayer1) == 0)

    val res_level3 = res_level2._1.reduce(DoubleContract(ContractUlti(Acorns)), UltiPlayer3) // sub-contra -> 3
    val doubledUltiLevel3 = res_level3._1.contractsOfTheBidWinner.find(_.contract.isInstanceOf[ContractUlti]).get
    assert(doubledUltiLevel3.contraLevel.levelForPlayer(UltiPlayer3) == 3)
    assert(doubledUltiLevel3.contraLevel.levelForPlayer(iu.currentBidWinner.get) == 0)
    assert(doubledUltiLevel3.contraLevel.levelForPlayer(UltiPlayer1) == 0)

    val res_level4 = res_level3._1.reduce(DoubleContract(ContractUlti(Acorns)), iu.currentBidWinner.get) // mord-contra -> 4
    val doubledUltiLevel4 = res_level4._1.contractsOfTheBidWinner.find(_.contract.isInstanceOf[ContractUlti]).get
    assert(doubledUltiLevel4.contraLevel.levelForPlayer(UltiPlayer3) == 4)
    assert(doubledUltiLevel4.contraLevel.levelForPlayer(iu.currentBidWinner.get) == 0)
    assert(doubledUltiLevel4.contraLevel.levelForPlayer(UltiPlayer1) == 0)

    val res_level5 = res_level4._1.reduce(DoubleContract(ContractUlti(Acorns)), UltiPlayer3) // hirsch-contra -> 5
    val doubledUltiLevel5 = res_level5._1.contractsOfTheBidWinner.find(_.contract.isInstanceOf[ContractUlti]).get
    assert(doubledUltiLevel5.contraLevel.levelForPlayer(UltiPlayer3) == 5)
    assert(doubledUltiLevel5.contraLevel.levelForPlayer(iu.currentBidWinner.get) == 0)
    assert(doubledUltiLevel5.contraLevel.levelForPlayer(UltiPlayer1) == 0)

    val res_level6 = res_level5._1.reduce(DoubleContract(ContractUlti(Acorns)), iu.currentBidWinner.get) // Fedák Sári -> 6
    val doubledUltiLevel6 = res_level6._1.contractsOfTheBidWinner.find(_.contract.isInstanceOf[ContractUlti]).get
    assert(doubledUltiLevel6.contraLevel.levelForPlayer(UltiPlayer3) == 6)
    assert(doubledUltiLevel6.contraLevel.levelForPlayer(iu.currentBidWinner.get) == 0)
    assert(doubledUltiLevel6.contraLevel.levelForPlayer(UltiPlayer1) == 0)

    //Cannot go over Fedák Sári

    for (up <- UltiPlayer.allThePlayers) {
      val overFedakSari = res_level6._1.reduce(DoubleContract(ContractUlti(Acorns)), up)
      val failureMessage = overFedakSari._2.asInstanceOf[GeneralFailure].message
      assert(failureMessage == InnerUlti.FailureMessages.maximumContraLevelAlreadyReached)
    }
  }
}
