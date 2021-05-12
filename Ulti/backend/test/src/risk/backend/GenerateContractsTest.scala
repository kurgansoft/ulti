package risk.backend

import org.scalatest.funsuite.AnyFunSuite
import ulti.backend.InnerUlti
import ulti.shared._
import ulti.shared.contracts._

class GenerateContractsTest extends AnyFunSuite {

  val ulti = InnerUlti(playerHands = Map.empty)

  test("simple") {
    val contracts = ulti.calculateContractsForBidWinner(BidSimple, Leaves)
    assert(contracts.size == 3)
    assert(contracts.contains(ContractSimple(Leaves)))
    assert(contracts.contains(ContractSilentDurchmars(Leaves)))
    assert(contracts.contains(ContractSilentUlti(Leaves)))
  }

  test("simple with at least one marriage") {
    val contracts = ulti.calculateContractsForBidWinner(BidSimple, Leaves, atLeastOneMarriageIsDeclared = true)
    assert(contracts.size == 4)
    assert(contracts.contains(ContractSimple(Leaves)))
    assert(contracts.contains(ContractSilentDurchmars(Leaves)))
    assert(contracts.contains(ContractSilentUlti(Leaves)))
    assert(contracts.contains(ContractSilent100(Leaves)))
  }

  test("ulti") {
    val contracts = ulti.calculateContractsForBidWinner(BidUlti, Leaves)
    assert(contracts.size == 3)
    assert(contracts.contains(ContractUlti(Leaves)))
    assert(contracts.contains(ContractSimple(Leaves)))
    assert(contracts.contains(ContractSilentDurchmars(Leaves)))
  }

  test("ulti with at least one marriage") {
    val contracts = ulti.calculateContractsForBidWinner(BidUlti, Leaves, atLeastOneMarriageIsDeclared = true)
    assert(contracts.size == 4)
    assert(contracts.contains(ContractUlti(Leaves)))
    assert(contracts.contains(ContractSimple(Leaves)))
    assert(contracts.contains(ContractSilentDurchmars(Leaves)))
    assert(contracts.contains(ContractSilent100(Leaves)))
  }

  test("40-100") {
    val contracts = ulti.calculateContractsForBidWinner(Bid40_100, Leaves)
    val contracts2 = ulti.calculateContractsForBidWinner(Bid40_100, Leaves, atLeastOneMarriageIsDeclared = true)
    assert(contracts == contracts2)
    assert(contracts.size == 3)
    assert(contracts.contains(Contract40_100(Leaves)))
    assert(contracts.contains(ContractSilentDurchmars(Leaves)))
    assert(contracts.contains(ContractSilentUlti(Leaves)))
  }

  test("20-100") {
    val contracts = ulti.calculateContractsForBidWinner(Bid20_100, Leaves)
    val contracts2 = ulti.calculateContractsForBidWinner(Bid20_100, Leaves, atLeastOneMarriageIsDeclared = true)
    assert(contracts == contracts2)
    assert(contracts.size == 3)
    assert(contracts.contains(Contract20_100(Leaves)))
    assert(contracts.contains(ContractSilentDurchmars(Leaves)))
    assert(contracts.contains(ContractSilentUlti(Leaves)))
  }

  test("betli") {
    val contracts = ulti.calculateContractsForBidWinner(BidBetli(), Hearts)
    val contracts2 = ulti.calculateContractsForBidWinner(BidBetli(true), Leaves)
    val contracts3 = ulti.calculateContractsForBidWinner(Red(BidBetli()))
    val contracts4 = ulti.calculateContractsForBidWinner(Red(BidBetli(true)))

    assert(contracts.size == 1)
    assert(contracts.contains(ContractBetli()))

    assert(contracts2.size == 1)
    assert(contracts2.contains(ContractBetli(open = true)))

    assert(contracts3.size == 1)
    assert(contracts3.contains(ContractBetli(red = true)))

    assert(contracts4.size == 1)
    assert(contracts4.contains(ContractBetli(red = true, open = true)))
  }

  test("durchmars") {
    val contracts = ulti.calculateContractsForBidWinner(BidDurchmars(), Leaves)
    assert(contracts.size == 1)
    assert(contracts.contains(ContractDurchmars(Leaves)))

    val contracts2 = ulti.calculateContractsForBidWinner(BidDurchmars(open = true), Leaves)
    assert(contracts2.size == 1)
    assert(contracts2.contains(ContractDurchmars(Leaves, open = true)))

    val contracts3 = ulti.calculateContractsForBidWinner(BidPlainDurchmars())
    val contractsx = ulti.calculateContractsForBidWinner(BidPlainDurchmars(), Hearts)
    assert(contracts3 == contractsx)
    assert(contracts3.size == 1)
    assert(contracts3.contains(ContractDurchmars(Plain)))

    val contracts4 = ulti.calculateContractsForBidWinner(BidPlainDurchmars(open = true))
    val contracts4x = ulti.calculateContractsForBidWinner(BidPlainDurchmars(open = true), Hearts)
    assert(contracts4 == contracts4x)
    assert(contracts4.size == 1)
    assert(contracts4.contains(ContractDurchmars(Plain, open = true)))
  }

  test("40-100 ulti") {
    val contracts = ulti.calculateContractsForBidWinner(CombinedBid(Set(Bid40_100, BidUlti)), Leaves)
    assert(contracts.size == 3)
    assert(contracts.contains(Contract40_100(Leaves)))
    assert(contracts.contains(ContractUlti(Leaves)))
    assert(contracts.contains(ContractSilentDurchmars(Leaves)))

    val contracts2 = ulti.calculateContractsForBidWinner(Red(CombinedBid(Set(Bid40_100, BidUlti))), Bells)
    assert(contracts2.size == 3)
    assert(contracts2.contains(Contract40_100(Hearts)))
    assert(contracts2.contains(ContractUlti(Hearts)))
    assert(contracts2.contains(ContractSilentDurchmars(Hearts)))
  }

  test("20-100 ulti durchmars") {
    val contracts = ulti.calculateContractsForBidWinner(CombinedBid(Set(Bid20_100, BidUlti, BidDurchmars())), Leaves)
    assert(contracts.size == 3)
    assert(contracts.contains(Contract20_100(Leaves)))
    assert(contracts.contains(ContractUlti(Leaves)))
    assert(contracts.contains(ContractDurchmars(Leaves)))

    val contracts2 = ulti.calculateContractsForBidWinner(Red(CombinedBid(Set(Bid20_100, BidUlti, BidDurchmars()))), Acorns)
    assert(contracts2.size == 3)
    assert(contracts2.contains(Contract20_100(Hearts)))
    assert(contracts2.contains(ContractUlti(Hearts)))
    assert(contracts2.contains(ContractDurchmars(Hearts)))

    val contracts3 = ulti.calculateContractsForBidWinner(Red(CombinedBid(Set(Bid40_100, BidUlti, BidDurchmars(open = true)))), Acorns)
    println(contracts3)
  }
}
