package ulti.shared

import org.scalatest.funsuite.AnyFunSuite
import ulti.shared
import ulti.shared.contracts.{Contract20_100, Contract40_100, ContractBetli, ContractDurchmars, ContractSimple, ContractUlti}

class ContractTest extends AnyFunSuite {
  test("contract toString methods") {
    val contracts = List(
      ContractSimple(Acorns),
      Contract40_100(Acorns),
      Contract20_100(Acorns),
      ContractUlti(Acorns),
      ContractBetli(),
      ContractBetli(open = true),
      ContractDurchmars(Acorns),
      shared.contracts.ContractDurchmars(Plain),
      shared.contracts.ContractDurchmars(Acorns, open = true),
      shared.contracts.ContractDurchmars(Plain, open = true),

      shared.contracts.ContractSimple(Hearts),
      shared.contracts.Contract40_100(Hearts),
      shared.contracts.Contract20_100(Hearts),
      shared.contracts.ContractUlti(Hearts),
      ContractBetli(),
      ContractBetli(open = true),
      shared.contracts.ContractDurchmars(Hearts),
      shared.contracts.ContractDurchmars(Plain),
      shared.contracts.ContractDurchmars(Hearts, open = true),
      shared.contracts.ContractDurchmars(Plain, open = true),
    )

    for (contract <- contracts) {
      println(contract.toString)
    }

  }

}
