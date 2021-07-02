package ulti.backend

import ulti.shared._
import ulti.shared.contracts._

object Helper {
  val allThePossibleContracts: List[Contract] = List(
    ContractBetli(),
    ContractBetli(red = true),
    ContractBetli(open = true),
    ContractBetli(open = true, red = true),

    ContractDurchmars(Plain),
    ContractDurchmars(Plain, open = true),
  ) ++ (for (suit <- List(Hearts, Leaves, Acorns, Bells)) yield
    List(
      ContractSimple(suit),
      Contract20_100(suit),
      Contract40_100(suit),
      ContractUlti(suit),
      ContractDurchmars(suit),
      ContractDurchmars(suit, open = true)
    )).flatten
}
