package ulti.shared

import org.scalatest.funsuite.AnyFunSuite

class BidTest extends AnyFunSuite {
  test("value of ulti") {
    assert(BidUlti.value == List(4,1))
    assert(CombinedBid(Set(BidUlti, Bid40_100)).value == List(4,4))
  }
}
