package ulti.shared

import org.scalatest.funsuite.AnyFunSuite

class CardValueTest extends AnyFunSuite {
  test("orders in trump & plain games") {
    implicit var trumpSuit: CardSuit = Hearts

    assert(V7 < V8)
    assert(V8 < V9)
    assert(V9 < V10)
    assert(VUnder < V10)
    assert(VOver < V10)
    assert(VKing < V10)
    assert(V10 < VAce)

    trumpSuit = Plain

    assert(V7 < V8)
    assert(V8 < V9)
    assert(V9 < V10)
    assert(VUnder > V10)
    assert(VOver > V10)
    assert(VKing > V10)
    assert(V10 < VAce)
  }
}
