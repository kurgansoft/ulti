package ulti.ui.offline_stuff

import org.scalatest.funsuite.AnyFunSuite

class BidPropositionTest extends AnyFunSuite {

  val initialPropositionState = BidProposition()

  test("initial state") {
    initialPropositionState.invariant()
    assert(!initialPropositionState.red)
    assert(initialPropositionState.partyColumn.isEmpty)
    assert(!initialPropositionState.ulti)
    assert(initialPropositionState.betliColumn.isEmpty)
    assert(initialPropositionState.durchmarsColumn.isEmpty)
  }

  test("RedProposition should change red status (most of the time)") {
    val bp2 = initialPropositionState.reduce(RedProposition)
    bp2.invariant()
    assert(bp2.red)
    val bp3 = bp2.reduce(RedProposition)
    bp3.invariant()
    assert(!bp3.red)
  }

  test("SP") {
    val bp2 = initialPropositionState.reduce(SimpleProposition)
    bp2.invariant()
    assert(bp2.partyColumn.contains(SimpleProposition))
    val bp3 = initialPropositionState.reduce(Proposition20_100)
    bp3.invariant()
    assert(bp3.partyColumn.contains(Proposition20_100))
    val bp4 = initialPropositionState.reduce(Proposition40_100)
    bp4.invariant()
    assert(bp4.partyColumn.contains(Proposition40_100))
  }

  test("ulti") {
    val bp2 = initialPropositionState.reduce(UltiProposition)
    bp2.invariant()
    assert(bp2.ulti)
    val bp3 = bp2.reduce(UltiProposition)
    bp3.invariant()
    assert(bp3 == initialPropositionState)
  }

  test("betli") {
    val bp2 = initialPropositionState.reduce(BetliProposition)
    bp2.invariant()
    assert(bp2.partyColumn.isEmpty && bp2.betliColumn.contains(BetliProposition))

    val bp3 = initialPropositionState.reduce(OpenBetliProposition)
    bp3.invariant()
    assert(bp3.partyColumn.isEmpty && bp3.betliColumn.contains(OpenBetliProposition))
  }

  test("durchmars") {
    val bp2 = initialPropositionState.reduce(DurchmarsProposition)
    bp2.invariant()
    assert(bp2.partyColumn.isEmpty && bp2.durchmarsColumn.contains(DurchmarsProposition))

    val bp3 = initialPropositionState.reduce(PlainDurchmarsProposition)
    bp3.invariant()
    assert(bp3.partyColumn.isEmpty && bp3.durchmarsColumn.contains(PlainDurchmarsProposition))

    val bp4 = initialPropositionState.reduce(OpenDurchmarsProposition)
    bp4.invariant()
    assert(bp4.partyColumn.isEmpty && bp4.durchmarsColumn.contains(OpenDurchmarsProposition))

    val bp5 = initialPropositionState.reduce(PlainAndOpenDurchmarsProposition)
    bp5.invariant()
    assert(bp5.partyColumn.isEmpty && bp5.durchmarsColumn.contains(PlainAndOpenDurchmarsProposition))
  }

  test("plain durchmars problems") {
    val plainDurchMars = initialPropositionState.reduce(PlainDurchmarsProposition)
    val bp2 = plainDurchMars.reduce(RedProposition)
    assert(bp2 == plainDurchMars)
  }

  test("simple + simple") {
    val p1 = initialPropositionState.reduce(SimpleProposition)
    val p2 = p1.reduce(SimpleProposition)
  }

  test("betli + betli") {
    val p1 = initialPropositionState.reduce(BetliProposition)
    val p2 = p1.reduce(BetliProposition)
  }
}
