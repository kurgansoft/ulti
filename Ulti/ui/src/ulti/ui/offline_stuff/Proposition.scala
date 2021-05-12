package ulti.ui.offline_stuff

import ulti.shared._

abstract sealed class Proposition {
  val incompatibleWith: Proposition => Boolean
  val toBid: Bid
}

case object RedProposition extends Proposition {
  override val incompatibleWith: Proposition => Boolean = proposition => {
    proposition.isInstanceOf[GeneralPlainDurchmarsProposition]
  }
  override val toBid: Bid = null
}
case object UltiProposition extends Proposition {
  override val incompatibleWith: Proposition => Boolean = proposition => {
    proposition.isInstanceOf[GeneralBetliProposition] ||
    proposition.isInstanceOf[GeneralPlainDurchmarsProposition] ||
    proposition == SimpleProposition
  }

  override val toBid: Bid = BidUlti
}

abstract sealed class PartyProposition extends Proposition
case object SimpleProposition extends PartyProposition {
  override val incompatibleWith: Proposition => Boolean = proposition => {
    proposition != RedProposition && !proposition.isInstanceOf[PartyProposition]
  }

  override val toBid: Bid = BidSimple
}
case object Proposition20_100 extends PartyProposition {
  override val incompatibleWith: Proposition => Boolean = proposition => {
    proposition.isInstanceOf[GeneralBetliProposition] || proposition.isInstanceOf[GeneralPlainDurchmarsProposition]
  }

  override val toBid: Bid = Bid20_100
}
case object Proposition40_100 extends PartyProposition {
  override val incompatibleWith: Proposition => Boolean = proposition => {
    proposition.isInstanceOf[GeneralBetliProposition] || proposition.isInstanceOf[GeneralPlainDurchmarsProposition]
  }

  override val toBid: Bid = Bid40_100
}

abstract sealed class GeneralBetliProposition extends Proposition {
  override val incompatibleWith: Proposition => Boolean = proposition => {
    proposition != RedProposition && !proposition.isInstanceOf[GeneralBetliProposition]
  }
}
case object BetliProposition extends GeneralBetliProposition {
  override val toBid: Bid = BidBetli()
}
case object OpenBetliProposition extends GeneralBetliProposition {
  override val toBid: Bid = BidBetli(true)
}

abstract sealed class GeneralDurchmarsProposition extends Proposition {
  override val incompatibleWith: Proposition => Boolean = proposition => {
    proposition.isInstanceOf[GeneralBetliProposition] ||
    proposition == SimpleProposition
  }
}
case object DurchmarsProposition extends GeneralDurchmarsProposition {
  override val toBid: Bid = BidDurchmars()
}
case object OpenDurchmarsProposition extends GeneralDurchmarsProposition {
  override val toBid: Bid = BidDurchmars(true)
}

abstract sealed class GeneralPlainDurchmarsProposition extends GeneralDurchmarsProposition {
  //Plain durchmars cannot be combined with anything else
  override val incompatibleWith: Proposition => Boolean = proposition => {
    !proposition.isInstanceOf[GeneralDurchmarsProposition]
  }
}
case object PlainDurchmarsProposition extends GeneralPlainDurchmarsProposition {
  override val toBid: Bid = BidPlainDurchmars()
}
case object PlainAndOpenDurchmarsProposition extends GeneralPlainDurchmarsProposition {
  override val toBid: Bid = BidPlainDurchmars(true)
}