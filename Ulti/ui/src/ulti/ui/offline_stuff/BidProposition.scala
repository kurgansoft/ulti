package ulti.ui.offline_stuff

import ulti.shared.{Bid, CombinableBid, CombinedBid, Red}

case class BidProposition(
                           red: Boolean = false,
                           partyColumn: Option[PartyProposition] = None,
                           ulti: Boolean = false,
                           betliColumn: Option[GeneralBetliProposition] = None,
                           durchmarsColumn: Option[GeneralDurchmarsProposition] = None
                         ) {
  def invariant(): Unit = {
    if (durchmarsColumn.exists(d => d == PlainAndOpenDurchmarsProposition || d == PlainDurchmarsProposition)) {
      assert(betliColumn.isEmpty)
      assert(!ulti)
      assert(partyColumn.isEmpty)
      assert(!red)
    }
    if (durchmarsColumn.exists(d => d == DurchmarsProposition || d == OpenDurchmarsProposition)) {
      assert(betliColumn.isEmpty)
      assert(partyColumn.isEmpty)
    }
    if (betliColumn.nonEmpty) {
      assert(durchmarsColumn.isEmpty)
      assert(!ulti)
      assert(partyColumn.isEmpty)
    }
  }

  val listOfPropositions: List[Proposition] = {
    val redList = if (red) List(RedProposition) else List.empty
    val ultiList = if (ulti) List(UltiProposition) else List.empty

    partyColumn.toList :::
    betliColumn.toList :::
    durchmarsColumn.toList :::
      redList ::: ultiList
  }

  val generatedBid: Option[Bid] = {
    if (partyColumn.isEmpty && durchmarsColumn.isEmpty && betliColumn.isEmpty && !ulti)
      None
    else {
      val column1 = partyColumn.map(_.toBid)
      val column2 = if (ulti) Some(UltiProposition.toBid) else None
      val column3 = betliColumn.map(_.toBid)
      val column4 = durchmarsColumn.map(_.toBid)
      val asSet: Set[Bid] = (column1.toList ::: column2.toList ::: column3.toList ::: column4.toList).toSet
      val temp = if (asSet.size > 1) {
        CombinedBid(asSet.map(_.asInstanceOf[CombinableBid]))
      } else {
        asSet.head
      }
      if (red)
        Some(Red(temp))
       else
        Some(temp)
    }
  }

  def reduce(proposition: Proposition): BidProposition = {
    if (red && RedProposition.incompatibleWith(proposition)) {
      this
    } else if (partyColumn.exists(_.incompatibleWith(proposition))) {
      this
    } else if (betliColumn.exists(_.incompatibleWith(proposition))) {
      this
    } else if (durchmarsColumn.exists(_.incompatibleWith(proposition))) {
      this
    } else if (ulti && UltiProposition.incompatibleWith(proposition)) {
      this
    } else {
      proposition match {
        case RedProposition => {
          this.copy(red = !red)
        }
        case pp: PartyProposition => {
          if (partyColumn.contains(pp)) {
            this.copy(partyColumn = None)
          } else {
            pp match {
              case SimpleProposition => {

                this.copy(partyColumn = Some(pp))
              }
              case Proposition20_100 => {
                this.copy(partyColumn = Some(pp))
              }
              case Proposition40_100 => {
                this.copy(partyColumn = Some(pp))
              }
            }
          }
        }
        case UltiProposition => {
          this.copy(ulti = !ulti)
        }
        case gbp: GeneralBetliProposition => {
          if (betliColumn.contains(gbp)) {
            this.copy(betliColumn = None)
          } else
            this.copy(betliColumn = Some(gbp))
        }
        case gdp: GeneralDurchmarsProposition => {
          if (durchmarsColumn.contains(gdp)) {
            this.copy(durchmarsColumn = None)
          } else
            this.copy(durchmarsColumn = Some(gdp))
        }
        case _ => this
      }
    }
  }
}
