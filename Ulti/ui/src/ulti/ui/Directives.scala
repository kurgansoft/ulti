package ulti.ui

import gbge.client.ClientEventHandler
import gbge.shared.FrontendPlayer
import japgolly.scalajs.react.{Callback, ReactEventFrom}
import japgolly.scalajs.react.vdom.TagOf
import ulti.shared._
import japgolly.scalajs.react.vdom.all._
import org.scalajs.dom.html.{Button, Div, Image, Input}
import org.scalajs.dom.raw.HTMLLinkElement
import ulti.shared.abstract0._
import ulti.shared.client.{ClientInnerUlti, ClientUlti}
import ulti.shared.contracts.{ContraLevel, ContractWithStatus, NoContra, Open}
import ulti.ui.offline_stuff._

object Directives {

  def trumpSuitText(trumpSuit: Option[CardSuit]): String = {
    if (trumpSuit.isEmpty)
      "Trump suit is not declared yet."
    else if (trumpSuit.get == Plain)
      "This is a plain game, no trump suit."
    else
      "Trump suit: " + trumpSuit.get
  }

  def loadExternalCss(): Unit = {
    val route: String = "./games/ulti/ulti.css"
    val document = org.scalajs.dom.document

    val ussId = "ultiStyleSheet"
    val theLinkElement = document.getElementById(ussId)
    if (theLinkElement == null) {
      val headSection = document.getElementsByTagName("head")(0)
      val cssNode = document.createElement("link").asInstanceOf[HTMLLinkElement]
      cssNode.id = ussId
      cssNode.`type` = "text/css"
      cssNode.rel = "stylesheet"
      cssNode.href = route
      headSection.appendChild(cssNode)
    }
  }

  def playerInfo(up: UltiPlayer, innerUlti: ClientInnerUlti)(implicit players: List[FrontendPlayer]): TagOf[Div] = {
    val name2 = players.find(_.role.contains(up.roleId)).map(_.name).getOrElse(FrontendPlayer.MISSING_PLAYERS_NAME)
    val bold = innerUlti.currentPlayer == up
    val fw = if (bold) "underline" else "none"
    val playersTricks = innerUlti.tricks.filter(_.taker == up)
    val scoreText: String = {
      val numbers = {
        val calculatedFromDM = {
          if (!innerUlti.declaredMarriages.keys.exists(_ == up))
            List.empty
          else {
            innerUlti.declaredMarriages(up).toList.map(suit => {
              if (innerUlti.trumpSuit.contains(suit)) 40 else 20
            }).sorted
          }
        }
        val theBid = innerUlti.currentBid

        if (theBid.isEmpty) {
          List.empty
        } else {
          theBid.get match {
            case Bid40_100 => {
              if (innerUlti.currentBidWinner.contains(up))
                List(40)
              else
                List.empty
            }
            case Bid20_100 => {
              if (innerUlti.currentBidWinner.contains(up))
                List(20)
              else
                List.empty
            }
            case cb: CombinedBid => {
              if (cb.components.contains(Bid40_100))
                if (innerUlti.currentBidWinner.contains(up))
                  List(40)
                else
                  List.empty
              else if (cb.components.contains(Bid20_100))
                if (innerUlti.currentBidWinner.contains(up))
                  List(20)
                else
                  List.empty
              else
                calculatedFromDM
            }
            case _ => calculatedFromDM
          }
        }
      }
      val totalScore = numbers.sum + innerUlti.pointsScored(up) * 10

      val temp = if (numbers.isEmpty)
        totalScore.toString
      else {
        "(" + numbers.mkString(" + ") + ") + " + innerUlti.pointsScored(up)*10 + " = " + totalScore
      }

      if (innerUlti.phase == GameOverPhase && innerUlti.round == 10 && innerUlti.winnerOfTheLastRound == up)
        temp + " (LAST TRICK TAKEN)"
      else
        temp
    }
    div(
      div(textDecoration:=fw, name2 + " (" + innerUlti.playerHands(up).map(r => r.size).fold(_.toString, _.toString)  + " cards)"),
      Option.when(innerUlti.roundCaller.contains(up))(div("CALLER")),
      Option.when(innerUlti.currentBidWinner.contains(up))(
        if (innerUlti.phase == BiddingPhase) {
          div("Current bid winner.")
        } else {
          div("SOLOIST")
        }
      ),
      Option.when(innerUlti.phase != BiddingPhase)
      (

        div(
          div("Tricks taken: " + playersTricks.size),
          div("Points: " + scoreText)
        )
      )
    )
  }

  def calculateCardCoordinates(centerPoint: (Int,Int), radius: Int, degree: Double, selected: Boolean = false): (Int, Int) = {
    val extendedRadius = if (selected) radius * 1.2 else  radius
    val horizontalDistance: Int = (Math.cos(Math.toRadians(degree)) * extendedRadius).toInt
    val verticalDistance: Int = (Math.sin(Math.toRadians(degree)) * extendedRadius).toInt
    (centerPoint._1 - horizontalDistance, centerPoint._2 + verticalDistance)
  }

  def stuffInTheMiddle(offlineUltiState: OfflineUltiState, ulti: ClientUlti, you: UltiPlayer, commander: ClientEventHandler[OfflineUltiEvent], smallCards: Boolean = false)(implicit players: List[FrontendPlayer]): TagOf[Div] = {
    // bidding Phase
    //   - talon is on the table
    //   - talon is on the table, you may pick it up
    //   - talon is in your hand
    // game Phase
    //   - doubling
    //   - declare a marriage
    //   - declare trump suit
    //   - cards in the middle
    // game over phase

    def trumpSuitSelectorShouldBeDisplayed(innerUlti: ClientInnerUlti, player: UltiPlayer): Boolean = {
      player == innerUlti.currentPlayer &&
        innerUlti.phase == PlayingPhase &&
        innerUlti.round == 1 &&
        innerUlti.currentBidWinner.contains(innerUlti.currentPlayer) &&
        innerUlti.currentBid.exists(_.trumpGame) &&
        innerUlti.trumpSuit.isEmpty
    }

    def marriageScreenShouldBeDisplayed(alreadyDeclared: Boolean, innerUlti: ClientInnerUlti, player: UltiPlayer): Boolean = {
      !alreadyDeclared &&
        innerUlti.trumpSuit.isDefined &&
        player == innerUlti.currentPlayer &&
        innerUlti.phase == PlayingPhase &&
        innerUlti.round == 1 &&
        innerUlti.currentBid.exists(_.canMarriagesBeDeclared) &&
        (!innerUlti.declaredMarriages.keys.exists(_ == player) || innerUlti.declaredMarriages(player).isEmpty) &&
        innerUlti.availableMarriagesForPlayer(player).nonEmpty
    }

    val innerUlti = ulti.innerUlti.get
    val offset = {
      if (smallCards)
        "-35px"
      else
        "50px"
    }
    innerUlti.phase match {
      case BiddingPhase => {
        if (innerUlti.talonOnTheTable) {
          div(
            if (innerUlti.currentPlayer == you) {
              pickUpTalonScreen(innerUlti.currentBidWinner.contains(innerUlti.currentPlayer), commander, offlineUltiState.smallCards)
            } else {
              Directives.talon(offlineUltiState.smallCards)
            },
          )(position:="relative", left:="50%", transform:="translate(-50%)", width:="fit-content")
        } else {
          div("")
        }
      }
      case PlayingPhase => {
          div(
            Option.when(trumpSuitSelectorShouldBeDisplayed(innerUlti, you))
            (Directives.declareTrumpSuitScreen(innerUlti.currentBid.get, commander)(position:="relative", left:="50%", transform:="translate(-50%)", width:="fit-content")),
            Option.when(marriageScreenShouldBeDisplayed(offlineUltiState.marriagesAreDeclared, innerUlti, you))
            (Directives.declareMarriagesScreen(offlineUltiState.proposedMarriages, innerUlti, you, commander)
            (position:="relative", left:="50%", transform:="translate(-50%)", width:="fit-content")),
            Directives.cardsInTheMiddle(innerUlti, Some(you), offlineUltiState.smallCards)(position:="absolute", bottom:=offset, left:="50%", transform:="translate(-50%)")
          )
      }
      case GameOverPhase => {
        div(
          div(
            Directives.resultsDirective(innerUlti)(players), br,
            Directives.aggregatedResultsDirective(ulti)(players),br,
            if (ulti.readyForNextRound(you)) {
              div("You are ready... :-)", textAlign:="center")
            } else {
              button(`class`:="btn btn-primary", "READY", onClick --> Callback {
                commander.addAnEventToTheEventQueue(ReadyEvent)
              })(position:="relative", left:="50%", transform:="translate(-50%)")
            }
          )(position:="absolute", left:="50%", transform:="translate(-50%)", top:="50px", animation:="delayed_appear 1s forwards"),
          Directives.cardsInTheMiddle(innerUlti, Some(you), offlineUltiState.smallCards)
          (position:="absolute", bottom:=offset, left:="50%", transform:="translate(-50%)")
        )
      }
    }
  }

  def animatedCardsInTheMiddle(cards: List[Card], animationDirection: Option[Int] = None, smallCards: Boolean = false): TagOf[Div] = {
    val x = if (smallCards) -(178/4) else -(178/2)
    val y = if (smallCards) 125 else 250
    val customRadius = if (smallCards) 150 else 300

    // The z index has to be really low, so the invisible cards won't potentially overlap with cards in the hand.
    // In that situation the invisible cards would steal the click event from the actual cards.
    val base = div(position:="absolute", bottom:="0px", left:="0px", zIndex:="-999",
      Directives.cardHand(cards.map((_, false)), -90, (x, y), radius = customRadius,commander = null),
    )
    if (animationDirection.isEmpty) {
      base
    } else {
      val t = animationDirection.get
      t match {
        case 1 => base(animation:="player1 1s forwards")
        case 2 => base(animation:="player2 1s forwards")
        case 3 => base(animation:="player3 1s forwards")
        case _ => base
      }
    }
  }

  def cardHand(cards: List[(Card, Boolean)], borderDegree: Int = -90, centerPoint: (Int, Int) = (960 - (178/2), -100), radius: Int = 300, commander: ClientEventHandler[CardClicked]): TagMod = {
    val customCardWidth: Int = (178.toDouble / (300.toDouble/radius.toDouble)).toInt

    val step: Double = 15
    val startingDegree = (180 - ((cards.size - 1) * step)) / 2
    cards.zipWithIndex.map(tuple => {
      val card = tuple._1
      val index = tuple._2
      val rotationNumber = index * step + startingDegree + borderDegree + 90
      val (x,y) = calculateCardCoordinates(centerPoint, radius, rotationNumber, card._2)
      (cardPicture(Some(card._1), customCardWidth)
      (position:="absolute", bottom:=y+"px", left:=x+"px", transform:= "rotate(" + (rotationNumber - 90) + "deg)",
        onClick --> Callback {
          if (commander != null)
            commander.addAnEventToTheEventQueue(CardClicked(card._1))
        }
      ))
    }).toTagMod
  }

  def cardHandWithBacks(numberOfCards: Int, borderDegree: Int = -90, centerPoint: (Int, Int) = (600 - (178/2), -100), radius: Int = 300): TagMod = {
    val customCardWidth: Int = (178.toDouble / (300.toDouble/radius.toDouble)).toInt

    val step: Double = 15
    val startingDegree = (180 - ((numberOfCards-1) * step)) / 2
    (0 until numberOfCards).map(number => {
      val rotationNumber = number * step + startingDegree + borderDegree + 90
      val (x,y) = calculateCardCoordinates(centerPoint, radius, rotationNumber)
      cardPicture(None, customCardWidth)(bottom:=y+"px", position:="absolute", left:=x+"px", transform:= "rotate(" + (rotationNumber - 90) + "deg)")
    }).toTagMod
  }

  def cardsOnTheTable(cards: List[Card]): TagOf[Div] = {
    div(
      cardHand(cards.map((_, false)), -90, (960 - (178/2), 250), commander = null)
    )
  }

  def talon(small: Boolean = false): TagOf[Div] = {
    val offset = if (small) 32 else 65
    val cardWidth = if (small) 89 else 178
    val calculatedWidth = cardWidth + offset
    div(position:="relative", width:= calculatedWidth + "px",
      cardPicture(None, cardWidth),
      cardPicture(None, cardWidth)(position:="absolute", left:=offset + "px")
    )
  }

  def cardsInTheMiddle(innerUlti: ClientInnerUlti, you: Option[UltiPlayer] = None, smallCards: Boolean = false): TagOf[Div] = {
    val theCards: List[Card] = {
      if (innerUlti.cardsOnTheTable.nonEmpty) {
        innerUlti.cardsOnTheTable
      } else if (innerUlti.lastTrick.exists(_.cards.isDefined)) {
        innerUlti.lastTrick.get.cards.get
      } else {
        List.empty
      }
    }
    if (theCards.size == 3) {
      val direction: Option[Int] = {
        val surplus: Int = you match {
          case None => 0
          case Some(UltiPlayer1) => 0
          case Some(UltiPlayer2) => 2
          case Some(UltiPlayer3) => 1
        }
        innerUlti.tricks.last.taker match {
          case UltiPlayer1 => Some(((0 + surplus) % 3) + 1)
          case UltiPlayer2 => Some(((1 + surplus) % 3) + 1)
          case UltiPlayer3 => Some(((2 + surplus) % 3) + 1)
        }
      }
      Directives.animatedCardsInTheMiddle(theCards, direction, smallCards = smallCards)
    } else
      Directives.animatedCardsInTheMiddle(theCards, smallCards = smallCards)
  }

  private def cardPicture(card: Option[Card], cardWidth: Int = 178): TagOf[Image] = {
    //Default size: 178*295
    val cardHeight = 295 / (178 / cardWidth.toDouble)
    val path = if (card.isEmpty)
      "./games/ulti/card_pics/cover.png"
    else
      "./games/ulti/card_pics/" + card.get.name + ".jpg"
    img(src:=path, width:=cardWidth + "px", height:=cardHeight + "px", borderRadius:="20px")
  }

  def gameInfoOnTheLeft(innerUlti: ClientInnerUlti)(implicit players: List[FrontendPlayer]): TagOf[Div] = {
    div(
      Option.when(innerUlti.phase == PlayingPhase)
      (div(
        div("Contracts of the bid winner:"),
        ul(
          innerUlti.contractsOfTheBidWinner.filterNot(_.contract.silent).map(contractWithStatus => {
            li(
              contractWithStatus.contract.toString +
              {if (contractWithStatus.contraLevel == NoContra) "" else " - " + contractWithStatus.contraLevel.displayText(players)}
            )
          }).toTagMod
        )
      )),
      div(innerUlti.bidStatusText()(players))
    )
  }

  def gameInfo(innerUlti: ClientInnerUlti, gameNumber: Int = 1): TagOf[Div] = {
    innerUlti.phase match {
      case BiddingPhase => {
        div(
          div(s"Game no. $gameNumber"),
          div("Bidding phase"),
          div("Round " + innerUlti.round)
        )
      }
      case PlayingPhase => {
        div(
          div(s"Game no. $gameNumber"),
          div("Game is in progress"),
          div("Round " + innerUlti.round),
          div(trumpSuitText(innerUlti.trumpSuit))
        )
      }
      case GameOverPhase => {
        div(s"Game no. $gameNumber is finished.")
      }
    }
  }

  def bidPropositionScreen(bp: BidProposition, currentBidWinnerAndBid: Option[(UltiPlayer, Bid)] = None, commander: ClientEventHandler[BidPropositionEvent], smallCards: Boolean = false)(implicit players: List[FrontendPlayer]): TagOf[Div] = {
    def generateTextDecorationForProposition(proposition: Proposition): TagMod = {
      val selected = List(textDecoration:="underline").toTagMod
      val unavailable = List(textDecoration:="line-through").toTagMod
      val simple = List(textDecoration:="inherit").toTagMod

      if (bp.listOfPropositions.contains(proposition)) {
        selected
      } else if (bp.listOfPropositions.exists(_.incompatibleWith(proposition))) {
        unavailable
      } else {
        simple
      }
    }

    val childStyle = List(
      paddingLeft:="10px",
      paddingRight:="10px"
    ).toTagMod

    val currentBidText: String = bp.generatedBid.map(_.stringWithValue).getOrElse("Specify your bid!")
    val currentWinningBidText: String = currentBidWinnerAndBid match {
      case None => "You are the first to bid!"
      case Some((ultiPlayer, bid)) => "Current winning bid: " + bid.stringWithValue + " [" + FrontendPlayer.getNameOfPlayerWithRole(ultiPlayer.roleId) + "]"
    }

    val fs = if (smallCards) "initial" else "x-large"

    div(
      div(display:="flex", flexDirection:="row", alignItems:="center", border:="solid 1px", fontSize:=fs, VdomStyle("userSelect"):="none",
        div(
          div("Red(2x)", onClick --> Callback {
            commander.addAnEventToTheEventQueue(BidPropositionEvent(RedProposition))
          })(generateTextDecorationForProposition(RedProposition))
        )(childStyle),
        div(
          div("Simple(1)", onClick --> Callback {
            commander.addAnEventToTheEventQueue(BidPropositionEvent(SimpleProposition))
          })(generateTextDecorationForProposition(SimpleProposition)),
          div("20-100(6)", onClick --> Callback {
            commander.addAnEventToTheEventQueue(BidPropositionEvent(Proposition20_100))
          })(generateTextDecorationForProposition(Proposition20_100)),
          div("40-100(4)", onClick --> Callback {
            commander.addAnEventToTheEventQueue(BidPropositionEvent(Proposition40_100))
          })(generateTextDecorationForProposition(Proposition40_100))
        )(childStyle),
        div(
          div("Ulti(4+1)", onClick --> Callback {
            commander.addAnEventToTheEventQueue(BidPropositionEvent(UltiProposition))
          })(generateTextDecorationForProposition(UltiProposition))
        )(childStyle),
        div(
          div("Betli(5)", onClick --> Callback {
            commander.addAnEventToTheEventQueue(BidPropositionEvent(BetliProposition))
          })(generateTextDecorationForProposition(BetliProposition)),
          div("Open Betli(10)", onClick --> Callback {
            commander.addAnEventToTheEventQueue(BidPropositionEvent(OpenBetliProposition))
          })(generateTextDecorationForProposition(OpenBetliProposition))
        )(childStyle),
        div(
        div("Durchmars(7)", onClick --> Callback {
          commander.addAnEventToTheEventQueue(BidPropositionEvent(DurchmarsProposition))
        })(generateTextDecorationForProposition(DurchmarsProposition)),
        div("Open Durchmars(14)", onClick --> Callback {
          commander.addAnEventToTheEventQueue(BidPropositionEvent(OpenDurchmarsProposition))
        })(generateTextDecorationForProposition(OpenDurchmarsProposition)),
        div("Plain Durchmars(14)", onClick --> Callback {
          commander.addAnEventToTheEventQueue(BidPropositionEvent(PlainDurchmarsProposition))
        })(generateTextDecorationForProposition(PlainDurchmarsProposition)),
        div("Open Plain Durchmars(28)", onClick --> Callback {
          commander.addAnEventToTheEventQueue(BidPropositionEvent(PlainAndOpenDurchmarsProposition))
        })(generateTextDecorationForProposition(PlainAndOpenDurchmarsProposition))
      )(childStyle),
      ),
      div(currentBidText),
      div(currentWinningBidText)
    )
  }

  def submitBidButton(
                       offlineUltiState: OfflineUltiState,
                       currentWinningBid: Option[Bid],
                       calculatedBid: Option[Bid],
                       commander: ClientEventHandler[SubmitBidEvent.type]): TagOf[Button] = {

    val buttonEnabled: Boolean = offlineUltiState.selectedCards.size == 2 &&
      (calculatedBid.isDefined &&
        (currentWinningBid.isEmpty || calculatedBid.get > currentWinningBid.get))

    if (currentWinningBid.isDefined)
      println("value of the current winning bid: " + currentWinningBid.get.value)

    if (calculatedBid.isDefined)
      println("value of the calculated bid: " + calculatedBid.get.value)

    button(`class`:="btn btn-primary", "SUBMIT BID",
      onClick --> Callback {
        commander.addAnEventToTheEventQueue(SubmitBidEvent)
      },
      disabled:=(!buttonEnabled)
    )
  }

  def pickUpTalonScreen(startPlayingPhase: Boolean, commander: ClientEventHandler[OfflineUltiEvent], smallCards: Boolean = false): TagOf[Div] = {
    val skipText = if (startPlayingPhase) "Start Game" else "Skip"
    div(display:="flex", flexDirection:="row", alignItems:="center",
      button(`class`:="btn btn-primary", "Pick up Talon", width:="121px", position:="relative", top:="-25px",
        onClick --> Callback {
          commander.addAnEventToTheEventQueue(PickUpTalonEvent)
        }
      ),
      Directives.talon(smallCards),
      button(`class`:="btn btn-primary", skipText, width:="121px", position:="relative", top:="-25px",
        onClick --> Callback {
          commander.addAnEventToTheEventQueue(SkipBidEvent)
        }
      )
    )
  }

  def declareTrumpSuitScreen(bid: Bid, commander: ClientEventHandler[OfflineUltiEvent]): TagOf[Div] = {
    div(marginTop:="25px",
      div("Specify the trump suit!", textAlign:="center"),
      div(display:="grid", gridTemplateRows:="55px 55px", gridTemplateColumns:="150px 150px", justifyItems:="center", alignItems:="center",
        button(`class`:="btn btn-primary", "Hearts", height:="fit-content", width:="80px",
          onClick --> Callback {
            commander.addAnEventToTheEventQueue(DeclareTrumpSuitEvent(Hearts))
          }
        ),
        button(`class`:="btn btn-primary", "Leaves", height:="fit-content", width:="80px",
          onClick --> Callback {
            commander.addAnEventToTheEventQueue(DeclareTrumpSuitEvent(Leaves))
          }
        ),
        button(`class`:="btn btn-primary", "Bells", height:="fit-content", width:="80px",
          onClick --> Callback {
            commander.addAnEventToTheEventQueue(DeclareTrumpSuitEvent(Bells))
          }
        ),
        button(`class`:="btn btn-primary", "Acorns", height:="fit-content", width:="80px",
        onClick --> Callback {
          commander.addAnEventToTheEventQueue(DeclareTrumpSuitEvent(Acorns))
        }
      ),
      ),
      Option.when(bid.canBeUpgradedToPlain)
      (button(`class`:="btn btn-primary", "Plain", height:="fit-content", width:="80px", textAlign:="center", marginTop:="15px",
        onClick --> Callback {
          commander.addAnEventToTheEventQueue(DeclareTrumpSuitEvent(Plain))
        }, position:="relative", left:="50%", transform:="translate(-50%)"
      ))
    )
  }

  def declareMarriagesScreen(proposedMarriages: Set[CardSuit], innerUlti: ClientInnerUlti, player: UltiPlayer, commander: ClientEventHandler[OfflineUltiEvent]): TagOf[Div] = {
    val colors: Set[CardSuit] = innerUlti.availableMarriagesForPlayer(player)
    val trumpSuit = innerUlti.trumpSuit.get

    def callbackForSuit(suit: CardSuit): ReactEventFrom[Input] => Callback = event => Callback {
      val x = event.target.checked
      println("the suit is: " + suit)
      println("is the checkbox checked? " + event.target.checked)
      commander.addAnEventToTheEventQueue(AddOrRemoveMarriageEvent(suit))
    }

    div(
      Option.when(colors.contains(trumpSuit))
        (div(
          input.checkbox(id:="trump", checked:=proposedMarriages.contains(trumpSuit), onChange ==> callbackForSuit(trumpSuit)),
          label(`for`:="trump", " " + trumpSuit + " - 40")
        )),
      Option.when(colors.contains(Hearts) && trumpSuit != Hearts)
        (div(
          input.checkbox(id:="hearts", checked:=proposedMarriages.contains(Hearts), onChange ==> callbackForSuit(Hearts)),
          label(`for`:="hearts", "Hearts - 20")
        )),
      Option.when(colors.contains(Leaves) && trumpSuit != Leaves)
        (div(
          input.checkbox(id:="leaves", checked:=proposedMarriages.contains(Leaves), onChange ==> callbackForSuit(Leaves)),
          label(`for`:="leaves", "Leaves - 20")
        )),
      Option.when(colors.contains(Acorns) && trumpSuit != Acorns)
        (div(
          input.checkbox(id:="acorns", checked:=proposedMarriages.contains(Acorns), onChange ==> callbackForSuit(Acorns)),
          label(`for`:="acorns", "Acorns - 20")
        )),
      Option.when(colors.contains(Bells) && trumpSuit != Bells)
        (div(
          input.checkbox(id:="bells", checked:=proposedMarriages.contains(Bells), onChange ==> callbackForSuit(Bells)),
          label(`for`:="bells", "Bells - 20")
        )),
      button(`class`:="btn btn-primary", "Declare the selected marriages",
        onClick --> Callback {
          commander.addAnEventToTheEventQueue(DeclareMarriagesEvent)
        }
      )
    )
  }

  def aggregatedResultsDirective(ulti: ClientUlti)(implicit players: List[FrontendPlayer]): TagOf[Div] = {
    div(
      div("Aggregated results:", textAlign:="center", textDecoration:="underline", fontSize:="x-large"),br,
      div(display:="grid", gridTemplateColumns:="33% 33% 33%", alignItems:="center", justifyItems:="center", columnGap:="5px",
        div(FrontendPlayer.getNameOfPlayerWithRole(UltiPlayer1.roleId)),
        div(FrontendPlayer.getNameOfPlayerWithRole(UltiPlayer2.roleId)),
        div(FrontendPlayer.getNameOfPlayerWithRole(UltiPlayer3.roleId)),
        div(ulti.totalScoreOfPlayer(UltiPlayer1)),
        div(ulti.totalScoreOfPlayer(UltiPlayer2)),
        div(ulti.totalScoreOfPlayer(UltiPlayer3)),
      )
    )
  }

  def resultsDirective(innerUlti: ClientInnerUlti)(implicit players: List[FrontendPlayer]): TagOf[Div] = {
    val playersWithContracts: List[UltiPlayer] = innerUlti.contracts.keys.toList
    val x: List[(UltiPlayer, Set[ContractWithStatus])] = playersWithContracts.map(player => (player, innerUlti.contracts(player)))
    val playerContractPairs: List[(UltiPlayer, ContractWithStatus)] = x.flatMap(pair => pair._2.toList.map((pair._1, _)))

    val theList: List[TagOf[Div]] = playerContractPairs.filterNot(_._2.contractResult == Open).flatMap(t => {
      val contractor = FrontendPlayer.getNameOfPlayerWithRole(t._1.roleId)
      val result = t._2.contractResult
      val contract = t._2.contract

      List(
        div(contract + " (" + contractor + ")"),
        div(result.toString),
        div(innerUlti.gainForPlayerFromContract(UltiPlayer1, t._2, UltiPlayer1 == t._1)),
        div(innerUlti.gainForPlayerFromContract(UltiPlayer2, t._2, UltiPlayer2 == t._1)),
        div(innerUlti.gainForPlayerFromContract(UltiPlayer3, t._2, UltiPlayer3 == t._1)),
      )
    })

    div(
      div("Results of this round:", textAlign:="center", textDecoration:="underline", fontSize:="x-large"),br,
      div(display:="grid", gridTemplateColumns:="auto auto auto auto auto", alignItems:="center", justifyItems:="center",
        columnGap:="5px",
        div("-"),
        div("Result"),
        div(FrontendPlayer.getNameOfPlayerWithRole(UltiPlayer1.roleId)),
        div(FrontendPlayer.getNameOfPlayerWithRole(UltiPlayer2.roleId)),
        div(FrontendPlayer.getNameOfPlayerWithRole(UltiPlayer3.roleId)),

        theList.toTagMod,

        div("Total:"),
        div("-"),
        div(innerUlti.totalGainsForPlayer(UltiPlayer1)),
        div(innerUlti.totalGainsForPlayer(UltiPlayer2)),
        div(innerUlti.totalGainsForPlayer(UltiPlayer3)),
      )
    )
  }

  def contractsDisplayer(contracts: List[ContractWithStatus])(implicit players: List[FrontendPlayer]): TagOf[Div] = {
    val nonSilentContracts = contracts.filterNot(_.contract.silent)
    div(fontSize:="x-large",
      ul(
        nonSilentContracts.map(contractWithStatus => {
          val contraLevel = contractWithStatus.contraLevel
          val contract = contractWithStatus.contract
          if (contraLevel == NoContra) {
            li(contract.toString)
          } else {
            li(contract.toString + " - " + contractWithStatus.contraLevel.displayText(players))
          }
        }).toTagMod
      )
    )
  }

  def doublingDirective(currentPlayer: UltiPlayer, contractsToDouble: List[ContractWithStatus], commander: ClientEventHandler[DoubleContractEvent]): TagOf[Div] = {
    div(display:="flex", flexDirection:="column",
      contractsToDouble.map(contractWithStatus => {
        val contract = contractWithStatus.contract
        val contraLevel = contractWithStatus.contraLevel
        val doublingText = ContraLevel.levelText(contraLevel.levelForPlayer(currentPlayer)+1)
        val contractText = contract.toString
        button(`class`:="btn btn-primary", doublingText + " " + contractText, onClick --> Callback {
          commander.addAnEventToTheEventQueue(DoubleContractEvent(contractWithStatus.contract))
        }, marginBottom:="20px")
      }).toTagMod
    )
  }

  def clientLeftCorner(innerUlti: ClientInnerUlti, commander: ClientEventHandler[SwitchToSubScreen]): TagOf[Div] = {
    innerUlti.phase match {
      case BiddingPhase => div("Bidding Phase")
      case PlayingPhase => {
        div(
          button("BID INFO", `class`:="btn btn-primary", onClick --> Callback {
            commander.addAnEventToTheEventQueue(SwitchToSubScreen(Some(ContractInfoScreen)))
          }),
          div(trumpSuitText(innerUlti.trumpSuit))
        )
      }
      case GameOverPhase => div()
    }
  }
}
