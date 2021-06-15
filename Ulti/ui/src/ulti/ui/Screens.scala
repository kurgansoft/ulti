package ulti.ui

import gbge.client.{ClientEvent, ClientEventHandler}
import gbge.shared._
import gbge.ui.eps.player.ClientState
import gbge.ui.eps.spectator.SpectatorState
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react.vdom.TagOf
import org.scalajs.dom.html.{Div, Image}
import ulti.shared.abstract0._
import ulti.shared.client.ClientUlti

object Screens {

  def spectatorScreen(spectatorState: SpectatorState, commander: ClientEventHandler[ClientEvent]): TagOf[Div] = {
    Directives.loadExternalCss()
    if (spectatorState.offlineState.isInstanceOf[OfflineUltiSpectatorState]) {
      val offlineUltiSpectatorState = spectatorState.offlineState.asInstanceOf[OfflineUltiSpectatorState]

      assert(spectatorState.frontendUniverse.isDefined)
      val fu = spectatorState.frontendUniverse.get
      assert(fu.game.get.isInstanceOf[ClientUlti])
      val clientUlti = fu.game.get.asInstanceOf[ClientUlti]

      clientUlti.state match {
        case NOT_STARTED => {
          div(
            h1("Ulti", textAlign:="center", color:="yellow", marginTop:="50px"),
            gbge.ui.display.GeneralDirectives.generalRoleDisplayer(spectatorState.frontendUniverse.get.players, clientUlti)(marginTop:="100px", fontSize:="xx-large"),
          )
        }
        case IN_PROGRESS => {
          if (clientUlti.innerUlti.isDefined) {
            val h = offlineUltiSpectatorState.height
            val w = offlineUltiSpectatorState.width
            val theRectangle: (Int, Int) = {
              val ratio: Double = 1900.toDouble/1050.toDouble
              if (h*ratio > w) {
                val wi = Math.min(1900, w)
                (wi, (wi/ratio).toInt)
              } else {
                val he = Math.min(1050, h)
                ((he*ratio).toInt, he)
              }
//              (1900, 1050)
            }
            val scaleFactor: Double = theRectangle._1.toDouble / 1900
            val translatePercentage: Double = -50 / scaleFactor
            val bottomOffset: Double = - (1050 / scaleFactor - 1050) / 4
            div(
              table(clientUlti, offlineUltiSpectatorState.width, offlineUltiSpectatorState.height)(fu.players)
              (position:="absolute", left:="50%", transform:="scale(" + scaleFactor + ") translate(" + translatePercentage + "%)", bottom:=bottomOffset + "px"),
              otherStuff(clientUlti)(fu.players)
            )
          } else {
            div(color:="yellow", "This must be an intermediate state.")
          }
        }
      }
    } else {
      div()
    }
  }

  def playerScreen(clientState: ClientState, commander: ClientEventHandler[ClientEvent]): TagOf[Div] = {
    Directives.loadExternalCss()
    assert(clientState.frontendUniverse.isDefined)
    val fu = clientState.frontendUniverse.get
    assert(fu.game.isDefined)
    val ulti = fu.game.get.asInstanceOf[ClientUlti]
    assert(clientState.offlineState.isInstanceOf[OfflineUltiState])
    val offlineUltiState = clientState.offlineState.asInstanceOf[OfflineUltiState]

    if (ulti.state == NOT_STARTED) {
      div(color:="yellow",
        div(s"Hi ${clientState.you.get.name}!", textAlign:="center"),
        gbge.ui.display.GeneralDirectives.generalRoleChooserScreen(clientState.you.get, fu.players, fu.game.get, commander),br,br,
        Option.when(clientState.you.exists(_.isAdmin))
        (button(`class`:="btn btn-primary", "LAUNCH GAME", onClick --> Callback {
          commander.addAnEventToTheEventQueue(ClientInit)
        })(position:="relative", left:="50%", transform:="translate(-50%"))
      )
    }
    else {
      val innerUlti = ulti.innerUlti.get
      val player = clientState.you.get
      val yourName = player.name
      val ultiPlayer: Option[UltiPlayer] = player.role.flatMap(UltiPlayer.getUltiPlayerFromRole)
      if (ultiPlayer.isDefined) {
        val playerToTheRight: UltiPlayer = ultiPlayer.get.nextPlayer
        val playerToTheLeft: UltiPlayer = ultiPlayer.get.nextPlayer.nextPlayer
        offlineUltiState.currentSubScreen match {
          case Some(LeftPlayerPerspectiveScreen) => {
            SubScreens.foreignPlayerPerspective(innerUlti, playerToTheLeft, commander, offlineUltiState.smallCards)(fu.players)
          }
          case Some(RightPlayerPerspectiveScreen) => {
            SubScreens.foreignPlayerPerspective(innerUlti, playerToTheRight, commander, offlineUltiState.smallCards)(fu.players)
          }
          case Some(ContractInfoScreen) => {
            SubScreens.contractsInfoSubScreen(innerUlti, ultiPlayer.get, commander)(fu.players)
          }
          case Some(BidSelectionScreen) => {
            div(color:="yellow",
              Directives.bidPropositionScreen(offlineUltiState.bidProposition, innerUlti.winningBidTuple, commander, true)(fu.players)
              (position:="relative", left:="50%", transform:="translate(-50%)", width:="600px", marginTop:="50px", marginBottom:="50px"),br,br,
              button(`class`:="btn btn-primary", "BACK TO THE MAIN SCREEN", onClick --> Callback {
                commander.addAnEventToTheEventQueue(SwitchToSubScreen(None))
              })(position:="relative", left:="50%", transform:="translate(-50%)")
            )
          }
          case Some(DoublingScreen) => {
            div(color:="yellow",
              Directives.doublingDirective(innerUlti.currentPlayer, innerUlti.contractsThatCanBeDoubledByPlayer(ultiPlayer.get), commander),br,br,
              button("BACK TO THE MAIN SCREEN", `class`:="btn btn-primary", onClick --> Callback {
                commander.addAnEventToTheEventQueue(SwitchToSubScreen(None))
              }),
              Option.when(innerUlti.blockIsLifted)
              (div(FrontendPlayer.getNameOfPlayerWithRole(innerUlti.currentBidWinner.get.roleId)(fu.players) + " does not wish to further redouble."))
            )(width:="fit-content", position:="relative", left:="50%", top:="30px", transform:="translate(-50%)")
          }
          case None => {
            val sortedCards = innerUlti.getSortedPlayerHand(ultiPlayer.get).getOrElse(List.empty)
            val scw = sortedCards.map(card => {
              if (offlineUltiState.selectedCards.contains(card))
                (card, true)
              else
                (card, false)
            })
            val cardHandCenterPoint: (Int, Int) = if (offlineUltiState.smallCards) {
              (-(178 / 4), -50)
            } else {
              (-(178 / 2), -50)
            }
            val offset = {
              if (offlineUltiState.smallCards)
                "-25px"
              else
                "0px"
            }
            val customRadius = if (offlineUltiState.smallCards) 150 else 300
            val cardHandAnimation = if (innerUlti.phase == GameOverPhase) "delayed_disappear 1s forwards" else ""
            div(color:="yellow",
              div(
                Directives.cardHand(scw, commander = commander, radius = customRadius, centerPoint = cardHandCenterPoint),
                position:="absolute", left:="50%", bottom:=offset, transform:="translate(-50%)", animation:=cardHandAnimation
              ),
              Option.when(innerUlti.phase == BiddingPhase && !(innerUlti.playerHands(innerUlti.currentPlayer).map(_.size).getOrElse(0) == 12))
              ({
                val verticalOffset = if (offlineUltiState.smallCards) "220px" else "555px"
                div(innerUlti.bidStatusText()(fu.players), position:="absolute", bottom:=verticalOffset, left:="50%", transform:="translate(-50%)")
              }),
              Option.when(innerUlti.phase == PlayingPhase && innerUlti.round == 1 &&
                !innerUlti.currentBidWinner.contains(ultiPlayer.get) &&
                (!innerUlti.blockIsLifted && innerUlti.contractsThatCanBeDoubledByPlayer(innerUlti.currentBidWinner.get).nonEmpty)
              )
              ({
                val verticalOffset = if (offlineUltiState.smallCards) "220px" else "555px"
                div("Wating for " + FrontendPlayer.getNameOfPlayerWithRole(innerUlti.currentBidWinner.get.roleId)(fu.players) + " to redouble or skip.",
                  position:="absolute", bottom:=verticalOffset, left:="50%", transform:="translate(-50%)")
              }),
              Option.when(offlineUltiState.offlineErrorMessage.nonEmpty)
              ({
                val verticalOffset = if (offlineUltiState.smallCards) "220px" else "555px"
                div(color:="red", offlineUltiState.offlineErrorMessage.get,
                  position:="absolute", bottom:=verticalOffset, left:="50%", transform:="translate(-50%)")
              }),
              Directives.stuffInTheMiddle(offlineUltiState, ulti, ultiPlayer.get, commander, offlineUltiState.smallCards)(clientState.frontendUniverse.get.players),
              Option.when(ultiPlayer.contains(innerUlti.currentPlayer) && innerUlti.phase == BiddingPhase && !innerUlti.talonOnTheTable)(
                div(position:="relative",
                  if (offlineUltiState.separateBidScreen) {
                    div(
                      button("TAP to specify your bid!", `class`:="btn btn-primary", position:="relative", left:="50%", transform:="translate(-50%)", onClick --> Callback {
                        commander.addAnEventToTheEventQueue(SwitchToSubScreen(Some(BidSelectionScreen)))
                      }),
                      div(offlineUltiState.bidProposition.generatedBid.map(_.stringWithValue).getOrElse("Specify your bid!").toString, textAlign:="center")
                    )
                  } else {
                    div(
                      Directives.bidPropositionScreen(offlineUltiState.bidProposition, innerUlti.winningBidTuple, commander, offlineUltiState.smallCards)(fu.players)
                      (position:= "absolute", left:="50%", transform:="translate(-50%)", top:="200px", width:="830px")
                    )
                  },
                  div(
                    Directives.submitBidButton(offlineUltiState, innerUlti.currentBid, offlineUltiState.bidProposition.generatedBid, commander)
                    (position:="relative", left:="50%", transform:="translate(-50%)")
                  )
                )
              ),
              div(position:="absolute", bottom:="0px", right:="5px", fontSize:="xx-large", yourName, onClick --> Callback {
                val theBody = scala.scalajs.js.Dynamic.global.document.body
                theBody.requestFullscreen()
                scala.scalajs.js.Dynamic.global.window.screen.orientation.lock("landscape")
              }),
              Directives.clientLeftCorner(innerUlti, commander)(position:="absolute", bottom:="0px", left:="5px"),
              Option.when(innerUlti.currentPlayer == ultiPlayer.get && innerUlti.phase != GameOverPhase)
              (
                div(
                  if (innerUlti.phase == PlayingPhase && offlineUltiState.selectedCards.size == 1 && !offlineUltiState.submitCardsWithSingleClick)
                    button("PLAY CARD!", `class`:="btn btn-primary", onClick --> Callback {
                      commander.addAnEventToTheEventQueue(PlaySelectedCard)
                    })
                  else
                    if (offlineUltiState.smallCards) {
                      img(src:="./games/ulti/lightbulb_small.png")
                    } else {
                      img(src:="./games/ulti/lightbulb.png")
                    }
                )(position:="absolute", bottom:="0px", left:="50%", transform:="translate(-50%)")
              ),
              Option.when(innerUlti.contractsThatCanBeDoubledByPlayer(ultiPlayer.get).nonEmpty && !(innerUlti.currentBidWinner.contains(ultiPlayer.get)
                && innerUlti.blockIsLifted))
              ({
                if (innerUlti.currentBidWinner.contains(ultiPlayer.get)) {
                  div(
                    Directives.doublingDirective(innerUlti.currentPlayer, innerUlti.contractsThatCanBeDoubledByPlayer(ultiPlayer.get), commander), br,
                    button(`class`:="btn btn-primary", "DONE WITH DOUBLING", onClick --> Callback {
                      commander.addAnEventToTheEventQueue(DoneWithDoublingEvent)
                    })
                  )(position:="absolute", width:="fit-content", bottom:="50%", left:="50%", transform:="translate(-50%,-50%)")
                } else {
                  val bottomOffset = if (offlineUltiState.smallCards) "100px" else "400px"
                  val rightOffset = if (offlineUltiState.smallCards) "15px" else "50px"
                  div(
                    button("DOUBLE", `class`:="btn btn-primary", onClick --> Callback {
                      commander.addAnEventToTheEventQueue(SwitchToSubScreen(Some(DoublingScreen)))
                    } )
                    (position:="absolute", bottom:=bottomOffset, right:=rightOffset)
                  )
                }
              }),
              div(
                Option.when(innerUlti.phase == PlayingPhase && innerUlti.round > 1 && innerUlti.currentBid.exists(_.open))
                (button(`class`:="btn btn-primary", "SHOW PERSPECTIVE", onClick --> Callback {
                  commander.addAnEventToTheEventQueue(SwitchToSubScreen(Some(RightPlayerPerspectiveScreen)))
                })),
                Directives.playerInfo(playerToTheRight, innerUlti)(fu.players)
              )(position:="absolute", top:="100px", right:="50px", border:="solid 1px"),
              div(
                Option.when(innerUlti.phase == PlayingPhase && innerUlti.round > 1 && innerUlti.currentBid.exists(_.open))
                (button(`class` := "btn btn-primary", "SHOW PERSPECTIVE", onClick --> Callback {
                  commander.addAnEventToTheEventQueue(SwitchToSubScreen(Some(LeftPlayerPerspectiveScreen)))
                })),
                Directives.playerInfo(playerToTheLeft, innerUlti)(fu.players)
              )(position:="absolute", top:="100px", left:="50px", border:="solid 1px")
            )
          }
        }
      } else {
        div(color:="yellow", "You are not part of this game.")
      }
    }
  }

  def calculateCardCoordinates(degree: Double): (Int, Int) = {
    val centerPoint: (Int, Int) = (600 - (178/2), -100)
    val radius: Int = 300

    val horizontalDistance: Int= (Math.cos(Math.toRadians(degree)) * radius).toInt
    val verticalDistance: Int = (Math.sin(Math.toRadians(degree)) * radius).toInt

    val x: Int = centerPoint._1 - horizontalDistance
    val y: Int = centerPoint._2 + verticalDistance

    (x,y)
  }

  def table(ulti: ClientUlti, w: Int, h: Int)(implicit players: List[FrontendPlayer]): TagOf[Div] = {
    val defaultDimensions: (Int, Int) = (1900, 1050)

    val innerUlti = ulti.innerUlti.get
    val horizontalOffset: Int = {
      if (defaultDimensions._1 >= 1800) {
        250
      } else {
        (defaultDimensions._1/7.2).toInt
      }
    }

    val verticalOffsetFor2And3: Int = defaultDimensions._2 / 2 + 100

    val customRadius: Int = {
      if (defaultDimensions._1 >= 1800) {
        300
      } else {
        (defaultDimensions._1.toDouble / 6 ).toInt
      }
    }

    val horizontalOffset2: Int = - (178 / (300/customRadius))
    val cardAnimation = if (ulti.innerUlti.map(_.phase).contains(GameOverPhase)) "delayed_disappear 1s forwards" else ""

    div(color:="yellow",
      div(
        div(
          innerUlti.getSortedPlayerHand(UltiPlayer1) match {
            case Left(number) => Directives.cardHandWithBacks(number, radius = customRadius, centerPoint = (horizontalOffset2/2, -100))
            case Right(cards) => Directives.cardHand(cards.map(card => (card, false)), radius = customRadius, centerPoint = (horizontalOffset2/2, -100), commander = null)
          }, position:="absolute", left:="50%", transform:="translate(-50%)", bottom:="0px", animation:=cardAnimation
        ),
        div(
          innerUlti.getSortedPlayerHand(UltiPlayer2) match {
            case Left(number) => Directives.cardHandWithBacks(number, radius = customRadius, borderDegree = 135, centerPoint = (horizontalOffset2/2, 0))
            case Right(cards) => Directives.cardHand(cards.map(card => (card, false)), radius = customRadius, borderDegree = 135, centerPoint = (horizontalOffset2/2, 0), commander = null)
          }, position:="absolute", bottom:=verticalOffsetFor2And3 + "px", right:=horizontalOffset + "px", animation:=cardAnimation
        ),
        div(
          innerUlti.getSortedPlayerHand(UltiPlayer3) match {
            case Left(number) => Directives.cardHandWithBacks(number, radius = customRadius, borderDegree = 45, centerPoint = (horizontalOffset2/2, 0))
            case Right(cards) => Directives.cardHand(cards.map(card => (card, false)), radius = customRadius, borderDegree = 45, centerPoint = (horizontalOffset2/2, 0), commander = null)
          }, position:="absolute", bottom:= verticalOffsetFor2And3 + "px", left:=horizontalOffset + "px", animation:=cardAnimation
        ),
        Directives.playerInfo(UltiPlayer1, innerUlti)(players)(position:="absolute", left:="50%", transform:="translate(-50%)", bottom:="40px"),
        Directives.playerInfo(UltiPlayer2, innerUlti)(players)(position:="absolute", left:="1550px", bottom:="700px"),
        Directives.playerInfo(UltiPlayer3, innerUlti)(players)(position:="absolute", left:="250px", bottom:="700px"),
        innerUlti.phase match {
          case BiddingPhase => {
            if (innerUlti.talonOnTheTable)
              Directives.talon()(position:="absolute", left:="50%", transform:="translate(-50%)", top:="80px")
            else
              div()
          }
          case _ => {
            Directives.cardsInTheMiddle(innerUlti)(position:="absolute", left:="50%", transform:="translate(-50%)", bottom:="0px")
          }
        }
      ),
      width:=defaultDimensions._1 + "px",
      height:=defaultDimensions._2 + "px"
    )
  }

  def otherStuff(ulti: ClientUlti)(implicit players: List[FrontendPlayer]): TagOf[Div] = {
    val innerUlti = ulti.innerUlti.get
    div(color:="yellow",
      Directives.gameInfoOnTheLeft(innerUlti)(players)(position:="absolute", left:="20px", bottom:="0px"),
      Directives.gameInfo(innerUlti, ulti.roundNumber)(position:="absolute", right:="20px", bottom:="0px"),
      Option.when(innerUlti.phase == GameOverPhase)
      (div(display:="flex", flexDirection:="column", alignItems:="center",
        Directives.resultsDirective(innerUlti)(players)(border:="solid 1px"),br,
        Directives.aggregatedResultsDirective(ulti)(players)(border:="solid 1px"),br,
        div("Who is ready for the next round?"),br,
        ul(
          li(FrontendPlayer.getNameOfPlayerWithRole(UltiPlayer1.roleId) + {if (ulti.readyForNextRound(UltiPlayer1)) " ✓" else ""}),
          li(FrontendPlayer.getNameOfPlayerWithRole(UltiPlayer2.roleId) + {if (ulti.readyForNextRound(UltiPlayer2)) " ✓" else ""}),
          li(FrontendPlayer.getNameOfPlayerWithRole(UltiPlayer3.roleId) + {if (ulti.readyForNextRound(UltiPlayer3)) " ✓" else ""}),
        )
      )(position:="absolute", left:="50%", transform:="translate(-50%)", top:="50px", animation:="delayed_appear 1s forwards"))
    )
  }
}
