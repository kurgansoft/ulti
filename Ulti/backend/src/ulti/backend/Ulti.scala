package ulti.backend

import gbge.backend.{BackendGame, ExecuteEffect, GeneralFailure, OK, Player, Startable, Universe, UniverseResult}
import gbge.shared.{DecodeCapable, FrontendGame, GameState, IN_PROGRESS, NOT_STARTED}
import gbge.shared.actions.{Action, GameAction, NaturalLink}
import ulti.shared.{DealCards, Init, ReadyForNextRound, RoundScore, UltiAction}
import ulti.shared.abstract0.{AbstractUlti, GameOverPhase, UltiPlayer, UltiPlayer1, UltiPlayer2, UltiPlayer3}
import ulti.shared.client.ClientUlti

import scala.util.Random

object UltiEffects {
  import zio._
  def cardDealEffect(player: UltiPlayer): Universe => UIO[List[Action]] = _ => {
    val seed = Random.nextLong().abs
    ZIO.succeed(List(DealCards(player, seed)))
  }
}
case class Ulti(
                 override val state: GameState = NOT_STARTED,
                 override val innerUlti: Option[InnerUlti] = None,
                 override val scores: List[RoundScore] = List.empty,
                 override val readyForNextRound: Map[UltiPlayer, Boolean] = Map(UltiPlayer1 -> false, UltiPlayer2 -> false, UltiPlayer3 -> false)
               ) extends AbstractUlti(state, innerUlti) with BackendGame[ClientUlti] {
  override val noOfPlayers: Int = 3

  override def reduce(gameAction: GameAction, invoker: Option[Player]): (BackendGame[ClientUlti], UniverseResult) = {
    if (gameAction.isInstanceOf[UltiAction]) {
      val ultiAction = gameAction.asInstanceOf[UltiAction]
      reduce0(ultiAction, invoker.flatMap(_.role), invoker.exists(_.isAdmin))
    } else {
      (this, GeneralFailure("Improper action"))
    }
  }

  def reduce0(ultiAction: UltiAction, invokerRole: Option[Int], isAdmin: Boolean = false): (Ulti, UniverseResult) = {
    ultiAction match {
      case Init => {
        (copy(state = IN_PROGRESS), ExecuteEffect(UltiEffects.cardDealEffect(UltiPlayer1)))
      }
      case DealCards(ultiPlayer, seed) => {
        val inner = InnerUlti.generate(ultiPlayer, seed)
        (this.copy(innerUlti = Some(inner)), OK)
      }
      case ReadyForNextRound => {
        val invoker: Option[UltiPlayer] = invokerRole.flatMap(innerUlti.get.getUltiPlayerFromRoleId(_))
        if (invoker.isEmpty)
          (this, GeneralFailure("No role belongs to you."))
        else {
          val ultiPlayer = invoker.get
          if (innerUlti.isEmpty || innerUlti.get.phase != GameOverPhase) {
            (this, GeneralFailure("not applicable"))
          } else {
            if (!readyForNextRound(ultiPlayer)) {
              val temp = this.copy(readyForNextRound = for ((k, v) <- readyForNextRound) yield
                if (k == ultiPlayer) (k, true) else (k, v))
              if (temp.readyForNextRound.values.forall(_ == true)) {
                val starterPlayer = innerUlti.get.currentBidWinner.get.nextPlayer
                (copy(readyForNextRound = Map(UltiPlayer1 -> false, UltiPlayer2 -> false, UltiPlayer3 -> false)),
                  ExecuteEffect(UltiEffects.cardDealEffect(starterPlayer))
                )
              } else {
                (temp, OK)
              }
            } else {
              (this, GeneralFailure("You have already signaled your readiness."))
            }
          }
        }
      }
      case iua => {
        if (state == NOT_STARTED) {
          (this, GeneralFailure("Inner game is not started yet."))
        } else {
          assert(innerUlti.isDefined)
          val invoker: Option[UltiPlayer] = invokerRole.flatMap(innerUlti.get.getUltiPlayerFromRoleId(_))
          val (newInnerUlti, result) = innerUlti.get.reduce(iua, invoker.get)

          if (innerUlti.get.phase != GameOverPhase && newInnerUlti.phase == GameOverPhase) {
            val rs = RoundScore(
              newInnerUlti.totalGainsForPlayer(UltiPlayer1),
              newInnerUlti.totalGainsForPlayer(UltiPlayer2),
              newInnerUlti.totalGainsForPlayer(UltiPlayer3)
            )
            (this.copy(innerUlti = Some(newInnerUlti), scores = scores.appended(rs)), result)
          } else {
            (this.copy(innerUlti = Some(newInnerUlti)), result)
          }
        }
      }
    }
  }

  override def toFrontendGame(role: Option[Int]): ClientUlti = {
    val ultiPlayer: Option[UltiPlayer] = role.flatMap(role => UltiPlayer.getUltiPlayerFromRole(role))
    ClientUlti(state, innerUlti.map(_.toClientInnerUlti(ultiPlayer)), scores, readyForNextRound)
  }
}

object Ulti extends Startable {
  override def start(noOfPlayers: Int): (BackendGame[_ <: FrontendGame[_ <: GameAction]], Option[Action]) = {
    (Ulti(), Some(NaturalLink((1 to 3).toList)))
  }

  override val frontendGame: DecodeCapable = ClientUlti
  override val name: String = "Ulti"
}
