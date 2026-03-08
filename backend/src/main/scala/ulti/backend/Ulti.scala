package ulti.backend

import gbge.backend.models.Player
import gbge.backend.{BackendGame, Failure, GeneralFailure}
import gbge.shared.GameState
import gbge.shared.GameState.*
import gbge.shared.actions.{Action, GameAction}
import ulti.shared.abstract0.*
import ulti.shared.client.ClientUlti
import ulti.shared.*
import zio.{IO, UIO, ZIO}

import scala.language.implicitConversions

object UltiEffects {
  def cardDealEffect(player: UltiPlayer): UIO[Option[Action]] = for {
    seed <- zio.Random.nextLong().map(_.abs)
  } yield Some(DealCards(player, seed))
}

case class Ulti(
                 override val state: GameState = NOT_STARTED,
                 override val innerUlti: Option[InnerUlti] = None,
                 override val scores: List[RoundScore] = List.empty,
                 override val readyForNextRound: Map[UltiPlayer, Boolean] = Map(UltiPlayer1 -> false, UltiPlayer2 -> false, UltiPlayer3 -> false)
               ) extends AbstractUlti(state, innerUlti) with BackendGame[UltiAction, ClientUlti] {
  override val noOfPlayers: Int = 3

  override def reduce(gameAction: GameAction, invoker: Player): Either[Failure, (Ulti, IO[Nothing, Option[Action]])] = {
    gameAction match {
      case ultiAction: UltiAction =>
        reduce0(ultiAction, invoker.role, invoker.isAdmin)
      case _ =>
        Left(GeneralFailure("Improper action"))
    }
  }
  
  implicit def convert (x: Ulti): Either[Failure, (Ulti, IO[Nothing, Option[Action]])] = Right((x, ZIO.none))

  private def reduce0(ultiAction: UltiAction, invokerRole: Option[Int], isAdmin: Boolean = false): Either[Failure, (Ulti, IO[Nothing, Option[Action]])] = {
    ultiAction match {
      case Init =>
        Right((copy(state = IN_PROGRESS), UltiEffects.cardDealEffect(UltiPlayer1)))
      case DealCards(ultiPlayer, seed) =>
        val inner = InnerUlti.generate(ultiPlayer, seed)
        this.copy(innerUlti = Some(inner))
      case ReadyForNextRound =>
        val invoker: Option[UltiPlayer] = invokerRole.flatMap(innerUlti.get.getUltiPlayerFromRoleId(_))
        if (invoker.isEmpty)
          Left(GeneralFailure("No role belongs to you."))
        else {
          val ultiPlayer = invoker.get
          if (innerUlti.isEmpty || innerUlti.get.phase != GameOverPhase) {
            Left(GeneralFailure("not applicable"))
          } else {
            if (!readyForNextRound(ultiPlayer)) {
              val temp = this.copy(readyForNextRound = for ((k, v) <- readyForNextRound) yield
                if (k == ultiPlayer) (k, true) else (k, v))
              if (temp.readyForNextRound.values.forall(_ == true)) {
                val starterPlayer = innerUlti.get.currentBidWinner.get.nextPlayer
                Right((copy(readyForNextRound = Map(UltiPlayer1 -> false, UltiPlayer2 -> false, UltiPlayer3 -> false)),
                  UltiEffects.cardDealEffect(starterPlayer)
                ))
              } else {
                temp
              }
            } else {
              Left(GeneralFailure("You have already signaled your readiness."))
            }
          }
        }
      case iua =>
        if (state == NOT_STARTED) {
          Left(GeneralFailure("Inner game is not started yet."))
        } else {
          assert(innerUlti.isDefined)
          val invoker: Option[UltiPlayer] = invokerRole.flatMap(innerUlti.get.getUltiPlayerFromRoleId(_))
          innerUlti.get.reduce(iua, invoker.get) match {
            case Left(failure) => Left(failure)
            case Right(newInnerUlti: InnerUlti) =>
              if (innerUlti.get.phase != GameOverPhase && newInnerUlti.phase == GameOverPhase) {
                val rs = RoundScore(
                  newInnerUlti.totalGainsForPlayer(UltiPlayer1),
                  newInnerUlti.totalGainsForPlayer(UltiPlayer2),
                  newInnerUlti.totalGainsForPlayer(UltiPlayer3)
                )
                this.copy(innerUlti = Some(newInnerUlti), scores = scores.appended(rs))
              } else {
                this.copy(innerUlti = Some(newInnerUlti))
              }
          }
        }
    }
  }

  override def toFrontendGame(role: Option[Int]): ClientUlti = {
    val ultiPlayer: Option[UltiPlayer] =
      role match {
        case Some(roleId) => UltiPlayer.getUltiPlayerFromRole(roleId).toOption
        case None => None
      }
    val a = ClientUlti(state, innerUlti.map(_.toClientInnerUlti(ultiPlayer)), scores, readyForNextRound)
    a
  }
}
