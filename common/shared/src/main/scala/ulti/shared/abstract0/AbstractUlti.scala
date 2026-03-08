package ulti.shared.abstract0

import gbge.shared.{Game, GameRole, GameState}
import ulti.shared.RoundScore

class AbstractUlti(
                    val state: GameState,
                    val innerUlti: Option[AbstractInnerUlti],
                    val scores: List[RoundScore] = List.empty,
                    val readyForNextRound: Map[UltiPlayer, Boolean] = Map(UltiPlayer1 -> false, UltiPlayer2 -> false, UltiPlayer3 -> false)
                  ) extends Game {
  override val minPlayerNumber: Int = 3
  override val maxPlayerNumber: Int = 3

  override val roles: List[GameRole] = List(UltiPlayer1, UltiPlayer2, UltiPlayer3)

  lazy val roundNumber: Int = {
    if (innerUlti.isEmpty)
      0
    else {
      val iu = innerUlti.get
      if (iu.phase == GameOverPhase) {
        scores.size
      } else {
        scores.size + 1
      }
    }
  }

  def totalScoreOfPlayer(ultiPlayer: UltiPlayer): Int = {
    scores.map(_.getScoreForPlayer(ultiPlayer)).sum
  }
}
