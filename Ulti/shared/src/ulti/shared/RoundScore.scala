package ulti.shared

import ulti.shared.abstract0._
import upickle.default.{macroRW, ReadWriter => RW}

case class RoundScore(
                       scoreOfUltiPlayer1: Int,
                       scoreOfUltiPlayer2: Int,
                       scoreOfUltiPlayer3: Int,
                     ) {
  def getScoreForPlayer(ultiPlayer: UltiPlayer): Int = ultiPlayer match {
    case UltiPlayer1 => scoreOfUltiPlayer1
    case UltiPlayer2 => scoreOfUltiPlayer2
    case UltiPlayer3 => scoreOfUltiPlayer3
  }
}

object RoundScore {
  implicit def rw: RW[RoundScore] = macroRW
}