package ulti.shared.abstract0

import upickle.default.{macroRW, ReadWriter => RW}

abstract sealed class Phase

object Phase {
  implicit def rw: RW[Phase] = macroRW
}

case object BiddingPhase extends Phase
case object PlayingPhase extends Phase
case object GameOverPhase extends Phase