package ulti.shared.contracts

import ulti.shared.abstract0.UltiPlayer
import upickle.default.{macroRW, ReadWriter => RW}

case class ContractWithStatus(
                             contract: Contract,
                             contraLevel: ContraLevel = NoContra,
                             contractResult: ContractResult = Open
                             ) {
  def doubleForPlayer(ultiPlayer: UltiPlayer, soloist: UltiPlayer): ContractWithStatus = {
    assert(soloist != ultiPlayer)
    this.contraLevel match {
      case NoContra => {
        this.contract match {
          case ContractSimple(_) => this.copy(contraLevel = JointContra(1))
          case _ => {
            val theOtherPlayer = UltiPlayer.allThePlayers.filterNot(_ == soloist).filterNot(_ == ultiPlayer).head
            this.copy(contraLevel = IndividualContra(Map(ultiPlayer -> 1, theOtherPlayer -> 0)))
          }
        }
      }
      case JointContra(level) => {
        this.copy(contraLevel = JointContra(level+1))
      }
      case IndividualContra(map) => {
        val newMap = map.map(keyValuePair => {
          val key = keyValuePair._1
          val currentContraLevel: Int = keyValuePair._2
          if (key == ultiPlayer) {
            (key, currentContraLevel+1)
          } else {
            keyValuePair
          }
        })
        this.copy(contraLevel = IndividualContra(newMap))
      }
    }
  }
}

case object ContractWithStatus {
  implicit def rw: RW[ContractWithStatus] = macroRW
}