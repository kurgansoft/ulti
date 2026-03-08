package ulti.shared.contracts

import gbge.shared.FrontendPlayer
import ulti.shared.abstract0.UltiPlayer
import zio.json.JsonCodec
import zio.schema.{DeriveSchema, Schema}

sealed trait ContraLevel {
  def levelForPlayer(ultiPlayer: UltiPlayer): Int

  def displayText(players: List[FrontendPlayer]): String
}

object ContraLevel {
  implicit val schema: Schema[ContraLevel] = DeriveSchema.gen[ContraLevel]
  implicit val codec: JsonCodec[ContraLevel] = 
    zio.schema.codec.JsonCodec.jsonCodec(schema)

  val maximumContraLevel = 6

  def levelText(level: Int): String = level match {
    case 1 => "Double"
    case 2 => "Redouble"
    case 3 => "Surdouble"
    case 4 => "Morddouble"
    case 5 => "Hirschdouble"
    case 6 => "Fedák Sári"
  }
}

case object NoContra extends ContraLevel {
  override def levelForPlayer(ultiPlayer: UltiPlayer): Int = 0

  override def displayText(players: List[FrontendPlayer]): String = ""
}

case class JointContra(level: Int = 1) extends ContraLevel {
  assert(level >= 1)

  override def levelForPlayer(ultiPlayer: UltiPlayer): Int = level

  override def displayText(players: List[FrontendPlayer]): String = ContraLevel.levelText(level) + " (" + Math.pow(2, level) + "x)"
}

case class IndividualContra(map: Map[UltiPlayer, Int]) extends ContraLevel {
  assert(map.size == 1 || map.size == 2)

  override def levelForPlayer(ultiPlayer: UltiPlayer): Int = {
    if (map.keys.exists(_ == ultiPlayer))
      map(ultiPlayer)
    else 0
  }

  override def displayText(players: List[FrontendPlayer]): String =  {
    val inside = map.filter(_._2 > 0).map(keyValuePair => {
      val playerName = FrontendPlayer.getNameOfPlayerWithRole(keyValuePair._1.roleId)(players)
      val contraLevel = levelForPlayer(keyValuePair._1)
      ContraLevel.levelText(contraLevel) + " (" + Math.pow(2, contraLevel) + "x) against " + playerName
    }).mkString("; ")

    "("+ inside + ")"
  }
}

object IndividualContra {
  implicit val schema: Schema[IndividualContra] = DeriveSchema.gen[IndividualContra]
  implicit val codec: JsonCodec[IndividualContra] = zio.schema.codec.JsonCodec.jsonCodec(schema)
}