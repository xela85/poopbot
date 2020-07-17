package fr.xela.poopbot.slackapi

import io.circe.Encoder

sealed trait SlackResponseType

object SlackResponseType {

  case object InChannel extends SlackResponseType

  case object Ephemeral extends SlackResponseType

  implicit val encodeAsJson: Encoder[SlackResponseType] = Encoder.encodeString.contramap {
    case InChannel => "in_channel"
    case Ephemeral => "ephemeral"
  }

}
