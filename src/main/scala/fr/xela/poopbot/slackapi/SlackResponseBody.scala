package fr.xela.poopbot.slackapi

import io.circe.Encoder

case class SlackResponseBody(responseType: SlackResponseType, text: String)

object SlackResponseBody {
  implicit val encode: Encoder[SlackResponseBody] =
    Encoder.forProduct2("response_type", "text")(user => (user.responseType, user.text))
}
