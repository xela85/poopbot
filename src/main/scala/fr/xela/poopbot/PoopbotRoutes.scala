package fr.xela.poopbot

import cats.data.EitherT
import cats.effect.Sync
import cats.implicits._
import fr.xela.poopbot.protocol.PoopBotError
import fs2.text.utf8Decode
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.impl.QueryParamDecoderMatcher
import org.http4s.multipart.Multipart
import org.http4s._

object PoopbotRoutes {

  object SlackTextMatcher extends QueryParamDecoderMatcher[String]("text")


  case class SlackApiBody(text: String, slackUser: User)

  implicit def slackApiBodyDecoder[F[_] : Sync]: EntityDecoder[F, SlackApiBody] = {
    EntityDecoder[F, UrlForm].flatMapR { formData =>
      EitherT.fromEither {
        (formData.getFirst("text"),
          formData.getFirst("user_id").map(User(_)))
          .mapN(SlackApiBody)
          .toRight[DecodeFailure](InvalidMessageBodyFailure(s"Fields missing"))
      }
    }
  }

  def poopRoutes[F[_] : Sync](poopAlg: PoopAlg[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    def handlePoop(request: Request[F], action: String => F[Either[PoopBotError, BranchAssignation]]) = {
      for {
        slackApiBody <- request.as[SlackApiBody]
        branchResult <- action(slackApiBody.text)
        httpResult <- Ok(branchResult.fold(PoopBotError.show, BranchAssignation.show))
      } yield httpResult
    }

    HttpRoutes.of[F] {
      case req@POST -> Root / "take" => handlePoop(req, poopAlg.take)
      case req@POST -> Root / "release" => handlePoop(req, poopAlg.release)
    }

  }

}