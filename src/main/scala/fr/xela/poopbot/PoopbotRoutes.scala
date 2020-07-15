package fr.xela.poopbot

import cats.Show
import cats.data.EitherT
import cats.effect.Sync
import cats.implicits._
import fr.xela.poopbot.protocol.{AssignationResult, PoopBotError, User}
import fr.xela.poopbot.state.MergingQueue
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.impl.QueryParamDecoderMatcher

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

    def handlePoop(request: Request[F], action: SlackApiBody => F[Either[PoopBotError, AssignationResult]]) = {
      for {
        slackApiBody <- request.as[SlackApiBody]
        branchResult <- action(slackApiBody)
        httpResult <- Ok(branchResult.fold(PoopBotError.show, Show[AssignationResult].show))
      } yield httpResult
    }

    HttpRoutes.of[F] {
      case req@POST -> Root / "take" => handlePoop(req, poopAlg.take)
      case req@POST -> Root / "release" => handlePoop(req, poopAlg.release)
    }

  }

}