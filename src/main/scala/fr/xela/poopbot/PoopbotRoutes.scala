package fr.xela.poopbot

import cats.Show
import cats.data.EitherT
import cats.effect.Sync
import cats.instances.option._
import cats.syntax.apply._
import cats.syntax.flatMap._
import cats.syntax.functor._
import fr.xela.poopbot.protocol.{BranchInfo, PoopBotError, User}
import fr.xela.poopbot.slackapi.SlackResponseType.{Ephemeral, InChannel}
import fr.xela.poopbot.slackapi.{SlackApiBody, SlackResponseBody}
import io.circe.syntax._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.impl.QueryParamDecoderMatcher

object PoopbotRoutes {

  object SlackTextMatcher extends QueryParamDecoderMatcher[String]("text")

  implicit def slackApiBodyDecoder[F[_]: Sync]: EntityDecoder[F, SlackApiBody] =
    EntityDecoder[F, UrlForm].flatMapR { formData =>
      EitherT.fromEither(
        (formData.getFirst("text"), formData.getFirst("user_id").map(User(_)))
          .mapN(SlackApiBody)
          .toRight[DecodeFailure](InvalidMessageBodyFailure(s"Fields missing"))
      )
    }

  def poopRoutes[F[_]: Sync](poopAlg: PoopAlg[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    def handlePoop(request: Request[F], action: SlackApiBody => F[Either[PoopBotError, BranchInfo]]) =
      for {
        slackApiBody <- request.as[SlackApiBody]
        branchResult <- action(slackApiBody)
        httpResult <- branchResult match {
          case Left(value)  => Ok(SlackResponseBody(Ephemeral, PoopBotError.show(value)).asJson)
          case Right(value) => Ok(SlackResponseBody(InChannel, Show[BranchInfo].show(value)).asJson)
        }
      } yield httpResult

    HttpRoutes.of[F] {
      case req @ POST -> Root / "take"    => handlePoop(req, poopAlg.take)
      case req @ POST -> Root / "release" => handlePoop(req, poopAlg.release)
      case req @ POST -> Root / "status"  => handlePoop(req, poopAlg.getState)
    }

  }

}
