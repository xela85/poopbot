package fr.xela.poopbot

import cats.Apply
import cats.data.{EitherT, OptionT}
import cats.effect.Sync
import cats.implicits._
import fr.xela.poopbot.protocol.PoopBotError
import org.http4s.{DecodeFailure, EntityDecoder, HttpRoutes, InvalidMessageBodyFailure}
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.impl.QueryParamDecoderMatcher
import org.http4s.multipart.{Multipart, MultipartParser}
import cats.syntax.apply._
import cats.syntax.traverse._
import cats.instances.option._
import fr.xela.poopbot.PoopbotRoutes.transformToMessage
import fs2.text.utf8Decode

object PoopbotRoutes {

  object SlackTextMatcher extends QueryParamDecoderMatcher[String]("text")


  case class SlackApiBody(text: String, slackUser: User)

  implicit def slackApiBodyDecoder[F[_] : Sync]: EntityDecoder[F, SlackApiBody] = {
    EntityDecoder[F, Multipart[F]].flatMapR { formData =>
      def parseMultipartField(fieldName: String) = {
        EitherT(formData.parts.find(_.name.contains(fieldName))
          .toRight[DecodeFailure](InvalidMessageBodyFailure(s"Field $fieldName is missing"))
          .traverse(_.body.through(utf8Decode).compile.foldMonoid))
      }

      for {
        _ <- EitherT.right(Sync[F].delay(println(formData)))
        text <- parseMultipartField("text")
        user <- parseMultipartField("user_id").map(User(_))
      } yield SlackApiBody(text, user)
    }
  }

  // oauth : xoxb-653116819058-1238938273842-AeT4G4h5C59ZYRIwVZrwhqbX
  def poopRoutes[F[_] : Sync](poopAlg: PoopAlg[F]): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._
    HttpRoutes.of[F] {
      case req @ POST -> Root / "take" =>
        for {
          slackApiBody <- req.as[SlackApiBody]
          takeBranchResult <- poopAlg.take(slackApiBody.text)
          httpResult <- Ok(transformToMessage(takeBranchResult))
        } yield httpResult
      case req @  POST -> Root / "release" =>
        for {
          slackApiBody <- req.as[SlackApiBody]
          takeBranchResult <- poopAlg.release(slackApiBody.text)
          httpResult <- Ok(transformToMessage(takeBranchResult))
        } yield httpResult
    }

  }

  def transformToMessage(result: Either[PoopBotError, BranchAssignation]): String = result.fold(PoopBotError.message, BranchAssignation.show)


}