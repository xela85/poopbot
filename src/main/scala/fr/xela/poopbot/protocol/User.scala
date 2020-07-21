package fr.xela.poopbot.protocol

import cats.Show
import cats.instances.string._
import cats.syntax.contravariant._
import fr.xela.poopbot.protocol.PoopBotError.UserParsingFailure

case class User(name: String) extends AnyVal

object User {

  private val escapedUserRegex = "<@(.+)\\|.+>".r

  def fromMention(strUser: String): Either[UserParsingFailure, User] =
    strUser match {
      case escapedUserRegex(userId) => Right(User(userId))
      case _                        => Left(UserParsingFailure(strUser))
    }

  implicit val showUser: Show[User] = Show[String].contramap(user => s"<@${user.name}>")
}
