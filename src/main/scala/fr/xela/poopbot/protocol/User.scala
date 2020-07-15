package fr.xela.poopbot.protocol

import cats.Show
import cats.instances.string._
import cats.syntax.contravariant._

case class User(name: String) extends AnyVal

object User {
  def single: User = User("single-user")

  implicit val showUser: Show[User] = Show[String].contramap(_.name)

}