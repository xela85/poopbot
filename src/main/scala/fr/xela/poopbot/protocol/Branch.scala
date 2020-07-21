package fr.xela.poopbot.protocol

import cats.Show
import fr.xela.poopbot.protocol.PoopBotError.BranchParsingFailure

sealed trait Branch
object Branch {
  case object Master extends Branch
  case object Next   extends Branch
  case object Prod   extends Branch

  def values: List[Branch] = List(Master, Next, Prod)

  def parse(str: String): Either[BranchParsingFailure, Branch] =
    str match {
      case "master" => Right(Master)
      case "next"   => Right(Next)
      case "prod"   => Right(Prod)
      case _        => Left(BranchParsingFailure(str))
    }

  implicit val showBranch: Show[Branch] = Show.fromToString

}
