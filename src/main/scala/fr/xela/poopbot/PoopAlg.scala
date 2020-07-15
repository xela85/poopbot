package fr.xela.poopbot

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.either._
import cats.syntax.functor._
import fr.xela.poopbot.protocol.PoopBotError.WrongNumberOfArguments
import fr.xela.poopbot.protocol.{AssignationResult, Branch, PoopBotError, User}
import fr.xela.poopbot.state.BotState


trait PoopAlg[F[_]] {
  def take(message: String): F[Either[PoopBotError, AssignationResult]]
  def release(message: String): F[Either[PoopBotError, AssignationResult]]
}

object PoopAlg {

  def withInitialState[F[_]: Sync]: F[PoopAlg[F]] = for {
    initial <- Ref.of(BotState.initial)
  } yield impl(initial)

  def impl[F[_]: Sync](state: Ref[F, BotState]): PoopAlg[F] =
    new PoopAlg[F] {

    override def take(message: String): F[Either[PoopBotError, AssignationResult]] =
      parseMessage(message).traverse { branch =>
        state.modifyState(BotState.addUserFromBranch(branch, User.single))
      }

    override def release(message: String): F[Either[PoopBotError, AssignationResult]] =
      parseMessage(message).traverse { branch =>
        state.modifyState(BotState.removeUserFromBranch(branch, User.single))
      }

    private def parseMessage(message: String): Either[PoopBotError, Branch] = {
      val splitted = message.split("\\s+")
      splitted.toList match {
        case branchStr :: Nil =>
          Branch.parse(branchStr)
        case _ => Left(WrongNumberOfArguments)
      }
    }

  }

}