package fr.xela.poopbot

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.functor._
import fr.xela.poopbot.protocol.{Branch, PoopBotError}
import cats.syntax.either._
import fr.xela.poopbot.protocol.PoopBotError.WrongNumberOfArguments


trait PoopAlg[F[_]] {
  def take(message: String): F[Either[PoopBotError, BranchAssignation]]
  def release(message: String): F[Either[PoopBotError, BranchAssignation]]
}

object PoopAlg {

  def withInitialState[F[_]: Sync]: F[PoopAlg[F]] = for {
    initial <- Ref.of(BotState.initial)
  } yield impl(initial)

  def impl[F[_]: Sync](state: Ref[F, BotState]): PoopAlg[F] =
    new PoopAlg[F] {

    override def take(message: String): F[Either[PoopBotError, BranchAssignation]] =
      parseMessage(message).traverse { branch =>
        state.modifyState(BotState.addUserFromBranch(branch, User.single))
      }

    override def release(message: String): F[Either[PoopBotError, BranchAssignation]] =
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