package fr.xela.poopbot

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.instances.either._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.functor._
import fr.xela.poopbot.protocol.PoopBotError.WrongNumberOfArguments
import fr.xela.poopbot.protocol._
import fr.xela.poopbot.slackapi.SlackApiBody
import fr.xela.poopbot.state.BotState


trait PoopAlg[F[_]] {
  def take(message: SlackApiBody): F[Either[PoopBotError, BranchInfo]]

  def release(message: SlackApiBody): F[Either[PoopBotError, BranchInfo]]

  def getState(message: SlackApiBody): F[Either[PoopBotError, BranchInfo]]
}

object PoopAlg {

  def withInitialState[F[_] : Sync]: F[PoopAlg[F]] = for {
    initial <- Ref.of(BotState.initial)
  } yield impl(initial)

  def impl[F[_] : Sync](state: Ref[F, BotState]): PoopAlg[F] =
    new PoopAlg[F] {

      override def take(body: SlackApiBody): F[Either[PoopBotError, BranchInfo]] =
        parseCommand(body).traverse { command =>
          state.modifyState(BotState.addUserFromBranch(command.branch, command.user))
        }

      override def release(body: SlackApiBody): F[Either[PoopBotError, BranchInfo]] =
        parseCommand(body).traverse { command =>
          state.modifyState(BotState.removeUserFromBranch(command.branch, command.user))
        }


      override def getState(message: SlackApiBody): F[Either[PoopBotError, BranchInfo]] =
        Branch.parse(message.text.trim).traverse { branch =>
          state.get.map(BotState.getAssignation(_, branch))
        }

      private def parseCommand(body: SlackApiBody): Either[PoopBotError, Command] = {
        val splitted = body.text.split("\\s+")
        splitted.toList match {
          case branchStr :: Nil =>
            Branch.parse(branchStr).map(Command(_, body.slackUser))
          case branchStr :: userStr :: Nil =>
            (Branch.parse(branchStr), User.fromMention(userStr)).mapN(Command)
          case _ => Left(WrongNumberOfArguments)
        }
      }

    }

}