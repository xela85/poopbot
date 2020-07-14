package fr.xela.poopbot

import cats.Show
import cats.instances.queue._
import cats.instances.string._
import cats.syntax.foldable._
import fr.xela.poopbot.protocol.Branch
import monocle.Lens
import cats.data.State
import monocle.macros.GenLens

import scala.collection.immutable.Queue

case class BotState(master: BranchAssignation, next: BranchAssignation, prod: BranchAssignation)

case class BranchAssignation(users: Queue[User])

object BranchAssignation {
  val empty: BranchAssignation = BranchAssignation(Queue.empty)

  val usersL: Lens[BranchAssignation, Queue[User]] = GenLens[BranchAssignation](_.users)

  def show(branchAssignation: BranchAssignation): String = branchAssignation.users.map(_.name).intercalate(", ")



  def queueUser(branchAssignation: BranchAssignation, user: User): BranchAssignation =
    BranchAssignation(branchAssignation.users.enqueue(user))

  def dequeueUser(branchAssignation: BranchAssignation, user: User): BranchAssignation =
    BranchAssignation(branchAssignation.users.filterNot(_ == user))

}

case class User(name: String) extends AnyVal

object User {
  def single: User = User("single-user")
}

object BotState {

  private val masterL: Lens[BotState, BranchAssignation] = GenLens[BotState](_.master)
  private val nextL: Lens[BotState, BranchAssignation] = GenLens[BotState](_.next)
  private val prodL: Lens[BotState, BranchAssignation] = GenLens[BotState](_.prod)

  def initial: BotState = BotState(
    master = BranchAssignation.empty,
    next = BranchAssignation.empty,
    prod = BranchAssignation.empty
  )

  def addUserFromBranch(branch: Branch, user: User): State[BotState, BranchAssignation] = {
    State { botState =>
      val lensForBranch = updateBranchLens(branch)
      val updated = (lensForBranch composeLens BranchAssignation.usersL).modify(_.enqueue(user))(botState)
      (updated, lensForBranch.get(updated))
    }
  }

  def removeUserFromBranch(branch: Branch, user: User): State[BotState, BranchAssignation] = {
    State { botState =>
      val lensForBranch = updateBranchLens(branch)
      val updated = (lensForBranch composeLens BranchAssignation.usersL).modify(_.filterNot(_ == user))(botState)
      (updated, lensForBranch.get(updated))
    }
  }

  private def updateBranchLens(branch: Branch): Lens[BotState, BranchAssignation] = branch match {
    case Branch.Master => masterL
    case Branch.Next => nextL
    case Branch.Prod => prodL
  }

}
