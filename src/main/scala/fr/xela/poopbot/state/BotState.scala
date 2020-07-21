package fr.xela.poopbot.state

import cats.data.State
import fr.xela.poopbot.protocol.{Branch, BranchInfo, User}
import fr.xela.poopbot.state.MergingQueue.Nobody
import monocle.Lens
import monocle.macros.GenLens

case class BotState(master: MergingQueue[User], next: MergingQueue[User], prod: MergingQueue[User])

object BotState {

  private val masterL: Lens[BotState, MergingQueue[User]] = GenLens[BotState](_.master)
  private val nextL: Lens[BotState, MergingQueue[User]]   = GenLens[BotState](_.next)
  private val prodL: Lens[BotState, MergingQueue[User]]   = GenLens[BotState](_.prod)

  def initial: BotState =
    BotState(
      master = Nobody,
      next = Nobody,
      prod = Nobody
    )

  def addUserFromBranch(branch: Branch, user: User): State[BotState, BranchInfo] =
    State { botState =>
      val lensForBranch = branchLens(branch)
      val updated       = lensForBranch.modify(_.append(user))(botState)
      (updated, BranchInfo(branch, lensForBranch.get(updated)))
    }

  def removeUserFromBranch(branch: Branch, user: User): State[BotState, BranchInfo] =
    State { botState =>
      val lensForBranch = branchLens(branch)
      val updated       = lensForBranch.modify(_.remove(user))(botState)
      (updated, BranchInfo(branch, lensForBranch.get(updated)))
    }

  def getAssignation(botState: BotState, branch: Branch): BranchInfo =
    BranchInfo(branch, branchLens(branch).get(botState))

  private def branchLens(branch: Branch): Lens[BotState, MergingQueue[User]] =
    branch match {
      case Branch.Master => masterL
      case Branch.Next   => nextL
      case Branch.Prod   => prodL
    }

}
