package fr.xela.poopbot.protocol

import cats.Show
import fr.xela.poopbot.state.MergingQueue
import cats.syntax.show._

case class BranchInfo(branch: Branch, mergingQueue: MergingQueue[User])

object BranchInfo {

  implicit val showAssignationResult: Show[BranchInfo] = Show.show { assignationResult =>
    s"""*Etat de la branche sur ${assignationResult.branch.show}*
       |${assignationResult.mergingQueue.show}""".stripMargin
  }

}
