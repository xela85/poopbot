package fr.xela.poopbot.protocol

import cats.Show
import fr.xela.poopbot.state.MergingQueue
import cats.syntax.show._

case class AssignationResult(branch: Branch, mergingQueue: MergingQueue[User])

object AssignationResult {

  implicit val showAssignationResult: Show[AssignationResult] = Show.show { assignationResult =>
    s"""*Etat de la branche sur ${assignationResult.branch.show}*
       |${assignationResult.mergingQueue.show}""".stripMargin
  }

}
