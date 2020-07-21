package fr.xela.poopbot.state

import cats.Show
import cats.instances.list._
import cats.instances.string._
import cats.syntax.foldable._
import cats.syntax.show._
import fr.xela.poopbot.state.MergingQueue.{Nobody, SomebodyAnd}
import scala.annotation.tailrec


sealed trait MergingQueue[+T] {

  final def append[B >: T](element: B): MergingQueue[B] = this match {
    case Nobody => SomebodyAnd(element, Nobody)
    case assignation@SomebodyAnd(existing, _) if existing == element => assignation
    case SomebodyAnd(existing, rest) => SomebodyAnd(existing, rest.append(element))
  }

  final def remove[B >: T](element: B): MergingQueue[B] = this match {
    case Nobody => Nobody
    case SomebodyAnd(existing, rest) if existing == element => rest
    case SomebodyAnd(existing, rest) => SomebodyAnd(existing, rest.remove(element))
  }

  final def concat[B >: T](mergingQueue: MergingQueue[B]): MergingQueue[B] = mergingQueue match {
    case Nobody => this
    case SomebodyAnd(user, rest) => rest.concat(append(user))
  }

  final def size: Int = {
    def recursiveSize(queue: MergingQueue[T], acc: Int): Int = queue match {
      case Nobody => acc
      case SomebodyAnd(_, rest) => recursiveSize(rest, acc + 1)
    }
    recursiveSize(this, 0)
  }

  final def toList: List[T] = this match {
    case MergingQueue.Nobody => Nil
    case SomebodyAnd(user, rest) => user :: rest.toList
  }

}

object MergingQueue {

  case object Nobody extends MergingQueue[Nothing]

  case class SomebodyAnd[+T](user: T, rest: MergingQueue[T]) extends MergingQueue[T]

  implicit def showQueue[T: Show]: Show[MergingQueue[T]] = Show.show { queue =>
    queue.toList match {
      case Nil => "Personne n'attend sur cette branche."
      case list => list.map(_.show).intercalate(" ➡ ")
    }

  }

}