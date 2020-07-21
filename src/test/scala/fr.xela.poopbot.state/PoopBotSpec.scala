import org.specs2.Specification
import fr.xela.poopbot.state.MergingQueue
import fr.xela.poopbot.state.MergingQueue.Nobody
import fr.xela.poopbot.state.MergingQueue.SomebodyAnd
import org.http4s.Method.NoBody

class PoopBotSpec extends Specification {

  def is = s2"""
    
A merging queue enables to sort users who will be able to merge their work into a specific branch. It implements specific rules for ordering. 

Calling append on a merging queue
    - should lead element to be added if it is not present ${Adding.elmNotPresent}
    - should not lead element to be added if it is already there ${Adding.elmPresent}

Calling remove on a merging queue
    - should lead element to be removed if it exists ${Removing.elmPresent}
    - should return an unchanged queue if it does not exist ${Removing.elmNotPresent}
    """

  private val exampleQueue: MergingQueue[String] = Nobody
    .append("1")
    .append("2")
    .append("3")

  private val initialSize = exampleQueue.size

  private val newItem: String      = "4"
  private val itemToRemove: String = "3"

  object Adding {

    def elmNotPresent = {
      val updatedQueue = exampleQueue.append(newItem)
      (updatedQueue.toList must contain(
        newItem
      )).and(updatedQueue.size must beEqualTo(exampleQueue.size + 1))
    }

    def elmPresent = {
      val itemAlreadyHere = exampleQueue.append(newItem)
      val updatedQueue    = itemAlreadyHere.append(newItem)
      itemAlreadyHere.size should beEqualTo(updatedQueue.size)
    }

  }

  object Removing {

    def elmPresent = {
      val updatedQueue = exampleQueue.remove(itemToRemove)
      (updatedQueue.toList must not contain (
        itemToRemove
      )).and(updatedQueue.size must beEqualTo(exampleQueue.size - 1))
    }

    def elmNotPresent = {
      val itemNotHere  = exampleQueue.remove(itemToRemove)
      val updatedQueue = itemNotHere.remove(itemToRemove)
      updatedQueue.size should beEqualTo(itemNotHere.size)
    }

  }

}
