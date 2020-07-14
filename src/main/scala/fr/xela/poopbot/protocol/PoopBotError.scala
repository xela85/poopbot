package fr.xela.poopbot.protocol

sealed trait PoopBotError

object PoopBotError {

  case class BranchParsingFailure(given: String) extends PoopBotError
  case object WrongNumberOfArguments extends PoopBotError

  def message(poopBotError: PoopBotError): String = poopBotError match {
    case BranchParsingFailure(given) => s"$given is not a valid branch name."
    case WrongNumberOfArguments => s"Few or too many arguments were given to the bot."
  }

}
