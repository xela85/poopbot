package fr.xela.poopbot.protocol

sealed trait PoopBotError

object PoopBotError {

  case class BranchParsingFailure(given: String) extends PoopBotError

  case class UserParsingFailure(given: String) extends PoopBotError

  case object WrongNumberOfArguments extends PoopBotError

  def show(poopBotError: PoopBotError): String = {
    val explanation = poopBotError match {
      case BranchParsingFailure(given) => s"$given n'est pas un nom valide de branche"
      case UserParsingFailure(given) => s"$given n'est pas une mention correcte d'utilisateur"
      case WrongNumberOfArguments => s"mauvais nombre d'arguments"
    }
    s"""L'erreur suivante a été rencontrée par PoopBot: *$explanation*.
       |Veuillez vérifier votre commande""".stripMargin
  }

}
