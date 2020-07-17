package fr.xela.poopbot.slackapi

import fr.xela.poopbot.protocol.User

case class SlackApiBody(text: String, slackUser: User)
