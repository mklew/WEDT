package wedt

import akka.actor.Props
import akka.io.IO
import com.typesafe.scalalogging.slf4j.StrictLogging
import spray.can.Http
import wedt.conf.Config
import wedt.di.WedtModule
import wedt.ws.SentimentAnalyzerWsActor

/**
 * @author Marek Lewandowski <marek.lewandowski@semantive.com>
 * @since 13/01/15
 */
object Boot extends App with StrictLogging with WedtModule {

  implicit val system = actorSystem

  val service = system.actorOf(Props[SentimentAnalyzerWsActor], "sentiment-analyzer-ws")

  logger.info(
    """
      |##########################################
      |#      Sentiment analyzer starts         #
      |##########################################
    """.stripMargin)

  IO(Http) ! Http.Bind(service, interface = Config.network.interface, port = Config.network.port)

}
