package wedt.conf

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.slf4j.StrictLogging
import net.ceedubs.ficus.Ficus._

/**
 * @author Marek Lewandowski <marek.lewandowski@semantive.com>
 * @since 13/01/15
 */
object Config extends StrictLogging {

  val cf = {
    val nameOfConfig: String = sys.props.get("config").getOrElse("config.conf")
    logger.info(s"Loading config $nameOfConfig")
    ConfigFactory.load(nameOfConfig)
  }

  object crawler {
    val minimumPosts = cf.as[Int]("crawler.minimum-posts")

    val minimumWords = cf.as[Int]("crawler.minimum-words")

    val pagesToAnalyze = cf.as[Int]("crawler.pages-to-analyze")
  }

  object network {
    val interface = cf.as[String]("network.interface")
    val port = cf.as[Int]("network.port")
  }

  object restApi {
    val context = cf.as[String]("restApi.context")
    val queryPath = cf.as[String]("restApi.queryPath")
  }

}
