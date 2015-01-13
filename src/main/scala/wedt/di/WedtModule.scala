package wedt.di

import akka.actor.{Props, ActorSystem}
import com.softwaremill.macwire.Macwire

/**
 * @author Marek Lewandowski <marek.lewandowski@semantive.com>
 * @since 13/01/15
 */
trait WedtModule extends Macwire {
  lazy val actorSystem = ActorSystem("on-spray-can")


}
