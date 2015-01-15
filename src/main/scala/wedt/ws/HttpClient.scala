package wedt.ws

import akka.actor.ActorSystem

import scala.concurrent.Future

/**
 * @author Marek Lewandowski <marek.lewandowski@semantive.com>
 * @since 15/01/15
 */
trait HttpClient {
  def get(url: String): Future[String]
}

class SprayHttpClient(actorSystem: ActorSystem) extends HttpClient {
  implicit val system = ActorSystem()
  import system.dispatcher // execution context for futures

  override def get(url: String): Future[String] = {

    import spray.http._
    import spray.client.pipelining._

    val pipeline: HttpRequest => Future[HttpResponse] = sendReceive

    val response: Future[HttpResponse] = pipeline(Get(url))

    response.map(_.entity.asString)
  }
}

class DummyHttpClient(response: Future[String]) extends HttpClient{
  override def get(url: String): Future[String] = response
}
