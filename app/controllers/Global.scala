
import java.util.{TimerTask, Timer}
import play.api.GlobalSettings
import play.api.mvc.{Action, RequestHeader}
import play.api.mvc.Results._




/**
 * @author michael neale
 */
object Global extends GlobalSettings {

  val requestCounter = new ReqCounter
  val errorCounter = new ReqCounter

  override def onRouteRequest(request: RequestHeader) = {
    requestCounter.increment()
    if (request.uri == "/_stats") {
      val echo = Action { request =>
          Ok("requestCountPerMinute=" + requestCounter.rate +
             "\nerrorCountPerMinute=" + errorCounter.rate)
       }
      Some(echo)
    } else {
      super.onRouteRequest(request)
    }
  }

  override def onError(request: RequestHeader, ex: Throwable) = {
    errorCounter.increment()
    super.onError(request, ex)
  }


  class ReqCounter(val timer: Timer) {
    var currentRate = 0.0
    var requestCount = 0

    def this() = {
      this(new Timer)
      timer.scheduleAtFixedRate(new TimerTask {
        def run() {
          currentRate = requestCount
          requestCount = 0
        }
      }, 0, 60000)
    }



    def increment() {
      requestCount = requestCount + 1
    }

    def rate = if (currentRate > 0) currentRate else requestCount
  }
}




