
import java.util.{TimerTask, Timer}
import management.ManagementFactory
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
          Ok(statsReport)
       }
      Some(echo)
    } else {
      super.onRouteRequest(request)
    }
  }


  def statsReport = {
    val memory = ManagementFactory.getMemoryMXBean
    val nonHeap = memory.getNonHeapMemoryUsage
    val heap = memory.getHeapMemoryUsage
    val threads = ManagementFactory.getThreadMXBean
    val classes = ManagementFactory.getClassLoadingMXBean

    line("requestCountPerMinute", requestCounter.rate) +
    line("errorCountPerMinute", errorCounter.rate) +
    line("totalMemory", Runtime.getRuntime.totalMemory()) +
    line("freeMemory", Runtime.getRuntime.freeMemory()) +
    line("maxMemory", Runtime.getRuntime.maxMemory()) +
    line("committedNonHeap", nonHeap.getCommitted) +
    line("maxNonHeap", nonHeap.getMax) +
    line("usedNonHeap", nonHeap.getUsed) +
    line("maxHeap", heap.getMax) +
    line("committedHeap", heap.getCommitted) +
    line("userHeap", heap.getUsed) +
    line("currentThreadCPUTime", threads.getCurrentThreadCpuTime) +
    line("currentThreadUserTime", threads.getCurrentThreadUserTime) +
    line("daemonThreadCount", threads.getDaemonThreadCount) +
    line("peakThreadCount", threads.getPeakThreadCount) +
    line("threadCount", threads.getThreadCount) +
    line("totalLoadedClassesCount", classes.getTotalLoadedClassCount) +
    line("loadedClassesCount", classes.getLoadedClassCount) +
    line("loadedUnloadedClassesCount", classes.getUnloadedClassCount)
  }

  def line(item: String, value: Double) = item + "=" + value + "\n"


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




