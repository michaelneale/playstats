
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

    line("request_requestCount", requestCounter.rate) +
    line("request_errorCount", errorCounter.rate) +
    line("totalMemory", Runtime.getRuntime.totalMemory()) +
    line("freeMemory", Runtime.getRuntime.freeMemory()) +
    line("maxMemory", Runtime.getRuntime.maxMemory()) +
    line("memory_nonheap_committed", nonHeap.getCommitted) +
    line("memory_nonheap_max", nonHeap.getMax) +
    line("memory_nonheap_used", nonHeap.getUsed) +
    line("memory_heap_max", heap.getMax) +
    line("memory_heap_committed", heap.getCommitted) +
    line("memory_heap_used", heap.getUsed) +
    line("threads_cputime", threads.getCurrentThreadCpuTime) +
    line("currentThreadUserTime", threads.getCurrentThreadUserTime) +
    line("daemonThreadCount", threads.getDaemonThreadCount) +
    line("threads_peakCount", threads.getPeakThreadCount) +
    line("threads_count", threads.getThreadCount) +
    line("totalLoadedClassesCount", classes.getTotalLoadedClassCount) +
    line("totalStartedThreadCount", threads.getTotalStartedThreadCount) +
    line("classes_loaded", classes.getLoadedClassCount) +
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




