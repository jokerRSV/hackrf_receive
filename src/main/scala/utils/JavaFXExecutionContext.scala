package utils

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.ExecutionContext


object JavaFXExecutionContext {
  val customExecutor: ExecutorService = Executors.newWorkStealingPool()
  implicit val customExecutionContext: ExecutionContext = ExecutionContext.fromExecutorService(customExecutor)
}