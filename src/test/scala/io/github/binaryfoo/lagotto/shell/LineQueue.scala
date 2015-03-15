package io.github.binaryfoo.lagotto.shell

import java.io.ByteArrayOutputStream
import java.util.concurrent.{TimeUnit, LinkedBlockingQueue, LinkedBlockingDeque}

class LineQueue extends ByteArrayOutputStream {

  private val queue = new LinkedBlockingQueue[String]()

  override def write(b: Array[Byte], off: Int, len: Int): Unit = {
    super.write(b, off, len)
    if (b(off + len - 1) == '\n') {
      flush()
    }
  }

  override def flush(): Unit = {
    queue.put(new String(toByteArray))
    reset()
  }

  def next(): String = queue.poll(500, TimeUnit.MILLISECONDS)
}
