package io.github.binaryfoo.lagotto.reader

import java.io.{File, FileOutputStream, PrintStream}

import io.github.binaryfoo.lagotto.LagoTest

class TailInputStreamTest extends LagoTest {

  "read()" should "return bytes immediately when enough are available" in {
    val (in, out) = tailOfTempFile()
    out.println("the quick brown fox")

    readString(in, 3) shouldBe "the"
  }

  it should "return contents written after a delay" in {
    val (in, out) = tailOfTempFile()
    afterDelay(100, out.print("from the future"))
    val start = System.currentTimeMillis()
    val read = readString(in)
    val elapsed = System.currentTimeMillis() - start
    read shouldBe "from the future"
    elapsed shouldBe > (100L)
  }

  it should "return extra content appended after a delay" in {
    val (in, out) = tailOfTempFile()
    out.println("line one")
    afterDelay(100, out.print("line two"))

    readString(in) shouldBe "line one\n"
    val start = System.currentTimeMillis()
    val read = readString(in)
    val elapsed = System.currentTimeMillis() - start
    read shouldBe "line two"
    elapsed shouldBe > (100L)

    afterDelay(100, out.print("line three"))
    readString(in) shouldBe "line three"
  }

  it should "read from new file with the same path" in {
    val (in, _) = tailOfTempFile()
    afterDelay(100, {
      val file = in.file.file
      file.delete()
      val newOut = new FileOutputStream(file, false)
      newOut.write("a line".getBytes)
      newOut.close()
    })

    readString(in) shouldBe "a line"
  }

  it should "stop waiting if the file is done" in {
    val (in, _) = tailOfTempFile()
    afterDelay(100, in.file.done = true)
    val start = System.currentTimeMillis()
    val read = in.read()
    val elapsed = System.currentTimeMillis() - start
    read shouldBe -1
    elapsed shouldBe > (100L)
  }

  private def readString(in: TailInputStream, len: Int = 1024): String = {
    val bytes = new Array[Byte](len)
    val n = in.read(bytes)
    new String(bytes, 0, n)
  }

  private def tailOfTempFile(): (TailInputStream, PrintStream) = {
    val file = tempFile()
    val out = new PrintStream(new FileOutputStream(file))
    val in = TailInputStream(file)
    (in, out)
  }
}
