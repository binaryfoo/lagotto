package io.github.binaryfoo.lagotto.reader

import java.io.{File, FileOutputStream, PrintStream}
import java.nio.file.{Files, StandardCopyOption}

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

  it should "read from new file with the same path when old file deleted" in {
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

  it should "read from new file with the same path when old file moved" in {
    val (in, out) = tailOfTempFile()
    out.println("line 1")
    afterDelay(100, {
      val trash = new File("lago-test-trash.txt")
      trash.deleteOnExit()
      val file = in.file.file
      Files.move(file.toPath, trash.toPath, StandardCopyOption.REPLACE_EXISTING)
      val path = file.getAbsolutePath
      val newOut = new FileOutputStream(path, false)
      newOut.write("two".getBytes)
      newOut.close()
    })

    readString(in) shouldBe "line 1\n"
    readString(in) shouldBe "two"
  }

  it should "keep waiting if the file disappears" in {
    val (in, out) = tailOfTempFile()
    out.println("line one")
    afterDelay(100, {
      in.file.file.delete()
      Thread.sleep(500) // magic: long enough the poller wakes up and finds the file missing
      val path = in.file.file.getAbsolutePath
      val newOut = new FileOutputStream(path, false)
      newOut.write("two".getBytes)
      newOut.close()
    })

    readString(in) shouldBe "line one\n"
    readString(in) shouldBe "two"
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
