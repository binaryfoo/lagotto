package io.github.binaryfoo.lagotto.reader

import io.github.binaryfoo.lagotto.LagoTest

class JstackLogTest extends LagoTest {

  it should "parse a single stack trace" in {
    val trace = """"FocusManager timer" daemon prio=2 tid=12bb62000 nid=0x12eb70000 in Object.wait() [12eb6f000]
                  |   java.lang.Thread.State: WAITING (on object monitor)
                  |	at java.lang.Object.wait(Native Method)
                  |	- waiting on <7c45c3490> (a java.util.TaskQueue)
                  |	at java.lang.Object.wait(Object.java:485)
                  |	at java.util.TimerThread.mainLoop(Timer.java:483)
                  |	- locked <7c45c3490> (a java.util.TaskQueue)
                  |	at java.util.TimerThread.run(Timer.java:462)
                  |""".stripMargin

    val log = JstackLog(lineIteratorFrom(trace))

    log("state") shouldBe "WAITING"
    log("name") shouldBe "FocusManager timer"
    log("depth") shouldBe "4"
    log("frame(0)") shouldBe "java.lang.Object.wait(Native Method)"
    log("frame(1)") shouldBe "java.lang.Object.wait(Object.java:485)"
    log("frame(3)") shouldBe "java.util.TimerThread.run(Timer.java:462)"
    log("frame(4)") shouldBe null
    log("rubbish") shouldBe null
    log.lines shouldBe trace.trim
  }

  it should "handle a single line WAITING trace" in {
    val trace = """"VM Periodic Task Thread" prio=10 tid=11b823800 nid=0x11c610000 waiting on condition"""
    val log = JstackLog(lineIteratorFrom(trace))

    log("name") shouldBe "VM Periodic Task Thread"
    log("state") shouldBe "WAITING"
  }

  it should "handle a single line RUNNABLE trace" in {
    val trace = """"Gang worker#7 (Parallel GC Threads)" prio=9 tid=106801000 nid=0x11640a000 runnable"""
    val log = JstackLog(lineIteratorFrom(trace))

    log("state") shouldBe "RUNNABLE"
  }

  it should "handle a final single line trace" in {
    val trace = """"Exception Catcher Thread" prio=10 tid=106001800 nid=0x105c01000 runnable
                  |JNI global references: 3876
                  |""".stripMargin
    val log = JstackLog(lineIteratorFrom(trace))

    log("state") shouldBe "RUNNABLE"
  }

  it should "parse a whole trace file" in {
    val trace = lineIteratorFrom(contentsOf("jstack.txt"))

    JstackLog(trace)("name") shouldBe "JobScheduler FJ pool 7/8"
     JstackLog(trace)("name") shouldBe "JobScheduler FJ pool 5/8"
  }
}
