package io.github.binaryfoo.lagotto.shell

import java.io.ByteArrayOutputStream

import io.github.binaryfoo.lagotto.LagoTest

class MainTest extends LagoTest {

  "Main" should "produce tab separated output with --tsv option" in {
    val output = run("--tsv", "time,mti,11", testFile("a-bunch.xml"))
    output shouldEqual """time	mti	11
                         |00:00:03.292	0200	1
                         |00:00:04.292	0200	2
                         |00:00:04.892	0210	2
                         |00:00:04.992	0210	1
                         |""".stripMargin
  }

  it should "produce jira formatted table with --jira-table option" in {
    val output = run("--jira-table", "time,mti,11", testFile("a-bunch.xml"))
    output shouldEqual """||time||mti||11||
|00:00:03.292|0200|1|
|00:00:04.292|0200|2|
|00:00:04.892|0210|2|
|00:00:04.992|0210|1|
"""
  }

  it should "produce a HTML formatted table with --html option" in {
    val output = run("--html", "time,mti,11", testFile("a-bunch.xml"))
    output should include ("<table>")
    output should include ("<tr><th>time</th><th>mti</th><th>11</th></tr>")
    output should include ("<tr><td>00:00:03.292</td><td>0200</td><td>1</td></tr>")
    output should include ("</table>")
  }

  it should "include delay between messages with delay field in --csv option" in {
    val output = run("-f", "socket=10.0.0.1:4321", "--csv", "time,mti,11,delay", testFile("a-bunch.xml"))
    output shouldEqual """time,mti,11,delay
                         |00:00:03.292,0200,1,0
                         |00:00:04.992,0210,1,1700
                         |""".stripMargin
  }

  it should "allow filtering on delay between messages" in {
    val output = run("-f", "socket=10.0.0.1:4321", "-f", "delay>1000", "--csv", "time,mti,11,delay", testFile("a-bunch.xml"))
    output shouldEqual """time,mti,11,delay
                         |00:00:04.992,0210,1,1700
                         |""".stripMargin
  }

  it should "still filter on delay between messages when it's not included in the field list" in {
    val output = run("-f", "socket=10.0.0.1:4321", "-f", "delay>1000", "--csv", "time,mti,11", testFile("a-bunch.xml"))
    output shouldEqual """time,mti,11
                         |00:00:04.992,0210,1
                         |""".stripMargin
  }

  it should "allow sort on delay between messages" in {
    val output = run("-f", "socket=10.0.0.1:4321", "--csv", "time,mti,11,delay", "--sort-desc", "delay", testFile("a-bunch.xml"))
    output shouldEqual """time,mti,11,delay
                         |00:00:04.992,0210,1,1700
                         |00:00:03.292,0200,1,0
                         |""".stripMargin
  }

  it should "group rows when count field included in --csv option" in {
    val output = run("--csv", "time(mm:ss),count", testFile("a-bunch.xml"))
    output shouldEqual """time(mm:ss),count
                          |00:03,1
                          |00:04,3
                          |""".stripMargin
  }

  it should "group rows on reformatted calc() expression" in {
    val output = run("--csv", "(calc(time-lifespan) time as time(HH:mm:ss)),count", testFile("a-bunch.xml"))
    output shouldEqual """(calc(time-lifespan) time as time(HH:mm:ss)),count
                          |,2
                          |00:00:04,2
                          |""".stripMargin
  }

  it should "filter on group count with --field count>N" in {
    val output = run("--csv", "time(mm:ss),count", "--field", "count>2", testFile("a-bunch.xml"))
    output shouldEqual """time(mm:ss),count
                          |00:04,3
                          |""".stripMargin
  }

  it should "still filter on group count>N if it's not in the --field list" in {
    val output = run("--csv", "time(mm:ss)", "--field", "count>2", testFile("a-bunch.xml"))
    output shouldEqual """time(mm:ss)
                          |00:04
                          |""".stripMargin
  }

  it should "filter on group count(condition) with --field count>N" in {
    val output = run("--csv", "time(mm:ss),count(time(mm:ss)=00:03)", "--field", "count(time(mm:ss)=00:03)>1", testFile("a-bunch.xml"))
    output shouldEqual """time(mm:ss),count(time(mm:ss)=00:03)
                          |00:03,1
                          |""".stripMargin
  }

  it should "count only if condition is true for count(39=01) in --csv option" in {
    val output = run("--csv", "time(mm:ss),count(39=01),count(39=00)", testFile("a-bunch.xml"))
    output shouldEqual """time(mm:ss),count(39=01),count(39=00)
                          |00:03,0,0
                          |00:04,1,1
                          |""".stripMargin
  }

  it should "support mysql like group_concat() in --csv option" in {
    val output = run("--csv", "time(mm:ss),group_concat(48.1(/.*#//))", testFile("a-bunch.xml"))
    output shouldEqual """time(mm:ss),group_concat(48.1(/.*#//))
                         |00:03,1
                         |00:04,2,3,4
                         |""".stripMargin
  }

  it should "allow sort on count field included in --csv option" in {
    val output = run("--csv", "time(mm:ss),count", "--sort-desc", "count", testFile("a-bunch.xml"))
    output shouldEqual """time(mm:ss),count
                         |00:04,3
                         |00:03,1
                         |""".stripMargin
  }

  it should "group rows when max(lifespan) field included in --csv option" in {
    val output = run("--csv", "ipAddress,max(lifespan)", testFile("a-pair.xml"))
    output shouldEqual """ipAddress,max(lifespan)
                          |10.0.0.1,10005
                          |,
                          |""".stripMargin
  }

  it should "group rows when min(calc(timestamp-lifespan)) field included in --csv option" in {
    val output = run("--csv", "min(calc(timestamp-lifespan))", testFile("a-pair.xml"))
    output shouldEqual """min(calc(timestamp-lifespan))
                          |2014-11-23 23:59:53.287
                          |""".stripMargin
  }

  it should "group rows when max(lifespan) field included in --jira-table option" in {
    val output = run("--jira-table", "ipAddress,max(lifespan)", testFile("a-pair.xml"))
    output shouldEqual """||ipAddress||max(lifespan)||
|10.0.0.1|10005|
|||
"""
  }

  it should "group rows when min(lifespan) field included in --csv option" in {
    val output = run("--csv", "ipAddress,min(lifespan)", testFile("a-pair.xml"))
    output shouldEqual """ipAddress,min(lifespan)
                          |10.0.0.1,1000
                          |,
                          |""".stripMargin
  }

  it should "group rows when sum(lifespan) field included in --csv option" in {
    val output = run("--csv", "ipAddress,sum(lifespan)", testFile("a-pair.xml"))
    output shouldEqual """ipAddress,sum(lifespan)
                          |10.0.0.1,11005
                          |,
                          |""".stripMargin
  }

  "calc(a-b)" should "output difference between two aggregate timestamps" in {
    val output = run("--csv", "calc(max(time)-min(time))", testFile("a-bunch.xml"))
    output shouldEqual """calc(max(time)-min(time))
                          |00:00:01.700
                          |""".stripMargin
  }

  it should "output difference between two direct timestamps" in {
    val output = run("--csv", "11,calc(resp.time-req.time)","--pair", testFile("a-bunch.xml"))
    output shouldEqual """11,calc(resp.time-req.time)
                         |2,00:00:00.600
                         |1,00:00:01.700
                         |""".stripMargin
  }

  it should "output difference between an aggregate timestamp and an aggregate millis value" in {
    val output = run("--csv", "calc(max(time)-max(lifespan))", testFile("a-bunch.xml"))
    output shouldEqual """calc(max(time)-max(lifespan))
                          |00:00:04.892
                          |""".stripMargin
  }

  it should "output difference between a direct timestamp and a direct millis value" in {
    val output = run("--csv", "48.1,calc(time-lifespan)", testFile("a-bunch.xml"))
    output shouldEqual """48.1,calc(time-lifespan)
                          |a-bunch.xml #1,
                          |a-bunch.xml #2,
                          |a-bunch.xml #3,00:00:04.882
                          |a-bunch.xml #4,00:00:04.892
                          |""".stripMargin
  }

  "Conversion (min(lifespan) as time(ss)" should "show minimum lifespan in seconds" in {
    val output = run("--csv", "ipAddress,(min(lifespan) as time(s))", testFile("a-pair.xml"))
    output shouldEqual """ipAddress,(min(lifespan) as time(s))
                         |10.0.0.1,1
                         |,
                         |""".stripMargin
  }

  "calc(a-b)" should "output difference between two timestamps in custom format" in {
    val output = run("--csv", "calc(max(time(mm:ss))-min(time(mm:ss)))", testFile("a-bunch.xml"))
    output shouldEqual """calc(max(time(mm:ss))-min(time(mm:ss)))
                          |00:01
                          |""".stripMargin
  }

  "calc(a/b)" should "output ratio between counts of message types" in {
    val output = run("--csv", "calc(count(mti=0200)/count)", testFile("a-bunch.xml"))
    output shouldEqual """calc(count(mti=0200)/count)
                          |0.5000
                          |""".stripMargin
  }

  "--field" should "allow calc(a-b)>N as a filter" in {
    val output = run("--csv", "mti,calc(max(time(millis))-min(time(millis)))", "--field", "calc(max(time(millis))-min(time(millis)))>101", testFile("a-bunch.xml"))
    output shouldEqual """mti,calc(max(time(millis))-min(time(millis)))
                         |0200,1000
                         |""".stripMargin
  }

  "--no-header" should "leave out the header" in {
    val output = run("--no-header", "--csv", "time,mti,11", testFile("basic.xml"))
    output shouldEqual """00:00:03.292,0800,28928
                         |13:10:55.000,,
                         |""".stripMargin
  }

  it should "filter on exact field match with -f option" in {
    val output = run("-f", "11=28928", testFile("basic.xml"))
    output shouldEqual """<log realm="some.channel/10.0.0.1:4321" at="Mon Nov 24 00:00:03 EST 2014.292" lifespan="10005ms">
                         |  <receive>
                         |    <isomsg direction="incoming">
                         |      <!-- org.jpos.iso.packager.XMLPackager -->
                         |      <field id="0" value="0800"/>
                         |      <field id="7" value="1124000003"/>
                         |      <field id="11" value="28928"/>
                         |      <field id="24" value="831"/>
                         |    </isomsg>
                         |  </receive>
                         |</log>
                         |""".stripMargin
  }

  "--pair" should "produce pair entries" in {
    val output = run("--pair", "--csv", "time,mti,11,rtt", testFile("a-bunch.xml"))
    // output ends up in ordered by response received time...
    output shouldEqual """time,mti,11,rtt
                         |00:00:04.292,0200,2,600
                         |00:00:03.292,0200,1,1700
                         |""".stripMargin
  }

  "--sort" should "sort by the chosen field" in {
    val output = run("--sort", "time", "--pair", "--csv", "time,mti,11,rtt", testFile("a-bunch.xml"))
    output shouldEqual """time,mti,11,rtt
                         |00:00:03.292,0200,1,1700
                         |00:00:04.292,0200,2,600
                         |""".stripMargin
  }

  it should "allow calc(a-b)>N as a sort" in {
    val output = run("--csv", "mti,calc(max(time(millis))-min(time(millis)))", "--sort", "calc(max(time(millis))-min(time(millis)))", testFile("a-bunch.xml"))
    output shouldEqual """mti,calc(max(time(millis))-min(time(millis)))
                         |0210,100
                         |0200,1000
                         |""".stripMargin
  }

  it should "allow a data dictionary name as the sort field" in {
    val output = run("--sort", "stan", "--csv", "11,48.1", testFile("a-bunch.xml"))
    output shouldEqual """11,48.1
                         |1,a-bunch.xml #1
                         |1,a-bunch.xml #4
                         |2,a-bunch.xml #2
                         |2,a-bunch.xml #3
                         |""".stripMargin
  }

  "--sort-desc" should "reverse sort order" in {
    val output = run("--sort-desc", "11", "--pair", "--csv", "time,mti,11,rtt", testFile("a-bunch.xml"))
    output shouldEqual """time,mti,11,rtt
                         |00:00:04.292,0200,2,600
                         |00:00:03.292,0200,1,1700
                         |""".stripMargin
  }

  it should "allow calc(a-b)>N as a reversed sort" in {
    val output = run("--csv", "mti,calc(max(time(millis))-min(time(millis)))", "--sort-desc", "calc(max(time(millis))-min(time(millis)))", testFile("a-bunch.xml"))
    output shouldEqual """mti,calc(max(time(millis))-min(time(millis)))
                         |0200,1000
                         |0210,100
                         |""".stripMargin
  }

  "-f" should "filter using greater than operator with -f rtt>1000 option" in {
    val output = run("--pair", "-f", "rtt>1000", "--csv", "time,mti,11,rtt", testFile("a-bunch.xml"))
    output shouldEqual """time,mti,11,rtt
                         |00:00:03.292,0200,1,1700
                         |""".stripMargin
  }

  it should "filter using less than operator with -f rtt<1000 option" in {
    val output = run("--pair", "-f", "rtt<1000", "--csv", "time,mti,11,rtt", testFile("a-bunch.xml"))
    output shouldEqual """time,mti,11,rtt
                         |00:00:04.292,0200,2,600
                         |""".stripMargin
  }

  it should "filter using case insensitive contains operator with -f 48.1~suBfield option" in {
    val output = run("-f", "48.1~suBfield", "--csv", "time,mti,11,file", testFile("a-pair.xml"))
    output shouldEqual """time,mti,11,file
                         |00:00:04.100,0810,28928,a-pair.xml:14
                         |""".stripMargin
  }

  it should "negate filter with !in -f 24!=831 option" in {
    val output = run("-f", "24!=831", testFile("a-pair.xml"))
    output shouldEqual """<log realm="rotate-log-listener" at="Mon Nov 24 13:10:55 EST 2014">
                         |   maxSize (50000000) threshold reached
                         |</log>
                         |""".stripMargin
  }

  it should "filter using a field name from the data dictionary" in {
    val output = run("-f", "stan=1", "--csv", "48.1", testFile("a-bunch.xml"))
    output shouldBe """48.1
                      |a-bunch.xml #1
                      |a-bunch.xml #4
                      |""".stripMargin
  }

  it should "allow names from the data dictionary in --csv field list" in {
    val output = run("--csv", "stan,48.1", testFile("a-bunch.xml"))
    output shouldBe """stan,48.1
                      |1,a-bunch.xml #1
                      |2,a-bunch.xml #2
                      |2,a-bunch.xml #3
                      |1,a-bunch.xml #4
                      |""".stripMargin
  }

  it should "include records matching all -f filters" in {
    val output = run("-f", "48.1~bunch", "-f", "at~.892", "--csv", "48.1", "--no-header", testFile("a-bunch.xml"))
    output shouldEqual
      """a-bunch.xml #3
        |""".stripMargin
  }

  "--grep" should "match anywhere in the message" in {
    val output = run("--grep", "threshold", testFile("a-pair.xml"))
    output shouldEqual """<log realm="rotate-log-listener" at="Mon Nov 24 13:10:55 EST 2014">
                         |   maxSize (50000000) threshold reached
                         |</log>
                         |""".stripMargin
  }

  it should "negate --grep with --grep! option" in {
    val output = run("--grep!", "threshold", testFile("a-pair.xml"))
    output shouldEqual """<log realm="some.channel/10.0.0.1:4321" at="Mon Nov 24 00:00:03 EST 2014.292" lifespan="10005ms">
                         |  <send>
                         |    <isomsg direction="outgoing">
                         |      <!-- org.jpos.iso.packager.XMLPackager -->
                         |      <field id="0" value="0800"/>
                         |      <field id="7" value="1124000003"/>
                         |      <field id="11" value="28928"/>
                         |      <field id="24" value="831"/>
                         |    </isomsg>
                         |  </send>
                         |</log>
                         |<log realm="some.channel/10.0.0.1:4321" at="Mon Nov 24 00:00:04 EST 2014.100" lifespan="1000ms">
                         |  <receive>
                         |    <isomsg direction="incoming">
                         |      <!-- org.jpos.iso.packager.XMLPackager -->
                         |      <field id="0" value="0810"/>
                         |      <field id="7" value="1124000003"/>
                         |      <field id="11" value="28928"/>
                         |      <field id="24" value="831"/>
                         |      <isomsg id="48">
                         |        <field id="1" value="subfield 48.1"/>
                         |      </isomsg>
                         |    </isomsg>
                         |  </receive>
                         |</log>
                         |""".stripMargin
  }

  "--igrep" should "match anywhere in the message ignoring case" in {
    val output = run("--igrep", "THRESH", testFile("a-pair.xml"))
    output shouldEqual """<log realm="rotate-log-listener" at="Mon Nov 24 13:10:55 EST 2014">
                         |   maxSize (50000000) threshold reached
                         |</log>
                         |""".stripMargin
  }

  it should "negate with --igrep!" in {
    val output = run("--igrep!", "THRESH", "--csv", "msgType", "--no-header", testFile("a-pair.xml"))
    output shouldEqual """send
                         |receive
                         |""".stripMargin
  }


  "Main" should "print full text by default" in {
    val output = run(testFile("a-pair.xml"))
    output shouldEqual """<log realm="some.channel/10.0.0.1:4321" at="Mon Nov 24 00:00:03 EST 2014.292" lifespan="10005ms">
                         |  <send>
                         |    <isomsg direction="outgoing">
                         |      <!-- org.jpos.iso.packager.XMLPackager -->
                         |      <field id="0" value="0800"/>
                         |      <field id="7" value="1124000003"/>
                         |      <field id="11" value="28928"/>
                         |      <field id="24" value="831"/>
                         |    </isomsg>
                         |  </send>
                         |</log>
                         |<log realm="some.channel/10.0.0.1:4321" at="Mon Nov 24 00:00:04 EST 2014.100" lifespan="1000ms">
                         |  <receive>
                         |    <isomsg direction="incoming">
                         |      <!-- org.jpos.iso.packager.XMLPackager -->
                         |      <field id="0" value="0810"/>
                         |      <field id="7" value="1124000003"/>
                         |      <field id="11" value="28928"/>
                         |      <field id="24" value="831"/>
                         |      <isomsg id="48">
                         |        <field id="1" value="subfield 48.1"/>
                         |      </isomsg>
                         |    </isomsg>
                         |  </receive>
                         |</log>
                         |<log realm="rotate-log-listener" at="Mon Nov 24 13:10:55 EST 2014">
                         |   maxSize (50000000) threshold reached
                         |</log>
                         |""".stripMargin
  }

  it should "include N messages preceding a match with -B N option" in {
    val output = run("-B", "1", "--csv", "48.1", "--no-header", "--grep", "#3", testFile("a-bunch.xml"))
    output shouldEqual
      """a-bunch.xml #2
        |a-bunch.xml #3
        |""".stripMargin
  }

  it should "include N messages following a match with -A N option" in {
    val output = run("-A", "1", "--csv", "48.1", "--no-header", "--grep", "#2", testFile("a-bunch.xml"))
    output shouldEqual
      """a-bunch.xml #2
        |a-bunch.xml #3
        |""".stripMargin
  }

  it should "include N messages before and after a match with -C N option" in {
    val output = run("-C", "1", "--csv", "48.1", "--no-header", "--grep", "#3", testFile("a-bunch.xml"))
    output shouldEqual
      """a-bunch.xml #2
        |a-bunch.xml #3
        |a-bunch.xml #4
        |""".stripMargin
  }

  it should "includes preceding context only once with -B N option" in {
    val output = run("-B", "1", "--csv", "48.1", "--no-header", "--field", "at~00:00:04", testFile("a-bunch.xml"))
    output shouldEqual
      """a-bunch.xml #1
        |a-bunch.xml #2
        |a-bunch.xml #3
        |a-bunch.xml #4
        |""".stripMargin
  }

  it should "print wrap pairs in <pair></pair> with --pair" in {
    val output = run("--pair",testFile("a-pair.xml"))
    output shouldEqual """<pair>
                         |<log realm="some.channel/10.0.0.1:4321" at="Mon Nov 24 00:00:03 EST 2014.292" lifespan="10005ms">
                         |  <send>
                         |    <isomsg direction="outgoing">
                         |      <!-- org.jpos.iso.packager.XMLPackager -->
                         |      <field id="0" value="0800"/>
                         |      <field id="7" value="1124000003"/>
                         |      <field id="11" value="28928"/>
                         |      <field id="24" value="831"/>
                         |    </isomsg>
                         |  </send>
                         |</log>
                         |<log realm="some.channel/10.0.0.1:4321" at="Mon Nov 24 00:00:04 EST 2014.100" lifespan="1000ms">
                         |  <receive>
                         |    <isomsg direction="incoming">
                         |      <!-- org.jpos.iso.packager.XMLPackager -->
                         |      <field id="0" value="0810"/>
                         |      <field id="7" value="1124000003"/>
                         |      <field id="11" value="28928"/>
                         |      <field id="24" value="831"/>
                         |      <isomsg id="48">
                         |        <field id="1" value="subfield 48.1"/>
                         |      </isomsg>
                         |    </isomsg>
                         |  </receive>
                         |</log>
                         |</pair>
                         |""".stripMargin
  }

  it should "filter on lifespan greater than a number" in {
    val output = run("--field", "lifespan>1001", "--csv", "mti,lifespan", testFile("a-pair.xml"))
    output shouldBe
      """mti,lifespan
        |0800,10005
        |""".stripMargin
  }

  it should "filter on lifespan less than a number" in {
    val output = run("--field", "lifespan<1000", "--csv", "mti,lifespan", testFile("a-pair.xml"))
    output shouldBe
      """mti,lifespan
        |0810,1000
        |,
        |""".stripMargin
  }

  "Given a <log> record garbled by a 2nd partial within it" should "keep on truckin'" in {
    val output = run("--csv", "0,11", testFile("message-inception-garbage.xml"))
    output shouldEqual """0,11
                          |2804,
                          |2814,127
                          |""".stripMargin
  }

  "With --strict garbled input" should "be rejected" in {
    the [IllegalArgumentException] thrownBy {
      run("--strict", testFile("message-inception-garbage.xml"))
    } should have message """Unexpected <log> start tag. Line message-inception-garbage.xml:5:             <field id="0" value="2804"/><log realm="remoteLink.channel" at="Mon Dec 08 14:51:55 EST 2014.648">"""
  }

  "With --digest" should "show full log with fewer characters" in {
    val output = run("--digest", testFile("a-pair.xml"))
    output shouldBe """<log realm="some.channel/10.0.0.1:4321" at="2014-11-24 00:00:03.292" type="send" lifespan="10005">
                      |  0 (mti): 0800 (Network Management Request)
                      |  7 (Transmission date and time): 1124000003
                      |  11 (System trace audit number): 28928
                      |  24 (Function code): 831
                      |<log realm="some.channel/10.0.0.1:4321" at="2014-11-24 00:00:04.100" type="receive" lifespan="1000">
                      |  0 (mti): 0810 (Network Management Response)
                      |  7 (Transmission date and time): 1124000003
                      |  11 (System trace audit number): 28928
                      |  24 (Function code): 831
                      |  48.1: subfield 48.1
                      |<log realm="rotate-log-listener" at="2014-11-24 13:10:55.000">
                      |
                      |""".stripMargin
  }

// TODO deal with global state vs concurrent tests ...
//  it should "unzip gzipped strings" in {
//    System.setProperty("custom.dictionaries.dir", "src/test/resources")
//    val output = run("--digest", testFile("gzip.xml"))
//    output should include("259 (A zipped message): hello digest")
//    System.clearProperty("custom.dictionaries.dir")
//  }

  "With --digest-as" should "show full log with even fewer characters" in {
    val output = run("--digest-as", "Export", testFile("a-pair.xml"))
    output shouldBe """<log realm="some.channel/10.0.0.1:4321" at="2014-11-24 00:00:03.292" type="send" lifespan="10005">
                      |  0 (mti): 0800 (Network Management Request)
                      |  7 (transmissionDateAndTime): 1124000003
                      |  11 (stan): 28928
                      |  24 (functionCode): 831
                      |<log realm="some.channel/10.0.0.1:4321" at="2014-11-24 00:00:04.100" type="receive" lifespan="1000">
                      |  0 (mti): 0810 (Network Management Response)
                      |  7 (transmissionDateAndTime): 1124000003
                      |  11 (stan): 28928
                      |  24 (functionCode): 831
                      |  48.1: subfield 48.1
                      |<log realm="rotate-log-listener" at="2014-11-24 13:10:55.000">
                      |
                      |""".stripMargin
  }

  "With --json" should "dump each record as a line of JSON" in {
    val output = run("--json", testFile("a-pair-uyst.xml"))
    output shouldBe """{"at":"2014-11-24T00:00:03.292-0200","lifespan":10005,"realm":"some.channel/10.0.0.1:4321","msgType":"send","mti":"0800","transmissionDateAndTime":"1124000003","stan":28928,"functionCode":"831"}
                      |{"at":"2014-11-24T00:00:04.100-0200","lifespan":1000,"realm":"some.channel/10.0.0.1:4321","msgType":"receive","mti":"0810","transmissionDateAndTime":"1124000003","stan":28928,"functionCode":"831","48.1":"subfield 48.1"}
                      |{"at":"2014-11-24T13:10:55.000-0200","realm":"rotate-log-listener"}
                      |""".stripMargin
  }

  "translate()" should "lookup value in the data dictionary" in {
    val output = run("--csv", "time,translate(0)", testFile("a-pair.xml"))
    output shouldBe """time,translate(0)
                      |00:00:03.292,Network Management Request
                      |00:00:04.100,Network Management Response
                      |13:10:55.000,
                      |""".stripMargin
  }

  "group_concat(translate(0))" should "be aggregate the translations" in {
    val output = run("--csv", "group_concat(translate(0))", testFile("a-pair.xml"))
    output shouldBe """group_concat(translate(0))
                      |Network Management Request,Network Management Response
                      |""".stripMargin
  }

  "exception with bad XML" should "not stop the show" in {
    val output = run(testFile("exception-with-bad-xml.xml"))
    output.trim shouldBe contentsOf("exception-with-bad-xml.xml")
  }

  "Option -n 1" should "limit output to only one entry" in {
    val output = run("-n", "1", testFile("a-bunch.xml"))
    output shouldBe """    <log realm="some.channel/10.0.0.1:4321" at="Mon Nov 24 00:00:03 EST 2014.292">
                      |        <send>
                      |            <isomsg direction="outgoing">
                      |                <field id="0" value="0200"/>
                      |                <field id="4" value="10001"/>
                      |                <field id="11" value="1"/>
                      |                <isomsg id="48">
                      |                    <field id="1" value="a-bunch.xml #1"/>
                      |                </isomsg>
                      |            </isomsg>
                      |        </send>
                      |    </log>
                      |""".stripMargin
  }

  it should "apply limit for table formats too" in {
    val output = run("-n", "1", "--csv", "48.1", "--no-header", testFile("a-bunch.xml"))
    output shouldBe "a-bunch.xml #1\n"
  }

  it should "have long form --limit" in {
    val output = run("--limit", "2", "--csv", "48.1", "--no-header", testFile("a-bunch.xml"))
    output shouldBe
      """a-bunch.xml #1
        |a-bunch.xml #2
        |""".stripMargin
  }

  "pivot(mti)" should "do a basic pivot table operation" in {
    val output = run("--csv", "time(mm:ss),pivot(mti),count", testFile("pivot-set.xml"))
    output shouldBe """time(mm:ss),0200 - count,0210 - count
                      |00:03,2,0
                      |00:04,1,1
                      |00:05,0,2
                      |""".stripMargin
  }

  "log4j log" should "be readable" in {
    val output = run("--csv", "timestamp,message", testFile("log4j.txt"))
    output shouldBe """timestamp,message
                      |08 Nov 2014 00:00:00,001,Did something useful
                      |08 Nov 2014 00:00:00,002,And again
                      |This time over two lines
                      |
                      |08 Nov 2014 00:00:00,003,One more for good measure
                      |""".stripMargin
  }

  "apache log" should "be readable with --in-format=apache option" in {
    val output = run("--csv", "time,url", "--in-format", "apache", testFile("apache.txt"))
    output shouldBe
      """time,url
        |00:00:55.000,GET /some/url
        |13:01:55.000,GET /another/url
        |""".stripMargin
  }

  "gc log" should "be readable with --in-format=gc option" in {
    val output = run("--csv", "time,pause,before,after,heap", "--in-format", "gc", testFile("gc.log"))
    output shouldBe
      """time,pause,before,after,heap
        |10:18:13.300,2.42,2804197,1454927,4170176
        |15:16:55.969,2.37,2798349,1171035,4166784
        |""".stripMargin
  }

  def run(args: String*): String = {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      Main.main(args.toArray)
    }
    out.toString
  }
}
