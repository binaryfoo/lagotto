package io.github.binaryfoo.lagotto.shell

import java.io.ByteArrayOutputStream

import com.typesafe.config.{ConfigFactory, ConfigValueFactory}
import io.github.binaryfoo.lagotto.{Debug, LagoTest}
import io.github.binaryfoo.lagotto.reader.FileIO

import scala.collection.mutable.ArrayBuffer

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
    val output = run("-f", "socket=10.0.0.1:4321", "--csv", "time,mti,11,delay", "--sort", "delay desc", testFile("a-bunch.xml"))
    output shouldEqual """time,mti,11,delay
                         |00:00:04.992,0210,1,1700
                         |00:00:03.292,0200,1,0
                         |""".stripMargin
  }

  it should "calculate delays when converted to seconds" in {
    val output = run("-f", "socket=10.0.0.1:4321", "--csv", "time,mti,11,(delay millis as seconds)", testFile("a-bunch.xml"))
    output shouldEqual """time,mti,11,(delay millis as seconds)
                         |00:00:03.292,0200,1,0
                         |00:00:04.992,0210,1,1.7
                         |""".stripMargin
  }

  it should "calculate delays when included in aggregate" in {
    val individual = run("--csv", "delay", testFile("a-bunch.xml"))
    individual shouldEqual """delay
                             |0
                             |1000
                             |600
                             |100
                             |""".stripMargin
    val average = run("--csv", "avg(delay)", testFile("a-bunch.xml"))
    average shouldEqual """avg(delay)
                         |425
                         |""".stripMargin
    val bounds = run("--csv", "min(delay),max(delay)", testFile("a-bunch.xml"))
    bounds shouldEqual """min(delay),max(delay)
                         |0,1000
                         |""".stripMargin
  }

  it should "calculate delays for conversion of max(delay)" in {
    val output = run("-f", "socket=10.0.0.1:4321", "--csv", "(max(delay) millis as seconds)", testFile("a-bunch.xml"))
    output shouldEqual """(max(delay) millis as seconds)
                         |1.7
                         |""".stripMargin
  }

  it should "calculate delays for max of converted delay" in {
    val output = run("--csv", "max((delay millis as seconds))", testFile("a-bunch.xml"))
    output shouldEqual """max((delay millis as seconds))
                         |1
                         |""".stripMargin
  }

  it should "calculate delays for aliased delay" in {
    val output = run("--csv", "delay as \"gap\"", testFile("a-pair.xml"))
    output shouldEqual """gap
                         |0
                         |808
                         |47450900
                         |""".stripMargin
  }

  it should "calculate delays between paired results" in {
    val output = run("--pair", "--csv", "req.time,resp.time,rtt,delay", testFile("a-bunch.xml"))
    output shouldEqual """req.time,resp.time,rtt,delay
                         |00:00:04.292,00:00:04.892,600,0
                         |00:00:03.292,00:00:04.992,1700,-1000
                         |""".stripMargin
  }

  it should "calculate delays for aliased delay as seconds" in {
    val output = run("--csv", "(delay ms as s) as \"gap\"", testFile("a-pair.xml"))
    output should include("47450.9")
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

  it should "record all members of a group with group_trace()" in {
    val output = run("--csv", "socket,group_trace(main-test)", testFile("a-bunch.xml"))
    output shouldBe
      """socket,group_trace(main-test)
        |10.0.0.1:4321,main-test.1.log
        |10.0.0.1:4322,main-test.2.log
        |""".stripMargin
    FileIO.readToString("main-test.1.log") should include(FileIO.readLines(testFile("a-bunch.xml"), 3, Some(14)))
    FileIO.readToString("main-test.1.log") should include(FileIO.readLines(testFile("a-bunch.xml"), 42, Some(56)))
    FileIO.readToString("main-test.2.log") should include(FileIO.readLines(testFile("a-bunch.xml"), 15, Some(26)))
    FileIO.readToString("main-test.2.log") should include(FileIO.readLines(testFile("a-bunch.xml"), 27, Some(41)))
    delete("main-test.1.log")
    delete("main-test.2.log")
  }

  it should "record all members of a group with aliased group_trace()" in {
    val output = run("--csv", "socket,(group_trace(main-test) as href) as \"detail\"", testFile("a-bunch.xml"))
    output should include regex """<a href="main-test.1.log" title="main-test.1.log">.*</a>"""
    output should include("socket,detail")
    FileIO.readToString("main-test.1.log") should include(FileIO.readLines(testFile("a-bunch.xml"), 3, Some(14)))
    FileIO.readToString("main-test.2.log") should include(FileIO.readLines(testFile("a-bunch.xml"), 15, Some(26)))
    delete("main-test.1.log")
    delete("main-test.2.log")
  }

  it should "allow sort on count field included in --csv option" in {
    val output = run("--csv", "time(mm:ss),count", "--sort", "count desc", testFile("a-bunch.xml"))
    output shouldEqual """time(mm:ss),count
                         |00:04,3
                         |00:03,1
                         |""".stripMargin
  }

  it should "group rows when max(lifespan) field included in --csv option" in {
    val output = run("--csv", "ipAddress,max(lifespan)", testFile("a-pair.xml"))
    output shouldEqual """ipAddress,max(lifespan)
                          |10.0.0.1,10005
                          |""".stripMargin
  }

  it should "group rows when max(lifespan) is aliased" in {
    val output = run("--csv", "ipAddress,max(lifespan) as \"slowest\"", testFile("a-pair.xml"))
    output shouldEqual """ipAddress,slowest
                          |10.0.0.1,10005
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
"""
  }

  it should "group rows when min(lifespan) field included in --csv option" in {
    val output = run("--csv", "ipAddress,min(lifespan)", testFile("a-pair.xml"))
    output shouldEqual """ipAddress,min(lifespan)
                          |10.0.0.1,1000
                          |""".stripMargin
  }

  it should "group rows when sum(lifespan) field included in --csv option" in {
    val output = run("--csv", "ipAddress,sum(lifespan)", testFile("a-pair.xml"))
    output shouldEqual """ipAddress,sum(lifespan)
                          |10.0.0.1,11005
                          |""".stripMargin
  }

  "calc(a-b)" should "output difference between two aggregate timestamps" in {
    val output = run("--csv", "calc(max(time)-min(time))", testFile("a-bunch.xml"))
    output shouldEqual """calc(max(time)-min(time))
                          |00:00:01.700
                          |""".stripMargin
  }

  it should "output difference between two direct timestamps" in {
    val output = run("--csv", "11,calc(resp.time-req.time)", "--pair", testFile("a-bunch.xml"))
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

  "--names" should "output an extra name attribute in xml" in {
    val output = run("--names", testFile("basic.xml"))
    output should include("""<field id="7" name="Transmission date and time" value="1124000003"/>""")
    output should include("""<field id="11" name="System trace audit number" value="28928"/>""")
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

  "grep(...)" should "create a filter behaving like --grep" in {
    val output = run("-f", "grep(threshold)", testFile("basic.xml"))
    output shouldEqual """<log realm="rotate-log-listener" at="Mon Nov 24 13:10:55 EST 2014">
                         |   maxSize (50000000) threshold reached
                         |</log>
                         |""".stripMargin
  }

  "grep!(...)" should "create a filter behaving like --grep!" in {
    val output = run("-f", "grep!(threshold)", "-t" , "realm", testFile("basic.xml"))
    output shouldEqual """realm
                         |some.channel/10.0.0.1:4321
                         |""".stripMargin
  }

  "igrep(...)" should "create a filter behaving like --igrep" in {
    val output = run("-f", "igrep(ThREsholD)", "-t", "realm", testFile("basic.xml"))
    output shouldEqual """realm
                         |rotate-log-listener
                         |""".stripMargin
  }

  "igrep!(...)" should "create a filter behaving like --grep!" in {
    val output = run("-f", "igrep!(tHREshold)", "-t", "realm", testFile("basic.xml"))
    output shouldEqual """realm
                         |some.channel/10.0.0.1:4321
                         |""".stripMargin
  }

  "count(grep(...))" should "count rows with a text match" in {
    val output = run("--csv", "count(grep(threshold)),count(grep!(threshold))", testFile("a-pair.xml"))
    output shouldEqual """count(grep(threshold)),count(grep!(threshold))
                         |1,2
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

  it should "support aggregation on response time" in {
    val output = run("--pair", "--csv", "max(response.time),min(response.time)", testFile("a-bunch.xml"))
    output shouldBe
      """max(response.time),min(response.time)
        |00:00:04.992,00:00:04.892
        |""".stripMargin
  }

  it should "support aggregation key of response time" in {
    val output = run("--pair", "--csv", "response.time,count", testFile("a-bunch.xml"))
    output shouldBe
      """response.time,count
        |00:00:04.892,1
        |00:00:04.992,1
        |""".stripMargin
  }

  "--sort" should "sort by the chosen field" in {
    val output = run("--sort", "time", "--pair", "--csv", "time,mti,11,rtt", testFile("a-bunch.xml"))
    output shouldEqual """time,mti,11,rtt
                         |00:00:03.292,0200,1,1700
                         |00:00:04.292,0200,2,600
                         |""".stripMargin
  }

  it should "sort by multiple fields" in {
    val output = run("--sort", "11 desc,time", "--csv", "time,11", testFile("a-bunch.xml"))
    output shouldEqual
      """time,11
        |00:00:04.292,2
        |00:00:04.892,2
        |00:00:03.292,1
        |00:00:04.992,1
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

  "--sort X desc" should "reverse sort order" in {
    val output = run("--sort", "11 desc", "--pair", "--csv", "time,mti,11,rtt", testFile("a-bunch.xml"))
    output shouldEqual """time,mti,11,rtt
                         |00:00:04.292,0200,2,600
                         |00:00:03.292,0200,1,1700
                         |""".stripMargin
  }

  it should "allow calc(a-b)>N as a reversed sort" in {
    val output = run("--csv", "mti,calc(max(time(millis))-min(time(millis)))", "--sort", "calc(max(time(millis))-min(time(millis))) desc", testFile("a-bunch.xml"))
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
    val output = run("-f", "48.1~suBfield", "--csv", "time,mti,11,src", testFile("a-pair.xml"))
    output shouldEqual """time,mti,11,src
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
    val error = standardErrorFrom(Main.main(Array("--strict", testFile("message-inception-garbage.xml"))))
    error shouldBe
      """Unexpected <log> start tag. Line message-inception-garbage.xml:5:             <field id="0" value="2804"/><log realm="remoteLink.channel" at="Mon Dec 08 14:51:55 EST 2014.648">
        |""".stripMargin
  }

  "With --digest" should "show full log with fewer characters" in {
    val output = run("--digest", testFile("a-pair.xml"))
    output shouldBe """<log realm="some.channel/10.0.0.1:4321" at="2014-11-24 00:00:03.292" type="send" lifespan="10005">
                      |  0: 0800 (Network Management Request) [mti]
                      |  7: 1124000003 [Transmission date and time]
                      |  11: 28928 [System trace audit number]
                      |  24: 831 [Function code]
                      |</log>
                      |
                      |<log realm="some.channel/10.0.0.1:4321" at="2014-11-24 00:00:04.100" type="receive" lifespan="1000">
                      |  0: 0810 (Network Management Response) [mti]
                      |  7: 1124000003 [Transmission date and time]
                      |  11: 28928 [System trace audit number]
                      |  24: 831 [Function code]
                      |  48.1: subfield 48.1
                      |</log>
                      |
                      |<log realm="rotate-log-listener" at="2014-11-24 13:10:55.000">
                      |
                      |</log>
                      |
                      |""".stripMargin
  }

  it should "unzip gzipped strings" in {
    val config = ConfigFactory.load().withValue("custom.dictionaries.dir", ConfigValueFactory.fromAnyRef("src/test/resources"))
    val output = standardOutFrom { Main.runWith(Array("--digest", testFile("gzip.xml")), config) }
    output should include("259: hello digest [A zipped message]")
  }

  "With --digest-as" should "show full log with even fewer characters" in {
    val output = run("--digest-as", "Snake", testFile("a-pair.xml"))
    output shouldBe """<log realm="some.channel/10.0.0.1:4321" at="2014-11-24 00:00:03.292" type="send" lifespan="10005">
                      |  0: 0800 (Network Management Request) [mti]
                      |  7: 1124000003 [transmission_date_and_time]
                      |  11: 28928 [stan]
                      |  24: 831 [function_code]
                      |</log>
                      |
                      |<log realm="some.channel/10.0.0.1:4321" at="2014-11-24 00:00:04.100" type="receive" lifespan="1000">
                      |  0: 0810 (Network Management Response) [mti]
                      |  7: 1124000003 [transmission_date_and_time]
                      |  11: 28928 [stan]
                      |  24: 831 [function_code]
                      |  48.1: subfield 48.1
                      |</log>
                      |
                      |<log realm="rotate-log-listener" at="2014-11-24 13:10:55.000">
                      |
                      |</log>
                      |
                      |""".stripMargin
  }

  "With --json" should "dump each record as a line of JSON" in {
    val output = run("--json", testFile("a-pair-uyst.xml"))
    output shouldBe """{"at":"2014-11-24T00:00:03.292-0200","lifespan":10005,"realm":"some.channel/10.0.0.1:4321","mti":"0800","transmission_date_and_time":"1124000003","stan":28928,"function_code":"831","msgType":"send"}
                      |{"at":"2014-11-24T00:00:04.100-0200","lifespan":1000,"realm":"some.channel/10.0.0.1:4321","mti":"0810","transmission_date_and_time":"1124000003","stan":28928,"function_code":"831","f_48_1":"subfield 48.1","msgType":"receive"}
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

  "pivoted(sum(count))" should "sum columns in a pivoted row" in {
    val output = run("--csv", "time(mm:ss),pivot(mti),count,sum(pivoted(count))", testFile("pivot-set.xml"))
    output shouldBe """time(mm:ss),0200 - count,0210 - count,sum(pivoted(count))
                      |00:03,2,0,2
                      |00:04,1,1,2
                      |00:05,0,2,2
                      |""".stripMargin
  }

  "log4j log" should "be readable" in {
    val output = run("--csv", "timestamp,message", testFile("log4j.txt"))
    output shouldBe """timestamp,message
                      |2014-11-08 00:00:00.001,Did something useful
                      |2014-11-08 00:00:00.002,And again
                      |This time over two lines
                      |
                      |2014-11-08 00:00:00.003,One more for good measure
                      |""".stripMargin
  }

  it should "expose lines" in {
    val output = run("--no-header", "--csv", "lines(1),count", testFile("log4j.txt"))
    output shouldBe
      """[08 Nov 2014 00:00:00,001] INFO  [a.ClassName]: Did something useful,1
        |[08 Nov 2014 00:00:00,002] INFO  [a.ClassName]: And again,1
        |[08 Nov 2014 00:00:00,003] INFO  [a.ClassName]: One more for good measure,1
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

  "csv" should "be readable with --in-format=csv" in {
    val output = run("--csv", "type,count", "--in-format", "csv", testFile("some.csv"))
    output shouldBe
      """type,count
        |buy,2
        |sell,1
        |""".stripMargin
  }

  "csv" should "without a header row should be readable with --in-format=icsv" in {
    val output = run("--sort", "1 desc", "--in-format", "icsv", testFile("one.icsv"))
    output shouldBe
      """mouse,3
        |fox,2
        |rabbit,1
        |""".stripMargin
  }

  "tsv" should "be readable with --in-format=csv" in {
    val output = run("--csv", "type,count", "--in-format", "tsv", testFile("some.tsv"))
    output shouldBe
      """type,count
        |buy,2
        |sell,1
        |""".stripMargin
  }

  "silly number of files" should "be handled" in {
    val tooManyFiles = (1 to 1025)./:(new ArrayBuffer[String])((buf, i) => buf += s"file_$i.log")
    standardErrorFrom {
      run(tooManyFiles :_*)
    } shouldBe "file_1.log (No such file or directory)\n"
  }

  "auto detection" should "ignore unrecognised lines" in {
    val output = run("--grep!", "Exclude", testFile("mixed-bag.txt"))
    output shouldBe """[08 Nov 2014 00:00:00,001] INFO  [a.ClassName]: Did something useful
                      |[08 Nov 2014 00:00:00,002] INFO  [a.ClassName]: And again
                      |This time over two lines
                      |
                      |<log realm="some.channel/10.0.0.1:4321" at="Mon Nov 24 00:00:03 EST 2014.292" lifespan="10005ms">
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
                      |<log realm="rotate-log-listener" at="Mon Nov 24 13:10:55 EST 2014">
                      |   maxSize (50000000) threshold reached
                      |</log>
                      |[08 Nov 2014 00:00:00,003] INFO  [a.ClassName]: One more for good measure
                      |""".stripMargin
  }

  "--join" should "pair up rows on the numbered field" in {
    val output = run("--in-format", "icsv", "--join", "0", testFile("one.icsv"), testFile("two.icsv"))
    output shouldBe
      """rabbit,1,green
        |fox,2,yellow
        |mouse,3,
        |elephant,red,
        |""".stripMargin
  }

  it should "pair up rows on a named header field" in {
    val output = run("--in-format", "csv", "--join", "animal", testFile("one.csv"), testFile("two.csv"))
    output shouldBe
      """rabbit,1,green
        |fox,2,yellow
        |mouse,3,
        |elephant,red,
        |""".stripMargin
  }

  it should "pair jpos entries on realm and stan" in {
    val output = run("--csv", "realm,11,48.1,44.1", "--join", "link,11", testFile("a-bunch.xml"))
    output shouldBe """realm,11,48.1,44.1
                      |some.channel/10.0.0.1:4322,2,a-bunch.xml #2,response to 2
                      |some.channel/10.0.0.1:4321,1,a-bunch.xml #1,response to 1
                      |""".stripMargin
  }

  it should "pick first receive after session-start" in {
    val output = run("--csv", "right.time,socket,rtt", "--join", "socket", "-f", "left.msgType=session-start", "-f", "right.msgType=receive", testFile("channel-birth.xml"))
    output shouldBe
      """right.time,socket,rtt
        |00:00:05.292,10.0.0.2:4321,1000
        |""".stripMargin
  }

  "--inner-join" should "show only paired up rows" in {
    val output = run("--in-format", "csv", "--inner-join", "animal", testFile("one.csv"), testFile("two.csv"))
    output shouldBe
      """rabbit,1,green
        |fox,2,yellow
        |""".stripMargin
  }

  "in operator" should "work in count()" in {
    val output = run("--csv", "count(ipAddress in (10.0.0.1)) as \"localIp\",count(port in (4322)) as \"p1\"", testFile("a-bunch.xml"))
    output shouldBe
      """localIp,p1
        |4,2
        |""".stripMargin
  }

  "src" should "render as an href for --html" in {
    val output = run("--html", "src", testFile("basic.xml"))
    output should include regex """<a href="[^"]*/src/test/resources/basic.xml\?from=2&to=13&format=named".*basic.xml:3.*</a>"""
  }

  it should "render hrefs for aggregate results" in {
    val output = run("--html", "group_concat(src)", testFile("a-pair.xml"))
    output should include regex """<a href="[^"]*/src/test/resources/a-pair.xml\?from=2&to=13&format=named".*a-pair.xml:3.*</a>,<a href="[^"]*/src/test/resources/a-pair.xml\?from=13&to=27&format=named".*a-pair.xml:14.*</a>"""
  }

  "channelWith() operator" should "pick channel where a previous message matched" in {
    val output = run("--csv", "48.1", "-f", "channelWith(4=10001)", testFile("a-bunch.xml"))
    output shouldBe
      """48.1
        |a-bunch.xml #1
        |a-bunch.xml #4
        |""".stripMargin
  }

  "jpos entry within log4j entry" should "be accessible" in {
    val output = run("--csv", "jpos.ipAddress,jpos.exception", testFile("bridged-log4j.txt"))
    output should include("172.0.1.7,Received fatal alert: bad_record_mac")
  }

  "jpos.exception" should "be countable" in {
    val output = run("--csv", "count(jpos.exception~bad)", testFile("bridged-log4j.txt"))
    output should include("1")
  }

  "jpos.exception" should "be regex'able and aliasable" in {
    val output = run("--csv", "jpos.exception(/ .*//) as \"thing\",count", testFile("bridged-log4j.txt"))
    output should(include("Received,1") and include("thing,count"))
  }

  "length" should "show length of field" in {
    val output = run("--csv", "7,length(7)", testFile("basic.xml"))
    output should include("1124000003,10")
  }

  "elapsed" should "show time since the first entry" in {
    val output = run("--csv", "time,elapsed", testFile("a-bunch.xml"))
    output shouldBe
      """time,elapsed
        |00:00:03.292,0
        |00:00:04.292,1000
        |00:00:04.892,1600
        |00:00:04.992,1700
        |""".stripMargin
  }

  "elapsedSince" should "show time since the previous entry matching a filter" in {
    val output = run("--csv", "time,elapsedSince(48.1~#2)", testFile("a-bunch.xml"))
    output shouldBe
      """time,elapsedSince(48.1~#2)
        |00:00:03.292,0
        |00:00:04.292,0
        |00:00:04.892,600
        |00:00:04.992,700
        |""".stripMargin
  }

  "count(if(...))" should "count if condition matches" in {
    val output = run("--csv", "count(distinct(if(mti=0200,port,))),mti", testFile("a-bunch.xml"))
    output should (include("2,0200") and include("0,0210"))
  }

  "distinct(field)" should "output each value only once" in {
    val output = run("--no-header", "--csv", "distinct(mti)", testFile("a-bunch.xml"))
    output shouldBe
      """0200
        |0210
        |""".stripMargin
  }

  "--sqlIn output format" should "output a SQL in clause" in {
    val output = run("--sqlIn", "distinct(ipAddress)", testFile("a-bunch.xml"))
    output shouldBe
      """(
        |'10.0.0.1'
        |)
        |""".stripMargin
  }

  "--merge" should "remove duplicates from input" in {
    val output = run("--merge", testFile("one.xml"), testFile("two.xml"), testFile("one.xml"))
    output shouldBe contentsOf("one.xml") + contentsOf("two.xml")
  }

  "--highlight" should "use colours" in {
    val output = run("--highlight", "yes", testFile("one.xml"))
    output shouldBe contentsOf("expected-ansi-one.txt")
  }

  private def run(args: String*): String = standardOutFrom { Main.main(args.toArray) }

  private def standardOutFrom(thunk: => Unit): String = {
    val out = new ByteArrayOutputStream()
    Console.withOut(out)(thunk)
    out.toString
  }

  private def standardErrorFrom(thunk: => Unit): String = {
    val out = new ByteArrayOutputStream()
    Console.withErr(out)(thunk)
    out.toString
  }
}
