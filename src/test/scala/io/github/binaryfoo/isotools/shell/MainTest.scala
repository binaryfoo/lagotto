package io.github.binaryfoo.isotools.shell

import java.io.ByteArrayOutputStream

import org.scalatest.{FlatSpec, Matchers}

class MainTest extends FlatSpec with Matchers {

  "Main" should "produce tab separated output with --tsv option" in {
    val output = run("--tsv", "time,mti,11", "src/test/resources/a-bunch.xml")
    output shouldEqual """time	mti	11
                         |00:00:03.292	0200	1
                         |00:00:04.292	0200	2
                         |00:00:04.892	0210	2
                         |00:00:04.992	0210	1
                         |""".stripMargin
  }

  it should "leave out the header with --no-header option" in {
    val output = run("--no-header", "--csv", "time,mti,11", "src/test/resources/basic.xml")
    output shouldEqual """00:00:03.292,0800,28928
                         |13:10:55.000,null,null
                         |""".stripMargin
  }

  it should "filter on exact field match with -f option" in {
    val output = run("-f", "11=28928", "src/test/resources/basic.xml")
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

  it should "produce pair entries with --pair option" in {
    val output = run("--pair", "--csv", "time,mti,11,rtt", "src/test/resources/a-bunch.xml")
    // output ends up in ordered by response received time...
    output shouldEqual """time,mti,11,rtt
                         |00:00:04.292,0200,2,600
                         |00:00:03.292,0200,1,1700
                         |""".stripMargin
  }

  it should "sort by --sort option" in {
    val output = run("--sort", "time", "--pair", "--csv", "time,mti,11,rtt", "src/test/resources/a-bunch.xml")
    output shouldEqual """time,mti,11,rtt
                         |00:00:03.292,0200,1,1700
                         |00:00:04.292,0200,2,600
                         |""".stripMargin
  }

  it should "reverse sort with --sort-desc option" in {
    val output = run("--sort-desc", "11", "--pair", "--csv", "time,mti,11,rtt", "src/test/resources/a-bunch.xml")
    output shouldEqual """time,mti,11,rtt
                         |00:00:04.292,0200,2,600
                         |00:00:03.292,0200,1,1700
                         |""".stripMargin
  }

  it should "filter using greater than operator with -f rtt>1000 option" in {
    val output = run("--pair", "-f", "rtt>1000", "--csv", "time,mti,11,rtt", "src/test/resources/a-bunch.xml")
    output shouldEqual """time,mti,11,rtt
                         |00:00:03.292,0200,1,1700
                         |""".stripMargin
  }

  it should "filter using less than operator with -f rtt<1000 option" in {
    val output = run("--pair", "-f", "rtt<1000", "--csv", "time,mti,11,rtt", "src/test/resources/a-bunch.xml")
    output shouldEqual """time,mti,11,rtt
                         |00:00:04.292,0200,2,600
                         |""".stripMargin
  }

  it should "filter using case insensitive contains operator with -f 48.1~suBfield option" in {
    val output = run("-f", "48.1~suBfield", "--csv", "time,mti,11,file", "src/test/resources/a-pair.xml")
    output shouldEqual """time,mti,11,file
                         |00:00:04.100,0810,28928,a-pair.xml:14
                         |""".stripMargin
  }

  it should "negate filter with !in -f 24!=831 option" in {
    val output = run("-f", "24!=831", "src/test/resources/a-pair.xml")
    output shouldEqual """<log realm="rotate-log-listener" at="Mon Nov 24 13:10:55 EST 2014">
                         |   maxSize (50000000) threshold reached
                         |</log>
                         |""".stripMargin
  }

  it should "match anywhere in the message with -grep option" in {
    val output = run("--grep", "threshold", "src/test/resources/a-pair.xml")
    output shouldEqual """<log realm="rotate-log-listener" at="Mon Nov 24 13:10:55 EST 2014">
                         |   maxSize (50000000) threshold reached
                         |</log>
                         |""".stripMargin
  }

  it should "negate --grep with --grep! option" in {
    val output = run("--grep!", "threshold", "src/test/resources/a-pair.xml")
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

  it should "print full text by default" in {
    val output = run("src/test/resources/a-pair.xml")
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
    val output = run("-B", "1", "--csv", "48.1", "--no-header", "--grep", "#3", "src/test/resources/a-bunch.xml")
    output shouldEqual
      """a-bunch.xml #2
        |a-bunch.xml #3
        |""".stripMargin
  }

  it should "include N messages following a match with -A N option" in {
    val output = run("-A", "1", "--csv", "48.1", "--no-header", "--grep", "#2", "src/test/resources/a-bunch.xml")
    output shouldEqual
      """a-bunch.xml #2
        |a-bunch.xml #3
        |""".stripMargin
  }

  it should "include N messages before and after a match with -C N option" in {
    val output = run("-C", "1", "--csv", "48.1", "--no-header", "--grep", "#3", "src/test/resources/a-bunch.xml")
    output shouldEqual
      """a-bunch.xml #2
        |a-bunch.xml #3
        |a-bunch.xml #4
        |""".stripMargin
  }

  it should "includes preceding context only once with -B N option" in {
    val output = run("-B", "1", "--csv", "48.1", "--no-header", "--field", "at~00:00:04", "src/test/resources/a-bunch.xml")
    output shouldEqual
      """a-bunch.xml #1
        |a-bunch.xml #2
        |a-bunch.xml #3
        |a-bunch.xml #4
        |""".stripMargin
  }

  it should "include records matching all -f filters" in {
    val output = run("-f", "48.1~bunch", "-f", "at~.892", "--csv", "48.1", "--no-header", "src/test/resources/a-bunch.xml")
    output shouldEqual
      """a-bunch.xml #3
        |""".stripMargin
  }

  it should "print wrap pairs in <pair></pair> with --pair" in {
    val output = run("--pair","src/test/resources/a-pair.xml")
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

  def run(args: String*): String = {
    val out = new ByteArrayOutputStream()
    Console.withOut(out) {
      Main.main(args.toArray)
    }
    out.toString
  }
}
