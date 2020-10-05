package de.tu_dresden.epistemic_rewriter.test

import java.nio.file.{Files, Paths}

import de.tu_dresden.epistemic_rewriter.cli.ConsoleTool
import org.scalatest.FlatSpec



class ConsoleToolSpec extends FlatSpec {
  val doc =
    <doc>
      <configs>
        <config>
          <database dbpath="jdbc:postgresql://localhost:5434" dbname="cancer" user="postgres" password=""> </database>
          <ontology path="data/ontologies/cancer.ofn"></ontology>
          <output path="data/queries/results/cancer-test-results.xml"></output>

        </config>
      </configs>
      <operation name="answer"></operation>
      <queries>
        <query label="Query1">
          <part type="AND">
            <part type="NCQ" content="EX(y). (diagnosedWith(x,y) AND Cancer(y) AND NOT SkinCancer(y))"></part>
            <part type="GEN" n="2">
              <part type="NCQ" content="EX(y). (diagnosedWith(x,y) AND Cancer(y) AND NOT SkinCancer(y))"></part>
              <part type="NCQ" content="EX(y). (diagnosedWith(x,y) AND NOT SkinCancer(y))"></part>
              <part type="NCQ" content="EX(y). (diagnosedWith(x,y) AND Cancer(y) AND NOT LungCancer(y) AND LungCancer(y))"></part>
            </part>
          </part>
        </query>
        <query label="Query2">
          <part type="NCQ" content="EX(). (diagnosedWith(x,y) AND Cancer(y) AND NOT LungCancer(y))"></part>
        </query>
        <query label="True Positives">
          <part type="AND">
            <part type="REF" label="Query1"></part>
            <part type="REF" label="Query2"></part>
          </part>
        </query>
        <query label="False Positives">
          <part type="DIFF">
            <part type="REF" label="Query1"></part>
            <part type="REF" label="Query2"></part>
          </part>
        </query>
        <query label="False Negatives">
          <part type="DIFF">
            <part type="REF" label="Query2"></part>
            <part type="REF" label="Query1"></part>
          </part>
        </query>
      </queries>
    </doc>

  val temporalDoc =
    <doc>
      <configs>
        <config>
          <database dbpath="jdbc:postgresql://localhost:5434" dbname="temporalcancerfast" user="postgres" password=""> </database>
          <ontology path="data/ontologies/temporal-cancer.ofn"></ontology>
          <output path="data/queries/results/temporal-cancer-test-results.xml"></output>
          <options withInference="true" temporal="true"></options>
        </config>
      </configs>
      <operation name="answer"></operation>
      <queries>
        <query label="Query1">
          <part type="AND">
            <part type="TNCQ" timepoint="1994-10-02T18:48:05" content="E[P1D, P8D](PancreasCancerPatient(x))"></part>
          </part>
        </query>
        <query label="Query2">
          <part type="TNCQ" timepoint="1994-10-02T18:48:05" content="E[P1D, P8D](CancerPatient(x))"></part>
        </query>
        <query label="True Positives">
          <part type="AND">
            <part type="REF" label="Query1"></part>
            <part type="REF" label="Query2"></part>
          </part>
        </query>
        <query label="False Positives">
          <part type="DIFF">
            <part type="REF" label="Query1"></part>
            <part type="REF" label="Query2"></part>
          </part>
        </query>
        <query label="False Negatives">
          <part type="DIFF">
            <part type="REF" label="Query2"></part>
            <part type="REF" label="Query1"></part>
          </part>
        </query>
      </queries>
    </doc>

/*
  "reading OpTypes from string" should "work" in {
    assert(OpTypes.withNameWithDefault("answer") == OpTypes.Answer)
    assert(OpTypes.withNameWithDefault("Rewrite") == OpTypes.Rewrite)
  }

  "reading config from XML" should "return the correct config" in {

    val c = ConsoleTool.readConfigFromXML(doc).head
    assert(c.cmd == OpTypes.Answer)
    assert(c.queries.size == 5)

    val c1 = ConsoleTool.readConfigFromXML(temporalDoc).head
    assert(c1.cmd == OpTypes.Answer)
    assert(c1.queries.size == 5)
  }


  "operations" should "execute" in {
    val c = ConsoleTool.readConfigFromXML(doc).head
    ConsoleTool.rewriteAndAnswer(c)
    assert(Files.exists(Paths.get(c.output)))
  }
*/
  it should "evaluate temporal queries" in {
    //valConsoleTool.readConfigFromXML(temporalDoc).head

    val files = List(
    //  "data/queries/mi-6mos.xml",
      "data/queries/mi-6mos-temporal.xml"
    )

    for ( f <- files) {
      val temporalDoc = xml.XML.loadFile(f)
      val c = ConsoleTool.readConfigFromXML(temporalDoc).head
      ConsoleTool.rewriteAndAnswer(c)
      assert(Files.exists(Paths.get(c.output)))
    }

    val l = List("dd","d","ab","ac","a")
    val ls = l.sortWith{ (a,b) => b.contains(a)}
    println(l)
    println(ls)
  }



}
