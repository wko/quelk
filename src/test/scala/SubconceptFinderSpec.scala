package de.tu_dresden.epistemic_rewriter.test

import java.nio.file.{Files, Paths}

import de.tu_dresden.epistemic_rewriter.SubconceptFinder
import org.scalatest.FlatSpec


class SubconceptFinderSpec extends FlatSpec {
  val doc =
    <doc>
      <config>
        <ontology path="data/ontologies/snomed2017-english-norm.ofn"></ontology>
        <output path="data/subconcepts/advanced-cad-subconcepts.xml"></output>
      </config>
      <concepts>
        <concept iri="http://snomed.info/id/49601007" label="Cardiovascular Disease"></concept>
      </concepts>
    </doc>


  "operations" should "execute" in {
    val c = SubconceptFinder.readParamsFromXML(doc)

    SubconceptFinder.compute(c)

    assert(Files.exists(Paths.get(c.outpath)))
    val r = xml.XML.loadFile(c.outpath)
    assert((r \\ "subterm").nonEmpty)
  }



}
