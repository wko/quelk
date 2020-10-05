package de.tu_dresden.epistemic_rewriter.cli

import java.io.File

import com.typesafe.scalalogging.StrictLogging
import org.apache.commons.io.FilenameUtils
import org.phenoscape.scowl._
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat
import org.semanticweb.owlapi.model.{IRI, OWLClassAssertionAxiom}
import org.semanticweb.owlapi.util.AutoIRIMapper

import scala.collection.JavaConverters._

object N2C2Tool extends StrictLogging {

  def main(args: Array[String]): Unit = {
    parser.parse(args, Params()) match {
      case Some(params) => savePatientDataToOntology(params)
      case None =>
    }

  }

  /**
    * Add the ground truth from n2c2 xml files to the ontology.
    * @param params
    */
  def savePatientDataToOntology(params: Params) = {
    val axiomsT = for (file <- params.dir.listFiles.filter(f => f.isFile && FilenameUtils.getExtension(f.getName) == "xml").toList) yield {
      logger.info("Processing " + file.getName)
      val patient = FilenameUtils.getBaseName(file.getName)
      val doc = xml.XML.loadFile(file)
      readPatientData(patient, doc)
    }
    val axioms = axiomsT.flatten


    val manager = OWLManager.createOWLOntologyManager
    val o = params.input match {
      case Some(f) => {
        val irimapper = new AutoIRIMapper(f.getParentFile, false)
        manager.addIRIMapper(new AutoIRIMapper(f.getParentFile, false))
        manager.addIRIMapper(new AutoIRIMapper(params.output.getParentFile, false))
        manager.loadOntologyFromOntologyDocument(params.input.head)
      }
      case None => manager.createOntology()
    }
    manager.addAxioms(o, axioms.toSet.asJava)

    manager.saveOntology(o, new FunctionalSyntaxDocumentFormat(), IRI.create(params.output.toURI()))
  }

  def readPatientData(patient: String, doc: xml.Node): Seq[OWLClassAssertionAxiom] = {
    val p = NamedIndividual("http://goasq.lri.fr/ontology/n2c2#patient"+patient)
    for (x <- (doc \ "TAGS" \ "_").filter( _ \@ "met" == "met")) yield {
      val concept = Class("http://goasq.lri.fr/ontology/n2c2#" + x.label)
      ClassAssertion(concept, p)
    }
  }

  case class Params(dir: File = new File("."), output: File = new File("."), input: Option[File]= None)

  val parser = new scopt.OptionParser[Params]("rewriter") {
    opt[File]('d',"d").valueName("<file>").required().
      action( (x, c) => c.copy(dir = x) ).
      text("the directory with the xml patient data")

    opt[File]('o',"output").valueName("<file>").required().
      action( (x, c) => c.copy(output = x) ).
      text("the output file for the resulting ontology")

    opt[File]('i',"input").valueName("<file>").required().
      action( (x, c) => c.copy(input = Some(x)) ).
      text("facts are inserted into the ontology")
  }
}
