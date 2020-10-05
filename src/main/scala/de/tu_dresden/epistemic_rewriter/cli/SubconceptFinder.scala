package de.tu_dresden.epistemic_rewriter

import java.io.{File, PrintWriter}

import com.typesafe.scalalogging.StrictLogging
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model._

import scala.collection.JavaConverters._
import scala.xml.{Node, NodeSeq}

object SubconceptFinder extends StrictLogging {

  def findSubExpressionsR(concept: OWLClass)(implicit helper: OntologyHelper):Set[OWLLogicalEntity with SWRLPredicate] = {
    val subClasses = helper.querySubClasses(concept, false).asScala.toSet
    subClasses.map{ cls => helper.ontology.getSubClassAxiomsForSuperClass(cls).asScala.toSet.map{ ax:OWLSubClassOfAxiom => {
      val c = ax.getSubClass
      val newCls = c.getClassesInSignature.asScala
      val newRoles = c.getObjectPropertiesInSignature.asScala
      newRoles.++(newCls).++(subClasses)
    }}.flatten}.flatten

  }
  def compute(params: Params): Unit = {
    implicit val helper = OntologyHelper.createOntologyHelper(new File(params.ontology), None, false)
    val subs = for (concept <- params.concepts) yield {
      logger.info("Processing " + concept)
      val subDefs = findSubExpressionsR(concept)
      val xs:Seq[Node] = for ( x <- subDefs.toSeq) yield {
        val l = x match {
          case c:OWLClass => helper.getLabel(c).getOrElse("")
          case r:OWLObjectProperty => helper.getLabel(r).getOrElse("")
          case _ => ""
        }
        <subterm iri={x.getIRI.toString} label={l}></subterm>
      }
      <class iri={concept.getIRI.toString} label={helper.getLabel(concept).getOrElse("")}>
        {NodeSeq fromSeq xs}
      </class>
    }

    val pp = new scala.xml.PrettyPrinter(180, 2)

    val resultsQ = <results>
      {NodeSeq fromSeq subs}
    </results>

    if (params.outpath.nonEmpty) {
      logger.info("Writing results to " + params.outpath)
      val bw = new PrintWriter(params.outpath)
      bw.write(pp.format(resultsQ))
      bw.close()
    }
  }
  def main(args: Array[String]): Unit = {
    parser.parse(args, XMLConfig()).map{ x:XMLConfig => readParamsFromXML(x.xml.head)} match {
      case Some(params) => compute(params)
      case None =>
    }
  }

  case class Params(ontology: String, outpath: String, concepts: Seq[OWLClass])



  case class XMLConfig(xml: Option[Node] = None)

  def readParamsFromXML(doc: xml.Node): Params = {

    val opath = (doc \ "config"\ "ontology" \@ "path")
    val outpath = (doc \ "config"\ "output" \@ "path")
    val concepts = for (q <- doc \ "concepts" \\ "concept") yield Class(q \@ "iri")
    Params(opath, outpath, concepts)

  }
  val parser = new scopt.OptionParser[XMLConfig]("rewriter") {
    opt[File]("xml").valueName("<file>").
      action( (x, c) => c.copy(xml = Some(xml.XML.loadFile(x)) )).
      text("please provide a valid xml config file")
  }
}
