package de.tu_dresden.epistemic_rewriter.cli

import java.io.{File, PrintWriter}

import com.typesafe.scalalogging.StrictLogging
import de.tu_dresden.epistemic_rewriter._
import de.tu_dresden.epistemic_rewriter.cli.ConsoleTool.OpTypes.OpType
import de.tu_dresden.epistemic_rewriter.datatypes.ResultListType.ResultList
import de.tu_dresden.epistemic_rewriter.datatypes.{Answer, ComplexQuery, ComplexQueryExpression}
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.util.AutoIRIMapper

import scala.collection.immutable.{ListMap, Set}
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.xml.{Elem, Node, NodeSeq}


object ConsoleTool extends StrictLogging {
  val IMPORTS_INCLUDED = true
  def normalize(config: Config): Unit = {
    val ontfile = new File(config.ontology)
    val irimapper = new AutoIRIMapper(new File(ontfile.getParent), false)
    val helper = OntologyHelper.createOntologyHelper(ontfile, Some(irimapper), false)
    val outfile = new File(config.output)
    logger.info("Loading and Normalizing ontology " + ontfile)
    helper.normalizeOntology(helper.ontology, Imports.INCLUDED)
    helper.saveOntology(helper.manager, helper.ontology, outfile)
    logger.info("Ontology saved to " + outfile)

  }



  // TODO: Add Option to just update ABox
  def rewriteAndAnswer(config: Config): Unit = {

    if (!DatabaseManager.areValidParams(config.dbparams)) {
      logger.info("Could not connect to database. Please check the database params")
      logger.info(config.dbparams.toString)
      return
    }
    val manager = DatabaseManager.getManager(config.dbparams)


    val ontfile = new File(config.ontology)
    val irimapper = new AutoIRIMapper(new File(ontfile.getParent), false)
    logger.info("Loading ontology " + ontfile)
    val helper = OntologyHelper.createOntologyHelper(ontfile, Some(irimapper), false)


    val model: Modellike = config.temporal match {
      case true => {
        val model = new TemporalDatabaseModel(manager, config.withInference, helper)
        if (!model.isInitialized(helper.ontology)) {
          logger.info("Model is not initialized. Saving to Database once..")
          model.saveToDatabase()
          //model = DatabaseModel.saveToDatabase(helper, manager, config.withInference)
          logger.info("done.")
        }
        model
      }
      case false => {
        var model = new DatabaseModel(manager, config.withInference)
        if (!model.isInitialized(helper.ontology)) {
          logger.info("Model is not initialized. Saving to Database once..")
          model.saveToDatabase(helper)
          //model = DatabaseModel.saveToDatabase(helper, manager, config.withInference)
          logger.info("done.")
        }
        model
      }
    }

    val queries : Seq[ComplexQuery] = config.queries.map(ComplexQuery.fromXML(_)(manager))
    val r = queries.foldLeft(ListMap.empty:ResultList){ case (results, query) =>
      logger.info("Processing query:\n" + query.pShow())

      config.cmd match {
        case OpTypes.Rewrite => {
          throw new NotImplementedError("Rewriting is only implemented for NCQs. The function needs to be extended to complex queries.")
          //val pack = AnswersPack(ncq, rewritings.map((_, Set.empty: Set[Answer])).toMap)
          //pack.toXML()
        }
        case OpTypes.Answer => {
          val answers = Await.result(query.computeAnswersMapping(config.withRewriting)(helper, model, results), Duration.Inf)
          results.+((query.label, (query, answers._2)))
        }
      }
    }

    def answers2Node(a: Set[Answer]): Node = {
      <answers count={a.size.toString}>{NodeSeq fromSeq a.map(_.toXML()).toSeq.distinct}</answers>
    }

    val queriesWithAnswers: Iterable[Node] = queries.map{ ncq =>
      logger.info("Converting" + ncq.pShow())
      r.get(ncq.label) match {
        case Some((_, a)) => {
          def f(q: ComplexQueryExpression):NodeSeq = {
            val answers = a.aMap.get(q) match {
              case Some(answersSet) => answersSet
              case None => throw new Error("This should not happen.." + q.pShow())
            }
            answers2Node(answers)
          }

          ncq.toXML(answers2Node(a.answers), f)
        }
        case None => throw new Error()
      }
    }

    val resultsQ =
      <doc>
        { config.toXML }
        <operation name={config.cmd.toString}></operation>
        <queries>{NodeSeq fromSeq queriesWithAnswers.toSeq}</queries>
      </doc>

    val pp = new scala.xml.PrettyPrinter(180, 2)


    if (config.output.nonEmpty) {
      logger.info("Writing results to " + config.output)
      val bw = new PrintWriter(config.output)
      bw.write(pp.format(resultsQ))
      bw.close()
    }
    else {
      logger.info(pp.format(resultsQ))
    }
  }

  def main(args: Array[String]): Unit = {
    parser.parse(args, XMLConfig(Seq.empty)) match {
      case Some(config) => {
        logger.info("Reading config files..")
        for (xml <- config.xmls) {
          val configs = readConfigFromXML(xml.head)
          //readConfigFromXML(scala.xml.XML.load(System.in))
          logger.info("Parsing input done")
          for (config <- configs) {
            config.cmd match {
              case OpTypes.Normalize => normalize(config)
              case _ => rewriteAndAnswer(config)
            }
          }
        }
      }
      case None =>
    }
  }



  object OpTypes extends Enumeration {
    type OpType = Value
    val Rewrite, Answer, Normalize, NOOP = Value

    def withNameWithDefault(name: String): Value =
      values.find(_.toString.toLowerCase == name.toLowerCase()).getOrElse(NOOP)
  }
  implicit val opTypesRead: scopt.Read[OpTypes.Value] =
    scopt.Read.reads(OpTypes withName _)

  case class Config(ontology: String, output: String, cmd: OpType, queries: Seq[Node], dbparams : DBParams, withRewriting: Boolean = true, temporal: Boolean = false, withInference: Boolean = true) {
    def toXML():Elem= {
      <config>
        { dbparams.toXML() }
        <ontology path={ontology}></ontology>
        <output path={output}></output>
      </config>
    }
  }
  case class XMLConfig(xmls: Seq[Node])

  def readConfigFromXML(doc: xml.Node): Seq[Config] = {
    val queries : Seq[Node] = doc \ "queries" \ "query"
    val cmd = OpTypes.withNameWithDefault((doc \ "operation" \@ "name"))


    for (c <- doc \ "configs" \ "config") yield {
      val withInference = (c \ "options" \@ "withInference") == "true"

      val password = (c \ "database" \@ "password")
      val user = (c \ "database" \@ "user")
      val dbpath = (c \ "database" \@ "dbpath")
      val dbname = (c \ "database" \@ "dbname") + (if (withInference) "" else "noinf")

      val opath = (c \ "ontology" \@ "path")
      val outpath = (c \ "output" \@ "path")

      val withRewriting = (c \ "options" \@ "withRewriting") == "true"
      val temporal = (c \ "options" \@ "temporal") == "true"

      val dbparams = DBParams(dbpath, dbname, user, password)
      Config(opath, outpath, cmd, queries, dbparams, withRewriting, temporal, withInference)
    }
  }

  val parser = new scopt.OptionParser[XMLConfig]("rewriter") {
    arg[File]("xml_configs...").unbounded().optional().
      action { (x, c) => c.copy(xmls = c.xmls:+ xml.XML.loadFile(x)) }.
      text("please provide valid   xml config files")
  }


}
