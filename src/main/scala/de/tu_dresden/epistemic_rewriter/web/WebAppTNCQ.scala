package de.tu_dresden.epistemic_rewriter.web

import java.io.File
import java.time.format.DateTimeParseException

import com.typesafe.scalalogging.StrictLogging
import de.tu_dresden.epistemic_rewriter._
import de.tu_dresden.epistemic_rewriter.datatypes.TimePoint
import org.scalatra._
import org.scalatra.forms._
import org.scalatra.i18n.I18nSupport
import org.scalatra.scalate.ScalateSupport
import org.semanticweb.owlapi.util.AutoIRIMapper

import scala.collection.mutable


class WebAppTNCQ extends ScalatraServlet with FormSupport with I18nSupport with ScalateSupport with StrictLogging {

  val NORMALIZED : Boolean = false
  
   //"http://www.semanticweb.org/forkel/ontologies/2018/6/restaurant#"
  var lastInput:TemporalFormInput = TemporalFormInput("E[P1D, P8D](EX(y). (diagnosedWith(x,y) AND Cancer(y)))", "", "")
  val ontologyOptions = {
    val l = ConfigValues.getOntologies()
    logger.info("Found " + l.size + " ontologies in " + ConfigValues.ROOT_DIRECTORY)
    l
  }
  var lastOptionIdx:Int = -1
  val irimapper = new AutoIRIMapper(new File(ConfigValues.ROOT_DIRECTORY), false)

  var helper: Option[OntologyHelper] = None //option.getHelper(NORMALIZED)
  var model: Option[TemporalDatabaseModel] = None //new TemporalDatabaseModel(option.getDBManager,true, helper)

  val title:String = "Temporal Conjunctive Query Answering with Negation"
  val alerts: mutable.Queue[String] = mutable.Queue[String]()

  val queryform = mapping(
    "text"        -> label("ECQ", text(required)),
    "groundTruth"        -> label("GroundTruth", text()),
    "timepoint"        -> label("TimePoint", text(required)),
  )(TemporalFormInput.apply)



  def loadModel(ontologyIdx: Int) = {
    if (ontologyIdx != lastOptionIdx) {
      lastOptionIdx = ontologyIdx
      val option = ontologyOptions(lastOptionIdx)
      logger.debug(option.file.getPath)

      helper = Some(option.getHelper(NORMALIZED))
      val h = helper.head
      model = Some(new TemporalDatabaseModel(option.getDBManager, true, h))
      val m = model.head

      if (!m.isInitialized(h.ontology)) {
        logger.info("Model is not initialized..")
        logger.info(s"Saving Model to Database..${option.dbname}")
        m.saveToDatabase()
        logger.info("Model successfully saved to Database..")
      }
    }
  }

  before() {
    contentType="text/html"


    templateAttributes(request)("title") = title
    templateAttributes(request)("alerts") = alerts
    templateAttributes(request)("request") = request
  }
  after() {

  }

  // Ontology Chooser

  get("/") {
    contentType = "text/html"
    ssp("index", "ontologyOptions" -> ontologyOptions)
  }


  post("/") {
    contentType = "text/html"

    val ontologyChoiceForm = mapping(
      "ontology"    -> label("ontology", number(required))
    )(OntologyFormInput.apply)



    validate(ontologyChoiceForm)(
      errors => {
        BadRequest(ssp("/index", "ontologyOptions" -> ontologyOptions ,"errors" -> convertErrors(errors)))
      },
      success = form => {
        redirect(s"/ontologies/${form.ontologyOption}")
      }
    )
  }


  get("/ontologies/:id") {
    contentType = "text/html"
    try {
      val id = params("id").toInt
      loadModel(id)
      ssp("/temporal-querying", "formInput" -> lastInput, "model" -> model.head)
    }
    catch {
      case e:java.lang.NumberFormatException => {
        alerts.enqueue("Please provide a valid integer choice for an ontology.")
        redirect("/")
      }
      case e:java.lang.IndexOutOfBoundsException => {
        alerts.enqueue("Index out of range. Please provide a valid integer choice for an ontology.")
        redirect("/")
      }
      case e:Throwable => {
        alerts.enqueue(e.getMessage)
        redirect("/")
      }
    }



  }

  // Process Query for the selected ontology
  post("/ontologies/:id") {
    validate(queryform)(
      errors => {
        BadRequest(ssp("/temporal-querying", "errors" -> convertErrors(errors), "formInput" -> lastInput ))
      },
      success = form => {
        try {
          lastInput = form
          loadModel(params("id").toInt)
          logger.info(lastInput.toString)
          val t0 = System.nanoTime()
          model match {
            case None =>
            case Some(m) => {
              val query = TemporalFOQueryParser.parse(lastInput.text, m.manager)
              val timepoint = TimePoint.parse(lastInput.timepoint)


              m.setTimePoint(timepoint)
              val answers = m.getAnswers(query)
              var result = TemporalQueryResult(query, timepoint, Map(), answers, None)

              if (form.groundTruth.nonEmpty) {
                val gtq = TemporalFOQueryParser.parse(lastInput.groundTruth, m.manager)
                val gta = m.getAnswers(gtq)
                result = result.copy(groundTruthQuery = Some(gtq), groundTruthAnswers = Some(gta))
              }

              val t1 = System.nanoTime()
              val ms = (t1 - t0) / 1000000
              result = result.copy(millis = Some(ms))
              ssp("/temporal/result", "queryResult" -> result, "model" -> model.head, "formInput" -> lastInput)
            }
          }
        }




        catch {
          case e:java.lang.NumberFormatException => {
            alerts.enqueue("Please provide a valid integer choice for an ontology.")
            redirect("/")
          }
          case e:java.lang.IndexOutOfBoundsException => {
            alerts.enqueue("Index out of range. Please provide a valid integer choice for an ontology.")
            redirect("/")
          }
          case e:NoSuchFieldException => {
            //alerts.enqueue("Index out of range. Please provide a valid integer choice for an ontology.")
            alerts.enqueue(e.getMessage)
            BadRequest(ssp("/temporal-querying", "errors" -> Map(("text", e.getMessage)), "formInput" -> lastInput, "model" -> model.head))
          }
          case e:DateTimeParseException => {
            alerts.enqueue("The given datetime could not be parsed..")
            alerts.enqueue(e.getMessage)
            BadRequest(ssp("/temporal-querying", "errors" -> Map(("text", e.getMessage)), "formInput" -> lastInput, "model" -> model.head))
          }
          case e: org.postgresql.util.PSQLException => {
            logger.debug(params.toString)
            logger.debug(e.getServerErrorMessage.toString)
            alerts.enqueue(e.getMessage)
            BadRequest(ssp("/temporal-querying", "errors" -> Map(("text", e.getMessage)), "formInput" -> lastInput, "model" -> model.head))
          }
          case e: Throwable => {
            e.printStackTrace()
            alerts.enqueue(e.getMessage)
            BadRequest(ssp("/temporal-querying", "errors" -> Map(("text", e.getMessage)), "formInput" -> lastInput, "model" -> model.head))
          }
        }
      }
    )
  }

  def convertErrors(errors: Seq[(String, String)]):Map[String, String] = Map(errors :_*)
}
