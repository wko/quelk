package de.tu_dresden.epistemic_rewriter.web

import java.io.File

import com.typesafe.scalalogging.StrictLogging
import de.tu_dresden.epistemic_rewriter._
import org.scalatra._
import org.scalatra.forms._
import org.scalatra.i18n.I18nSupport
import org.scalatra.scalate.ScalateSupport
import org.semanticweb.owlapi.util.AutoIRIMapper


class WebAppECQ extends ScalatraServlet with FormSupport with I18nSupport with ScalateSupport with StrictLogging {

  val NORMALIZED : Boolean = false
  
   //"http://www.semanticweb.org/forkel/ontologies/2018/6/restaurant#"
  var lastCQ:FormInput = FormInput("EX(y). Patient(x) AND hasFinding(x,y) AND 49601007(y)", "", 0, true, false)
  val ontologyOptions = {
    val l = ConfigValues.getOntologies()
    logger.info("Found " + l.size + " ontologies in " + ConfigValues.ROOT_DIRECTORY) 
    l
  }
  var lastOptionIdx:Int = 0
  val irimapper = new AutoIRIMapper(new File(ConfigValues.ROOT_DIRECTORY), false)
  var option = ontologyOptions(lastOptionIdx)
  var helper = option.getHelper(NORMALIZED)
  var model: DatabaseModel = new DatabaseModel(option.getDBManager, withInference = true)

  val title:String = "Epistemic Conjunctive Query Answering"

  val ecqform = mapping(
    "text"        -> label("ECQ", text(required)),
    "groundTruth"        -> label("GroundTruth", text()),
    "ontology"    -> label("ontology", number(required)),
    "withInference"    -> label("withInference", boolean()),
  "temporal"    -> label("temporal", boolean())
  )(FormInput.apply)


  get("/") {
    contentType = "text/html"
    ssp("/form", "title" -> title, "formInput" -> lastCQ, "request" -> request, "ontologyOptions" -> ontologyOptions)
  }

  post("/") {
    contentType = "text/html"
    val attributes: List[(String, Any)] = List("title" -> title, "request" -> request, "ontologyOptions" -> ontologyOptions)
    validate(ecqform)(
      errors => {
        BadRequest(ssp("/form", attributes.:+("errors" -> convertErrors(errors)).:+("formInput" -> lastCQ ):_*))
      },
      success = form => {
        try {
          val lastWithInference = lastCQ.withInference
          lastCQ = form
          logger.info(lastCQ.toString)
          if (lastOptionIdx != lastCQ.ontologyOption || lastWithInference != form.withInference) {
            lastOptionIdx = form.ontologyOption
            option = ontologyOptions(lastOptionIdx).copy(withInference = form.withInference)
            logger.info(s"A different Ontology was chosen. Loading ontology ${option.dbname}")
            helper.reasoner.dispose()

            helper = option.getHelper(NORMALIZED)
            logger.debug(option.file.getPath)


            model = new DatabaseModel(option.getDBManager, form.withInference)
            if (!model.isInitialized(helper.ontology)) {
              logger.info("Model is not initialized..")


              logger.info(s"Saving Model to Database..${option.dbname}")
              model.saveToDatabase(helper)
              logger.info("Model successfully saved to Database..")

            }
          }
          val t0 = System.nanoTime()
          form.temporal match {
            // ATEMPORAL CASE
            case false => {
              val ecq = FOQueryParser.parse(lastCQ.text, option.getDBManager)
              logger.info("Computing the rewriting")
              val ucq = EpistemicConjunctiveQueryRewriter.transform(ecq, helper)
              logger.info("Computing the answers for " + ucq.size + " SQL queries")
              val answers = model.getAnswersMap(ucq)
              val answersWithJustifications = for ((q, as) <- answers) yield {
                (q, as.map(a => helper.computeJustification(q.ecq, a.named)))
              }

              var result = QueryResult(ecq, answersWithJustifications, answers.values.flatten.toSet, None)

              if (form.groundTruth.nonEmpty) {
                val gtq = FOQueryParser.parse(lastCQ.groundTruth, option.getDBManager)
                val ucqgt = EpistemicConjunctiveQueryRewriter.transform(gtq, helper)
                val gta = model.getAnswers(ucqgt)
                result = result.copy(groundTruthQuery = Some(gtq), groundTruthAnswers = Some(gta))
              }


              val t1 = System.nanoTime()
              val ms = (t1 - t0) / 1000000
              result = result.copy(millis = Some(ms))
              ssp("/result", attributes.:+("queryResult" -> result).:+("model" -> model).:+("formInput" -> lastCQ): _*)
            }
              // TEMPORAL CASE
            case true => {
              val ecq = TemporalFOQueryParser.parse(lastCQ.text, option.getDBManager)
              // TODO: TEMPORAL REWRITING TO BE INCLUDED HERE!!!
              val answers = model.getAnswers(ecq)
              var result = QueryResult(ecq, Map(), answers, None)

              if (form.groundTruth.nonEmpty) {
                val gtq = TemporalFOQueryParser.parse(lastCQ.groundTruth, option.getDBManager)
                val gta = model.getAnswers(gtq)
                result = result.copy(groundTruthQuery = Some(gtq), groundTruthAnswers = Some(gta))
              }

              val t1 = System.nanoTime()
              val ms = (t1 - t0) / 1000000
              result = result.copy(millis = Some(ms))
              ssp("/result", attributes.:+("queryResult" -> result).:+("model" -> model).:+("formInput" -> lastCQ): _*)
            }
          }


        }
        catch {
          case e: org.postgresql.util.PSQLException => {
            logger.debug(params.toString)
            logger.debug(e.getServerErrorMessage.toString)
            BadRequest(ssp("/form", attributes.:+("errors" -> Map(("text", e.getMessage))).:+("formInput" -> lastCQ): _*))
          }
          case e: Exception => {
            e.printStackTrace()
            BadRequest(ssp("/form", attributes.:+("errors" -> Map(("text", e.getMessage))).:+("formInput" -> lastCQ): _*))
          }
        }
      }
    )
  }

  def convertErrors(errors: Seq[(String, String)]):Map[String, String] = Map(errors :_*)
}
