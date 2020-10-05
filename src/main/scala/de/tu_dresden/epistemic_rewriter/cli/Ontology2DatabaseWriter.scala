package de.tu_dresden.epistemic_rewriter.cli

import java.io.File

import com.typesafe.scalalogging.StrictLogging
import de.tu_dresden.epistemic_rewriter._






object Ontology2DatabaseWriter extends StrictLogging {

  val parser = new scopt.OptionParser[OntologyOption]("ontology2DBWriter") {
    opt[File]("file").valueName("<file>").required().
      action( (x, c) => c.copy(file = x)).
      text("please provide a valid ontology file")
    opt[Boolean]("withInference").
      action( (x, c) => c.copy(withInference = x)).
      text("should inference be done to complete the ontology?")
  }


  def main(args: Array[String]): Unit = {
    parser.parse(args, OntologyOption(null, true)) match {
      case Some(option) => save2DB(option)
      case None =>
    }
  }

  def save2DB(option: OntologyOption) = {
    logger.info("Writing " + option.name + " to DB " + option.dbname + "...")
    logger.info("Loading ontology...")
    val helper = option.getHelper()
    logger.info("Saving model to DB...")
    val manager = DatabaseManager.getManager(ConfigValues.getConnectionParams(option.dbname))
    val model = new DatabaseModel(manager, true)
    model.saveToDatabase(helper)
    helper.reasoner.dispose()
  }







}
