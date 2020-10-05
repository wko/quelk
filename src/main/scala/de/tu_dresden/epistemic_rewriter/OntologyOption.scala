package de.tu_dresden.epistemic_rewriter

import java.io.File

import de.tu_dresden.epistemic_rewriter.ConfigValues.string2sql

case class OntologyOption(file: File, withInference: Boolean) {

  def this(path: String, withInference: Boolean) = this(new File(path), withInference)

  lazy val dbname = withInference match {
    case true => string2sql(name)
    case false => string2sql(name+"noinf")
  }
  lazy val name = file.toPath.getFileName.toString.substring(0,file.toPath.getFileName.toString.lastIndexOf("."))
  lazy val connection: DBParams = ConfigValues.getConnectionParams(dbname)
  def getHelper(normalized: Boolean=false): OntologyHelper = OntologyHelper.createOntologyHelper(file, None, normalized)
  def getDBManager: DatabaseManager = DatabaseManager.getManager(connection)
}
