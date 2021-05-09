package de.tu_dresden.epistemic_rewriter

import java.io.File







object ConfigValues {
  val ROOT_DIRECTORY:String = {
    sys.env.get("REWRITER_ONTOLOGY_HOME") match {
      case Some(path) => path + "/"
      case None => "data/ontologies/"
    }
  }
  val user = sys.env.get("DB_USER") match {
    case Some(u) => u
    case None => "postgres"
  }
  val password = sys.env.get("DB_PASSWORD") match {
    case Some(pw) => pw
    case None => "postgres"
  }

  val host = sys.env.get("DB_HOST") match {
    case Some(pw) => pw
    case None => "localhost"
  }

  val port = sys.env.get("DB_PORT") match {
    case Some(pw) => pw
    case None => "5432"
  }
   
  def getConnectionParams(dbname: String): DBParams = DBParams(s"jdbc:postgresql://${host}:${port}", dbname, user, password)

  //def iri2sql(iRI: IRI): String = "s" + iRI.getShortForm.filterNot("-_,;".contains(_))
  def string2sql(s: String): String = "s" + s.filterNot("-_,;.?".contains(_)).toLowerCase

  def getOntologies(dir: String = ROOT_DIRECTORY): List[OntologyOption] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      val allowedExt = List("ofn", "owl", "xml")
      val l = for (f <- d.listFiles.filter(x => x.isFile && allowedExt.contains(extension(x.getCanonicalPath))).toList) yield {
        OntologyOption(f, withInference = true)
      }
      l.sortBy(_.name)
    } else {
      List()
    }
  }

  def extension(name: String): String = {
    var i = name.length - 1
    while (i >= 0 && name.charAt(i) != '.')
      i -= 1

    if (i < 0) ""
    else name.substring(i + 1).toLowerCase
  }



  val TELH_ONTOLOGY_PATH:String = ROOT_DIRECTORY + "TemporalTests.ofn"
  val TEMPORAL_CANCER_ONTOLOGY_PATH:String = ROOT_DIRECTORY + "temporal-cancer.ofn"
  val CANCER_ONTOLOGY_PATH:String = ROOT_DIRECTORY + "cancer.ofn"
  val CANCER_ONTOLOGY_PREFIX = "cancer#"

  val RESTAURANT_ONTOLOGY_PATH:String = ROOT_DIRECTORY + "restaurant.ofn"
  val RESTAURANT_ONTOLOGY_PREFIX:String = "restaurant#"

  val PATIENT_ONTOLOGY_PATH: String = ROOT_DIRECTORY + "patients.ofn" // "/Users/forkel/Data/SNOMED/snomed2017-english.ofn"
  val PATIENT_ONTOLOGY_PREFIX: String = "http://goasq.lri.fr/ontology/n2c2#"


  val INT_BEGIN = "dt_from"
  val INT_END = "dt_to"
  val TSRelation = "timepoints"
  val TSRelevantFunction = "relevant_timepoints"

}
