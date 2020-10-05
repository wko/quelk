import java.io.File

import de.tu_dresden.epistemic_rewriter._
import de.tu_dresden.epistemic_rewriter.datatypes._
import org.scalatest._



class N2C2TestsSpec extends FlatSpec with DiagrammedAssertions {

  def testQ(q: TemporalFOQuery, exp: Int, tp: TimePoint, message: String = "")(implicit model:TemporalDatabaseModel) = {
    model.setTimePoint(tp)
    val answers = model.getAnswers(q)
    assert(answers.size == exp, s"${q.pShow} \n Got ${answers.size} answers; expected ${exp} \n ${answers} at timepoint $tp\n" + message)
  }

  def testQ1(q: TemporalFOQuery, exp: Set[Answer] => Boolean, tp: TimePoint)(implicit model:TemporalDatabaseModel) = {
    model.setTimePoint(tp)
    val answers = model.getAnswers(q)
    assert(exp(answers), s"${q.pShow} \n Answers: $answers")
  }

  /*"The patient model" should "be initialized" in {
    println(patientOption.getHelper.manager.getIRIMappers.toString)
    val dModel = new DatabaseModel(patientOption.getDBManager)
    assert(dModel.isInitialized(patientOption.getHelper.ontology))
  }*/


  "Ontology" should "be initialized" in {
    val n2c2Option = new OntologyOption(ConfigValues.ROOT_DIRECTORY + "temporal_n2c2.ofn", true)
    val normalize = false
    val n2c2Helper = OntologyHelper.createOntologyHelper(new File(ConfigValues.ROOT_DIRECTORY + "temporal_n2c2.ofn"), None, normalize)

    implicit val dModel = new TemporalDatabaseModel(n2c2Option.getDBManager, true, n2c2Helper)
    dModel.saveToDatabase()

    val tp2143 = TimePoint.parse("2143-05-19T00:00:00")
    assert(dModel.isInitialized(n2c2Helper.ontology))
    assert(dModel.getTimePoints().toList.contains(tp2143))
    assert(dModel.getRealTimePoints().toList.size >= 5)
    n2c2Helper.reasoner.dispose()
  }

  it should "contain the correct information" in {
    val n2c2Option = new OntologyOption(ConfigValues.ROOT_DIRECTORY + "temporal_n2c2.ofn", true)
    val normalize = false
    val n2c2Helper = OntologyHelper.createOntologyHelper(new File(ConfigValues.ROOT_DIRECTORY + "temporal_n2c2.ofn"), None, normalize)

    implicit val dModel = new TemporalDatabaseModel(n2c2Option.getDBManager, true, n2c2Helper)

    val tp2143 = TimePoint.parse("2143-05-19T00:00:00")
    assert(dModel.isInitialized(n2c2Helper.ontology))
    assert(dModel.getTimePoints().toList.contains(tp2143))
    assert(dModel.getRealTimePoints().toList.size >= 5)
    n2c2Helper.reasoner.dispose()
  }




}