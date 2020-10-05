import java.io.File

import de.tu_dresden.epistemic_rewriter._
import de.tu_dresden.epistemic_rewriter.datatypes._
import com.github.nscala_time.time.Imports._
import org.scalatest._
import de.tu_dresden.epistemic_rewriter.test.CancerOntology._
import de.tu_dresden.epistemic_rewriter.test.TestHelper
import scala.collection.SortedSet

trait MyDBBuilder extends BeforeAndAfterEach {
  this: Suite =>

  val cancerOption = new OntologyOption(ConfigValues.TEMPORAL_CANCER_ONTOLOGY_PATH, true)
  val restaurantOption = new OntologyOption(ConfigValues.RESTAURANT_ONTOLOGY_PATH, true)
  val patientOption = new OntologyOption(ConfigValues.PATIENT_ONTOLOGY_PATH, true)

  val tp2011 = TimePoint.parse("2011-10-02T18:48:05")
  val tp19941010 = TimePoint.parse("1994-10-10T18:48:05")
  val tp19941005 = TimePoint.parse("1994-10-05T18:48:05")
  val tp19941002 = TimePoint.parse("1994-10-02T18:48:05")
  val tpFuture = TimePoint.parse("2012-10-02T18:48:05")
  val tpPast = TimePoint.parse("1990-10-02T18:48:05")

  var cancerHelper: OntologyHelper = _
  var restaurantHelper: OntologyHelper = _

  override def beforeEach() {
    val normalize = false
    cancerHelper = OntologyHelper.createOntologyHelper(new File(ConfigValues.TEMPORAL_CANCER_ONTOLOGY_PATH), None, normalize)
    restaurantHelper = OntologyHelper.createOntologyHelper(new File(ConfigValues.RESTAURANT_ONTOLOGY_PATH), None, normalize)



    super.beforeEach() // To be stackable, must call super.beforeEach
  }

  override def afterEach() {
    try {
      super.afterEach() // To be stackable, must call super.afterEach
    }
    finally {
      cancerHelper.reasoner.dispose()
      restaurantHelper.reasoner.dispose()
    }
  }
}

class TemporalDatabaseModelSpec extends FlatSpec with DiagrammedAssertions with MyDBBuilder {

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


  "Diamond Operators" should "be completed correctly" in {
    implicit val availableTimestamps: SortedSet[TimePoint] = SortedSet(tp19941002, tp19941005, tp19941010)


    assert(Rigid().complete(SortedSet(tp19941010)) == availableTimestamps, "Rigidity")
    assert(Increasing().complete(SortedSet(tp19941005)) == SortedSet(tp19941005, tp19941010), "Increasing")
    assert(Decreasing().complete(SortedSet(tp19941005)) == SortedSet(tp19941002, tp19941005), "Decreasing")
    assert(Convex().complete(SortedSet(tp19941002, tp19941010)) == availableTimestamps, "Bounded Convexity")
    assert(BoundedConvex(java.time.Duration.parse("P2D")).complete(SortedSet(tp19941002, tp19941010)) == SortedSet(tp19941002, tp19941010), "Bounded Convexity")
    assert(BoundedConvex(java.time.Duration.parse("P20D")).complete(SortedSet(tp19941002, tp19941010)) == availableTimestamps, "Bounded Convexity")


  }

  /*"A temporal database model" should "be created successfully from an ontology helper" in {

    val dModel1 = TemporalDatabaseModel.saveToDatabase(cancerHelper, cancerOption.getDBManager, true)
    assert(dModel1.isInitialized(cancerHelper.ontology))

  }
*/

  it should "create the correct timepoints" in {
    implicit val dModel = new TemporalDatabaseModel(cancerOption.getDBManager, true, cancerHelper)
    implicit val prefix = ConfigValues.CANCER_ONTOLOGY_PREFIX

    var timepoints = dModel.getTimePoints()
    assert(timepoints.size == 7, "Timepoints are created correctly.")
    for (tp <- timepoints) {
      assert(dModel.lookup(Human,tp).toSet == Set(Bob, Alice, Anvil), s"Rigidity test at $tp")
    }

  }
  it should "contain the correct assertions" in {
    implicit val dModel = new TemporalDatabaseModel(cancerOption.getDBManager, true, cancerHelper)
    implicit val prefix = ConfigValues.CANCER_ONTOLOGY_PREFIX


    // Rigidity
    for (tp <- dModel.getTimePoints()) {
      dModel.setTimePoint(tp)
      assert(dModel.lookup(Human).toSet == Set(Bob, Alice, Anvil), s"Rigidity test at $tp")
    }

    // Bounded Convexity
    assert(dModel.lookup(PancreasCancerPatient,tp19941002).toSet.contains(Bob), s"Inference at ${tp19941002} using ELK")
    assert(dModel.lookup(PancreasCancerPatient,tp19941005).toSet.contains(Bob), s"Bounded Convexity at ${tp19941005} using ELK")
  }


  "A Temporal First Order Query" should "with no temporal operators" in {
    implicit val helper = cancerHelper
    implicit val model = new TemporalDatabaseModel(cancerOption.getDBManager, true, helper)
    implicit val prefix = ConfigValues.CANCER_ONTOLOGY_PREFIX


    testQ(TemporalAtom(FOQueryParser.parse("EX(). (Human(x) AND NOT CancerPatient(x))", model.getDBManager())), 1, tp19941010)
    val a = Set(Answer(Map((Variable("x"), Bob))), Answer(Map((Variable("x"), Anvil))))
    testQ1(TemporalAtom(FOQueryParser.parse("EX(y). (diagnosedWith(x,y) AND Cancer(y))", model.getDBManager())), _ == a, tp19941010)
    testQ1(TemporalAtom(FOQueryParser.parse("EX(y). (diagnosedWith(x,y) AND Cancer(y))", model.getDBManager())), _ == Set(Answer(Map((Variable("x"), Bob)))), tp19941002)
  }

  it should "be rewritten correctly" in {
    implicit val helper = cancerHelper
    implicit val model = new TemporalDatabaseModel(cancerOption.getDBManager, true, helper)
    implicit val prefix = ConfigValues.CANCER_ONTOLOGY_PREFIX


    testQ(TemporalAtom(FOQueryParser.parse("EX(y). (diagnosedWith(x,y) AND Cancer(y))", model.getDBManager())), 1, tp2011, "Rewriting is not working")
  }

  it should "be answered correctly over representative timepoints" in {
    implicit val helper = cancerHelper
    implicit val model = new TemporalDatabaseModel(cancerOption.getDBManager, true, helper)
    implicit val prefix = ConfigValues.CANCER_ONTOLOGY_PREFIX


    testQ(TemporalAtom(FOQueryParser.parse("EX(). (Human(x) AND NOT CancerPatient(x))", model.getDBManager())), 3, tpFuture)
    testQ(TemporalAtom(FOQueryParser.parse("EX(). (Human(x) AND NOT CancerPatient(x))", model.getDBManager())), 3, tpPast)
  }

  it should "eventually is answered correctly" in {

    implicit val helper = cancerHelper
    implicit val model = new TemporalDatabaseModel(cancerOption.getDBManager, true, helper)
    implicit val prefix = ConfigValues.CANCER_ONTOLOGY_PREFIX


    val cpq = TemporalAtom(FOQueryParser.parse("(CancerPatient(x))", model.getDBManager()))

    val q0 = TemporalEventually(RelInterval(-367.days, 0.days), cpq)
    testQ(q0, 1, tpFuture)
    testQ(q0, 1, tp2011)
    val q1 = TemporalEventually(RelInterval(-6202.days, 0.days), cpq)
    testQ(q1, 1, tpFuture)
    testQ(q1, 3, tp2011)

    //History of Test
    val q2 = TemporalEventually(RelInterval(NegInfinitePeriod(), 0.days), cpq)
    testQ(q2, 3, tpFuture)
    testQ(q2, 3, tp2011)
  }

  it should "conjunction is answered correctly" in {

    implicit val helper = cancerHelper
    implicit val model = new TemporalDatabaseModel(cancerOption.getDBManager, true, helper)
    implicit val prefix = ConfigValues.CANCER_ONTOLOGY_PREFIX


    val cpq = TemporalAtom(FOQueryParser.parse("(CancerPatient(x))", model.getDBManager()))

    val q0 = TemporalEventually(RelInterval(-367.days, 0.days), cpq)
    val q1 = TemporalEventually(RelInterval(-6202.days, 0.days), cpq)
    testQ(TemporalConj(List(q0, q1)), 1, tp2011, "Conjunction operator")
  }

  it should "disjunction is answered correctly" in {

    implicit val helper = cancerHelper
    implicit val model = new TemporalDatabaseModel(cancerOption.getDBManager, true, helper)
    implicit val prefix = ConfigValues.CANCER_ONTOLOGY_PREFIX


    val cpq = TemporalAtom(FOQueryParser.parse("(CancerPatient(x))", model.getDBManager()))

    val q0 = TemporalEventually(RelInterval(-367.days, 0.days), cpq)
    val q1 = TemporalEventually(RelInterval(-6202.days, 0.days), cpq)
    testQ(TemporalDisj(List(q0, q1)), 3, tp2011, "Disjunction operator")
  }

  it should "negation is answered correctly" in {

    implicit val helper = cancerHelper
    implicit val model = new TemporalDatabaseModel(cancerOption.getDBManager, true, helper)
    implicit val prefix = ConfigValues.CANCER_ONTOLOGY_PREFIX


    val cpq = TemporalAtom(FOQueryParser.parse("(CancerPatient(x))", model.getDBManager()))

    val q0 = TemporalEventually(RelInterval(-367.days, 0.days), cpq)
    val q1 = TemporalEventually(RelInterval(-6202.days, 0.days), cpq)

    testQ(TemporalConj(List(TemporalNeg(q0), q1)), 2, tp2011, "Negation operator")
    testQ(TemporalConj(List(TemporalNeg(q1), q0)), 0, tp2011,"Negation operator")
  }

  it should "always is answered correctly" in {

    implicit val helper = cancerHelper
    implicit val model = new TemporalDatabaseModel(cancerOption.getDBManager, true, helper)
    implicit val prefix = ConfigValues.CANCER_ONTOLOGY_PREFIX


    val cpq = TemporalAtom(FOQueryParser.parse("(PancreasCancerPatient(x))", model.getDBManager()))

    val q0 = TemporalAlways(RelInterval(-3.days, 0.days), cpq)
    testQ(q0, 0, tpFuture)
    testQ(q0, 0, tp19941002)
    testQ(q0, 1, tp19941010)
    testQ(q0, 1, tp19941005)
    val q1 = TemporalAlways(RelInterval(-3.days, 3.days), cpq)
    testQ(q1, 1, tp19941005)

    val hq = TemporalAtom(FOQueryParser.parse("(Human(x))", model.getDBManager()))
    val q2 = TemporalAlways(RelInterval(NegInfinitePeriod(), 0.days), hq)
    testQ(q2, 3, tp19941005)

    val q3 = TemporalAlways(RelInterval(0.days, PosInfinitePeriod()), hq)
    testQ(q3, 3, tp19941005)

    val q4 = TemporalAlways(RelInterval(NegInfinitePeriod(), PosInfinitePeriod()), hq)
    testQ(q4, 3, tp19941005)

  }

  it should "implication is answered correctly" in {

    implicit val helper = cancerHelper
    implicit val model = new TemporalDatabaseModel(cancerOption.getDBManager, true, helper)
    implicit val prefix = ConfigValues.CANCER_ONTOLOGY_PREFIX


    val cpq = TemporalAtom(FOQueryParser.parse("(CancerPatient(x))", model.getDBManager()))
    val hq = TemporalAtom(FOQueryParser.parse("(Human(x))", model.getDBManager()))

    val q0 = TemporalImpl(TemporalEventually(RelInterval(-60000.days, 60000.days), cpq),TemporalAlways(RelInterval(-60000.days, 60000.days), hq))
    testQ(q0, 3, tpFuture)
    testQ(q0, 3, tp19941002)



  }

  "Operators" should "be nestable" in {

    implicit val helper = cancerHelper
    implicit val model = new TemporalDatabaseModel(cancerOption.getDBManager, true, helper)
    implicit val prefix = ConfigValues.CANCER_ONTOLOGY_PREFIX


    val q1 = TemporalAtom(FOQueryParser.parse("EX(y). (diagnosedWith(x,y) AND Cancer(y))", model.getDBManager()))
    val q2 = TemporalAtom(FOQueryParser.parse("(Human(x))", model.getDBManager()))
    // Conjunction
    testQ(TemporalConj(List(q1, q2)),2, tp19941010)
    testQ(TemporalConj(List(q1, q2)),1, tp2011)
    // Disjunction
    testQ(TemporalDisj(List(q1, q2)),3, tp2011)


    /*testQ(TemporalConj(List(q1, TemporalAtom(FOQueryParser.parse("CancerPatient(x)", model.getDBManager())))), 2)

    val qCNLC = TemporalAtom(FOQueryParser.parse("EX(). (diagnosedWith(x,y) AND Cancer(y) AND NOT LungCancer(y))", model.getDBManager()))
    testQ(qCNLC, 2)
    testQ(TemporalConvex(60.days, qCNLC), 3)

    // Test Eventually operator with representative timepoints
    testQ(TemporalEventually(RelInterval(-365.days, 0.days), TemporalAtom(FOQueryParser.parse("EX(). (CancerPatient(x))", model.getDBManager()))), 1, Some(tpFuture))
    //testQ(TemporalConvex(730.days, TemporalAtom(FOQueryParser.parse("EX(). (Human(x) AND NOT CancerPatient(x))", model.getDBManager()))), 2, Some(tpFuture))

    // Test Always operator with representative timepoints

    //testQ(TemporalAlways(RelInterval(-20.days,0.days), TemporalAtom(FOQueryParser.parse("EX(). (Human(x) AND NOT CancerPatient(x))", model.getDBManager()))), 5)

    // Test nested temporal operators
    //testQ(TemporalEventually(RelInterval(-365.days, 0.days),TemporalAlways(RelInterval(-20.days,0.days), TemporalAtom(FOQueryParser.parse("EX(). (Human(x) AND NOT CancerPatient(x))", model.getDBManager())))), 5)

    // Test temporal operator which spans into representative interval, but does not include the representative directly

    // Test the cases for past and future in this case too

    // TODO: Maybe return validity intervals instead of single timepoints? If not, can it always be recovered?? Probably not...

    */

  }

  "Non Temporal FOQueries" should "be answered correctly when ignoring all temporal information" in {

    implicit val helper = cancerHelper
    implicit val dModel = new DatabaseModel(cancerOption.getDBManager, true)
    implicit val prefix = ConfigValues.CANCER_ONTOLOGY_PREFIX

    TestHelper.testECQ("EX(y). (diagnosedWith(x,y) AND Cancer(y) AND NOT LungCancer(y))", Set(Answer(Map((Variable("x"), Bob))), Answer(Map((Variable("x"), Anvil))), Answer(Map((Variable("x"), Alice)))))
    TestHelper.testECQ("EX(). (diagnosedWith(x,y) AND Cancer(y) AND NOT LungCancer(y))",
      { x: Set[Answer] => x == Set(Answer(Map((Variable("x"), Bob), (Variable("y"), BobsCancer)))) })

  }


  /*"Subconcept queries" should "be answered correctly" in {

    implicit val dModel = new TemporalDatabaseModel(cancerOption.getDBManager, true)
    implicit val prefix = ConfigValues.CANCER_ONTOLOGY_PREFIX

    val subClasses = dModel.querySubClasses(Cancer, false)
    assert(subClasses == Set(Cancer, SkinCancer, PancreasCancer, LungCancer))


    assert(cancerHelper.queryInstances(diagnosedWith).getFlattened.size() > 0)
  }

  "Subrole queries" should "be answered correctly" in {


    val model = new TemporalDatabaseModel(cancerOption.getDBManager, true)

    assert(model.queryEquivalentRoles(diagnosedWith) == Set(diagnosedWith))
    assert(model.queryEquivalentRoles(dueTo) == Set(dueTo, causedBy))
    assert(model.queryEquivalentRoles(causedBy) == Set(dueTo, causedBy))

    assert(model.querySubRoles(diagnosedWith, true) == Set())
    assert(model.querySubRoles(diagnosedWith, false) == Set(diagnosedWith))
    assert(model.querySubRoles(dueTo, true) == Set(diagnosedWith))
    assert(model.querySubRoles(dueTo, false) == Set(diagnosedWith, dueTo, causedBy))

  }

  */


}