package de.tu_dresden.epistemic_rewriter.test

import java.io.File

import de.tu_dresden.epistemic_rewriter._
import de.tu_dresden.epistemic_rewriter.datatypes.{Answer, Variable}
import de.tu_dresden.epistemic_rewriter.test.CancerOntology._
import de.tu_dresden.epistemic_rewriter.test.RestaurantOntology._
import org.scalatest._

trait DBBuilder extends BeforeAndAfterEach { this: Suite =>

  val cancerOption = new OntologyOption(ConfigValues.CANCER_ONTOLOGY_PATH, true)
  val restaurantOption = new OntologyOption(ConfigValues.RESTAURANT_ONTOLOGY_PATH, true)
  val patientOption = new OntologyOption(ConfigValues.PATIENT_ONTOLOGY_PATH, true)

  var cancerHelper: OntologyHelper = _
  var restaurantHelper: OntologyHelper = _

  override def beforeEach() {
    val normalize = false
    cancerHelper = OntologyHelper.createOntologyHelper(new File(ConfigValues.CANCER_ONTOLOGY_PATH), None, normalize)
    restaurantHelper = OntologyHelper.createOntologyHelper(new File(ConfigValues.RESTAURANT_ONTOLOGY_PATH), None ,normalize)
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

class DatabaseModelSpec extends FlatSpec with DiagrammedAssertions with DBBuilder {



  /*"The patient model" should "be initialized" in {
    println(patientOption.getHelper.manager.getIRIMappers.toString)
    val dModel = new DatabaseModel(patientOption.getDBManager)
    assert(dModel.isInitialized(patientOption.getHelper.ontology))
  }*/


 "A database model" should "be created successfully from an ontology helper" in {

    val dModel1 = DatabaseModel.saveToDatabase(cancerHelper, cancerOption.getDBManager, true)
    assert(dModel1.isInitialized(cancerHelper.ontology))

    val dModel = TestHelper.time(DatabaseModel.saveToDatabase(restaurantHelper, restaurantOption.getDBManager, true), "Saving model to Database.")
    assert(dModel.isInitialized(restaurantHelper.ontology))

 }



  "FOQueries" should "be answered correcty over the data without rewriting" in {
    implicit val helper = restaurantHelper
    implicit val model = new DatabaseModel(restaurantOption.getDBManager, true)
    implicit val prefix = ConfigValues.RESTAURANT_ONTOLOGY_PREFIX

    val BOTH: Set[Answer] = Set(Answer(Map((Variable("x"), BobsDiner))), Answer(Map((Variable("x"), EvesDiner))))
    val BOB_ONLY: Set[Answer] = Set(Answer(Map((Variable("x"), BobsDiner))))
    val EVE_ONLY: Set[Answer] = Set(Answer(Map((Variable("x"), EvesDiner))))
    val NONE: Set[Answer] = Set.empty

    def testA(string: String, expAnswers:Set[Answer]) = {
      val q = FOQueryParser.parse(string, model.getDBManager())
      val answers = model.getAnswers(q)
      assert(answers == expAnswers, s"${q.pShow} \n Answers: $answers \n Expectd: $expAnswers")
    }

    testA("BologneseRestaurant(x)", BOTH)
    testA("∃(y,z).(serves(x,y) ∧ hasIngredient(y,z) ∧ ¬Spicy(z))", NONE)
    testA("∃(y).(serves(x,y))", BOTH)
    testA("∃(y,z).(serves(x,y) ∧ hasIngredient(y,z))", EVE_ONLY)
    testA("∃(y).((PenneBolognese(y) ∧ serves(x,y)) ∧ [∃(z).hasIngredient(y,z) → ∃(z).(¬Spicy(z) ∧ hasIngredient(y,z))])", NONE)
    testA("∃(y).((PenneBolognese(y) ∧ serves(x,y)) ∧ [∃(z).(hasIngredient(y,z) ∧ Thing(z)) → ∃(z).(¬Spicy(z) ∧ Thing(z) ∧ hasIngredient(y,z))])", NONE)
    testA("(PenneBolognese(y) ∧ [∃(z).(hasIngredient(y,z) ∧ Penne(z)) → ∃(z).(¬Spicy(z) ∧ Penne(z) ∧ hasIngredient(y,z))] ∧ [∃(z1).(hasIngredient(y,z1) ∧ Bolognese(z1)) → ∃(z1).(¬Spicy(z1) ∧ Bolognese(z1) ∧ hasIngredient(y,z1))])", NONE)

  }



  it should "be answered correctly in restaurant ontology by rewriting" in {

    implicit val helper = restaurantHelper
    implicit val model = new DatabaseModel(restaurantOption.getDBManager, true)
    implicit val prefix = ConfigValues.RESTAURANT_ONTOLOGY_PREFIX

    val BOTH: Set[Answer] = Set(Answer(Map((Variable("x"), BobsDiner))), Answer(Map((Variable("x"), EvesDiner))))
    val BOB_ONLY: Set[Answer] = Set(Answer(Map((Variable("x"), BobsDiner))))
    val EVE_ONLY: Set[Answer] = Set(Answer(Map((Variable("x"), EvesDiner))))
    val NONE: Set[Answer] = Set.empty

    TestHelper.testECQ("EX(y,z). (serves(x,y) AND hasIngredient(y,z) AND Pasta(z) AND NOT Spicy(z))", BOB_ONLY)
    TestHelper.testECQ("EX(y,z). (serves(x,y) AND hasIngredient(y,z) AND NOT Pasta(z) AND NOT Spicy(z))", BOTH)
    TestHelper.testECQ("(Thing(x))", { x :Set[Answer] => x.size == model.getIndividuals.size})

    TestHelper.testECQ("EX(x,y). serves(x,y)", Set(Answer(Map())))

    TestHelper.testECQ("EX(y). (serves(x,y) AND NOT Spicy(y))", BOB_ONLY)
    TestHelper.testECQ("BologneseRestaurant(x)", BOTH)
    TestHelper.testECQ("EX(y). serves(x,y) ", BOTH)
    TestHelper.testECQ("EX(y,z1). (serves(x,y) AND hasIngredient(y,z1))", BOTH)

    TestHelper.testECQ("EX(z). (hasIngredient(y,z) AND NOT Spicy(z))", Set(Answer(Map((Variable("y"), BobsPizza))), Answer(Map((Variable("y"), EvesPastaMeal)))))
    TestHelper.testECQ("EX(y,z). (serves(x,y) AND hasIngredient(y,z) AND NOT Spicy(z))", BOTH)


    TestHelper.testECQ("∃(z,z1).(hasIngredient(y,z) ∧ Pasta(z) ∧ ¬Spicy(z) AND hasIngredient(y,z1) ∧ Bolognese(z1) ∧ ¬Spicy(z1))", NONE)

  }

  it should "be answered correctly in cancer ontology by rewriting" in {

    implicit val helper = cancerHelper
    implicit val dModel = new DatabaseModel(cancerOption.getDBManager, true)
    implicit val prefix = ConfigValues.CANCER_ONTOLOGY_PREFIX

    TestHelper.testECQ("EX(y). (diagnosedWith(x,y) AND Cancer(y) AND NOT LungCancer(y))", Set(Answer(Map((Variable("x"), Bob))), Answer(Map((Variable("x"), Anvil))), Answer(Map((Variable("x"), Alice)))) )
    TestHelper.testECQ("EX(). (diagnosedWith(x,y) AND Cancer(y) AND NOT LungCancer(y))",
      { x: Set[Answer] => x == Set(Answer(Map((Variable("x"), Bob), (Variable("y"), BobsCancer)))) } )

  }




  "Subconcept queries" should "be answered correctly" in {

    implicit val dModel = new DatabaseModel(cancerOption.getDBManager, true)
    implicit val prefix = ConfigValues.CANCER_ONTOLOGY_PREFIX

    val subClasses = dModel.querySubClasses(Cancer, false)
    assert(subClasses == Set(Cancer, SkinCancer, PancreasCancer, LungCancer))

    assert(cancerHelper.queryInstances(diagnosedWith).getFlattened.size() > 0)
  }

  "Subrole queries" should "be answered correctly" in {


    val model = new DatabaseModel(cancerOption.getDBManager, true)

    assert(model.queryEquivalentRoles(diagnosedWith) == Set(diagnosedWith))
    assert(model.queryEquivalentRoles(dueTo) == Set(dueTo, causedBy))
    assert(model.queryEquivalentRoles(causedBy) == Set(dueTo, causedBy))

    assert(model.querySubRoles(diagnosedWith, true) == Set())
    assert(model.querySubRoles(diagnosedWith, false) == Set(diagnosedWith))
    assert(model.querySubRoles(dueTo, true) == Set(diagnosedWith))
    assert(model.querySubRoles(dueTo, false) == Set(diagnosedWith, dueTo, causedBy))

  }

  /*
  "Queries with constants" should "be answered correctly" in {
    val Bob = NamedIndividual("http://www.semanticweb.org/forkel/ontologies/2018/6/cancer#Bob")

    implicit val helper = new OntologyHelper(new File(ConfigValues.CANCER_ONTOLOGY_PATH))
    val model = helper.getCanonicalModel
    val dbmanager = DatabaseManager.getManager(ConfigValues.getConnectionParams(ConfigValues.CANCER))
    implicit val dModel = DatabaseModel.saveToDatabase(helper, dbmanager)
    implicit val prefix = ConfigValues.CANCER_ONTOLOGY_PREFIX

    TestHelper.testECQ("EX(). diagnosedWith(Bob,BobsCancer) AND PancreasCancer(BobsCancer)", { x: Set[Answer] => x == Set(Answer(Map())) } )

    TestHelper.testECQ("EX(). diagnosedWith(Bob,BobsCancer) AND PancreasCancer(BobsCancer) AND NOT SkinCancer(BobsCancer)", { x: Set[Answer] => x == Set(Answer(Map())) } )
    TestHelper.testECQ("EX(). diagnosedWith(Bob,BobsCancer) AND SkinCancer(BobsCancer)", { x: Set[Answer] => x.isEmpty } )

    TestHelper.testECQ("EX(y). diagnosedWith(Bob,y) AND PancreasCancer(y)", { x: Set[Answer] => x == Set(Answer(Map())) } )
    TestHelper.testECQ("EX(). diagnosedWith(x,BobsCancer) AND PancreasCancer(BobsCancer)", { x: Set[Answer] => x == Set(Answer(Map((Variable("x"), Bob)))) } )

  }

  "Queries with filters" should "be answered correctly" in {



    implicit val helper = new OntologyHelper(new File(ConfigValues.EXPERIMENTAL_CANCER_ONTOLOGY_PATH))
    val dbmanager = DatabaseManager.getManager(ConfigValues.getConnectionParams(ConfigValues.CANCER))
    implicit val dModel = DatabaseModel.saveToDatabase(helper, dbmanager)
    implicit val prefix = ConfigValues.CANCER_ONTOLOGY_PREFIX


    //val helper = new OntologyHelper(exp_cancer_ontology_file)
    val queryS = "EX(y). diagnosedWith(x,y) AND Cancer(y) AND NOT LungCancer(y)"
    val lungCQ = FOQueryParser.parse(queryS, dbmanager)

    val cqRewritten = EpistemicConjunctiveQueryRewriter.transform(EpistemicConjunctiveQuery(lungCQ), helper)

    assert(dModel.getAnswers(cqRewritten(0)) == Set(Answer(Map((Variable("x"), Bob)))), cqRewritten(0).pShow)
    assert(dModel.getAnswers(cqRewritten(1)) == Set(Answer(Map((Variable("x"), Anvil))), Answer(Map((Variable("x"), Alice)))), cqRewritten(1).pShow)
    assert(dModel.getAnswers(cqRewritten(2)) == Set(Answer(Map((Variable("x"), Bob)))), cqRewritten(2).pShow)
    assert(dModel.getAnswers(cqRewritten(3)) == Set(Answer(Map((Variable("x"), Bob))), Answer(Map((Variable("x"),Alice))), Answer(Map((Variable("x"),Anvil)))), cqRewritten(3).pShow)

    assert(dModel.getAnswers(cqRewritten) == Set(Answer(Map((Variable("x"), Bob))), Answer(Map((Variable("x"),Alice))), Answer(Map((Variable("x"),Anvil)))))
  }

  */

  /*"Instance queries" should "be answered correctly" in {
    val dbmanager = DatabaseManager.getManager(ConfigValues.getConnectionParams(ConfigValues.CANCER))
    val helper = new OntologyHelper(new File(ConfigValues.CANCER_ONTOLOGY_PATH))
    implicit val dModel = DatabaseModel.saveToDatabase(helper, dbmanager)
    implicit val prefix = ConfigValues.CANCER_ONTOLOGY_PREFIX

    val query = diagnosedWith some(SkinCancer)
    val instances = dModel.queryInstances(query).getFlattened.asScala
    assert(instances == Set(Alice))
    helper.reasoner.dispose()
  }*/

}