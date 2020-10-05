package de.tu_dresden.epistemic_rewriter.benchmark


/*
class SnomedSpec {
  val ontfile = new File(ConfigValues.PATIENT_ONTOLOGY_PATH)
  implicit val helper = new OntologyHelper(ontfile)
  implicit val model = new DatabaseModel(DatabaseManager.getManager(ConfigValues.getConnectionParams(ConfigValues.PATIENTS)))

  test("Rewriting works") {

    val dbm = DatabaseManager.getManager(ConfigValues.getConnectionParams(ConfigValues.PATIENTS))
    val ecqS = "EX(y,z). Patient(x) AND hasFinding(x,y) AND 363698007(y,z) AND 170887008(z)"
    val ecq = EpistemicConjunctiveQuery(FOQueryParser.parse(ecqS, dbm))
    val queries = EpistemicConjunctiveQueryRewriter.transform(ecq, helper)
    assert(queries.size == 2)

  }


  test("Saving model to Database") {
    if (!model.isInitialized(helper.ontology)) {
      val mModel = TestHelper.time(helper.getCanonicalModel, "Creating canonical Model..")
      TestHelper.time(model.saveToDatabase(mModel), "Saving Model to Database")
    }
  }

  test("Answering an ECQ with an added assertion") {


    val skinSkar : OWLClass = Class("http://snomed.info/id/300398009")
    assert(helper.getLabel(skinSkar) == Some("Abdominal skin scar (finding)"))

    TestHelper.testECQ("MAJOR-DIABETES(x)", { x:Set[Answer] =>  x.size == 93} )

    val ecqS = "EX(y). Patient(x) AND hasFinding(x,y)"
    //val ecq:EpistemicConjunctiveQuery = EpistemicConjunctiveQuery().+("x", globIRI + "Patient" , Polarity.POSITIVE).+("x", "y",globIRI + "hasFinding", Polarity.POSITIVE).setAnswerVariables(List(Term("x")))
    TestHelper.testECQ(ecqS, {x: Set[Answer] => x.size == 202})



    // Diabetes Mellitus
    TestHelper.testECQ("EX(y). Patient(x) AND hasFinding(x,y) AND 73211009(y)", { x: Set[Answer] =>  x.size == 173})

    // Finger Injury: Disease with RoleGroup FindingSite Finger
    TestHelper.testECQ("EX(y,z,z1,z2). Patient(x) AND hasFinding(x,y) AND 64572001(y) AND 609096000(y,z) AND 363698007(z,z1) AND 7569003(z1) AND 116676008(z,z2) AND 19130008(z2)", { x: Set[Answer] =>  x.isEmpty })


  }

*/




