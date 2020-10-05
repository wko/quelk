package de.tu_dresden.epistemic_rewriter.test

import de.tu_dresden.epistemic_rewriter.FOQueryParser._
import de.tu_dresden.epistemic_rewriter._
import de.tu_dresden.epistemic_rewriter.datatypes._
import org.phenoscape.scowl._
import org.scalatest._

// See example at https://github.com/liveontologies/elk-reasoner/wiki/ElkOwlApi

class QuerySpec extends FlatSpec {
  val Alice = NamedIndividual("http://www.semanticweb.org/forkel/ontologies/2018/6/cancer#Alice")
  val Bob = NamedIndividual("http://www.semanticweb.org/forkel/ontologies/2018/6/cancer#Bob")
  val Anvil = NamedIndividual("http://www.semanticweb.org/forkel/ontologies/2018/6/cancer#Anvil")
  val BobsCancer = NamedIndividual("http://www.semanticweb.org/forkel/ontologies/2018/6/cancer#BobsCancer")
  val dueTo = ObjectProperty("http://www.semanticweb.org/forkel/ontologies/2018/6/cancer#dueTo")
  val causedBy = ObjectProperty("http://www.semanticweb.org/forkel/ontologies/2018/6/cancer#causedBy")
  val SkinCancer = Class("http://www.semanticweb.org/forkel/ontologies/2018/6/cancer#SkinCancer")
  val PancreasCancer = Class("http://www.semanticweb.org/forkel/ontologies/2018/6/cancer#PancreasCancer")
  val skinCancerPatient = Class("http://www.semanticweb.org/forkel/ontologies/2018/6/cancer#SkinCancerPatient")
  val diagnosedWith = ObjectProperty("http://www.semanticweb.org/forkel/ontologies/2018/6/cancer#diagnosedWith")

  val restaurantOption = new OntologyOption(ConfigValues.RESTAURANT_ONTOLOGY_PATH, true)


  "Parsing of Queries" should "be correct" in {
    parse("serves(x,y)")
    parse("Think(x)")

    parse("(Think(x) AND serves(x,y))")
    parse("EX(). (serves(x,y))", exQuantification)
    parse("EX(). serves(x,y)", exQuantification)
    parse("EX(x,y). (serves(x,y))", exQuantification)
    parse("Think(x) IMPL serves(x,y)", implication)
    parse("∃(z).(hasIngredient(z,y) ∧ Thing(z)) → ∃(z).(¬Spicy(z) ∧ Thing(z) ∧ hasIngredient(z,y))", implication)
    parse("∃(y).(serves(y,x) ∧ PenneBolognese(y)) → ∃(y).(PenneBolognese(y) ∧ serves(y,x) ∧ [∃(z).(hasIngredient(z,y) ∧ Thing(z)) → ∃(z).(¬Spicy(z) ∧ Thing(z) ∧ hasIngredient(z,y))])", implication)
    parse("EX(y,z). (serves(x,y) AND hasIngredient(y,z) AND NOT Spicy(z) AND Spicy(Tom))")
    parse("(BologneseRestaurant(x) ∧ [∃(y).(serves(y,x) ∧ PenneBolognese(y)) → ∃(y).(PenneBolognese(y) ∧ serves(y,x) ∧ [∃(z).(hasIngredient(z,y) ∧ Thing(z)) → ∃(z).(¬Spicy(z) ∧ Thing(z) ∧ hasIngredient(z,y))])])")
  }

  "Equality" should "be defined correctly" in {
    val q1 = parse("∃(y).((PenneArrabiata(y) ∧ PenneBolognese(y) ∧ serves(x,y)) ∧ [∃(z1).(hasIngredient(y,z1) ∧ Bolognese(z1)) → ∃(z1).(¬Spicy(z1) ∧ Bolognese(z1) ∧ hasIngredient(y,z1))])")
    val q2 = parse("∃(y).((PenneBolognese(y) AND PenneArrabiata(y) ∧ serves(x,y)) ∧ [∃(z1).(hasIngredient(y,z1) ∧ Bolognese(z1)) → ∃(z1).(¬Spicy(z1) ∧ Bolognese(z1) ∧ hasIngredient(y,z1))])")

    assert(q1 == q2)

  }

  "Printing of Queries" should "produce correct formulas" in {
    def parsePrintParse(string: String) = {
      val q = parse(string)
      val q1 = parse(q.pShow)
      assert(q == q1)
    }
    parsePrintParse("serves(x,y)")
    parsePrintParse("Think(x)")
    parsePrintParse("(Think(x) AND serves(x,y))")
    parsePrintParse("EX(y,z). (serves(x,y) AND hasIngredient(y,z) AND NOT Spicy(z) AND Spicy(Tom))")
    parsePrintParse("(BologneseRestaurant(x) ∧ [∃(y).(serves(y,x) ∧ PenneBolognese(y)) → ∃(y).(PenneBolognese(y) ∧ serves(y,x) ∧ [∃(z).(hasIngredient(z,y) ∧ Thing(z)) → ∃(z).(¬Spicy(z) ∧ Thing(z) ∧ hasIngredient(z,y))])])")
  }

  "Queries functions" should "return the right values" in {

    implicit val helper = restaurantOption.getHelper()

    val query = FOQueryParser.parse("EX(y,z). (serves(x,y) AND hasIngredient(y,z) AND NOT Spicy(z) AND Spicy(Tom))")



    assert(query.ecq.getPredecessors(Constant("Tom")) == Set.empty)
    assert(query.ecq.getPredecessors(Variable("z")) == Set(Variable("y")))

    assert(query.ecq.getSuccessors(Constant("Tom")) == Set.empty)
    assert(query.ecq.getSuccessors(Variable("y")) == Set(Variable("z")))

    assert(query.ecq.getRootTerms() == Set(Variable("x"), Constant("Tom")))

    assert(query.ecq.getLeafTerms() == Set(Variable("z"), Constant("Tom")), "LeafTerms")
    assert(query.ecq.getNonAnswerLeafVariables() == Set(Variable("z")), "nonAnswerLeafTerms")

    assert(query.ecq.getLiteralsContaining(Variable("z")) == List(PosLiteral(RoleAtom(ObjectProperty("hasIngredient"), Variable("y"), Variable("z"))), NegLiteral(ClassAtom(Class("Spicy"), Variable("z")))))

    assert(query.ecq.getClassAtoms(Constant("Tom"), Polarity.Pos) == Set(ClassAtom(Class("Spicy"), Constant("Tom"))))
    assert(query.ecq.getClassAtoms(Variable("z"), Polarity.Pos) == Set.empty)
    assert(query.ecq.getClassAtoms(Variable("z"), Polarity.Neg) == Set(ClassAtom(Class("Spicy"), Variable("z"))))

    assert(query.ecq.getIncomingRoleAtoms(Variable("z"), Polarity.Pos) == Set(RoleAtom(ObjectProperty("hasIngredient"), Variable("y"), Variable("z"))))
    assert(query.ecq.getOutgoingRoleAtoms(Variable("x"), Polarity.Pos) == Set(RoleAtom(ObjectProperty("serves"), Variable("x"), Variable("y"))))
    assert(query.ecq.getOutgoingRoleAtoms(Variable("z"), Polarity.Pos) == Set.empty)
  }

  "Temporal Queries" should "be parsed correctly" in {
    val cancerOption = new OntologyOption(ConfigValues.TEMPORAL_CANCER_ONTOLOGY_PATH, true)

    val cancerStr = "CancerPatient(x)"

    TemporalFOQueryParser.parse(cancerStr, cancerOption.getDBManager)

    TemporalFOQueryParser.parse(s"E[P-2D,P2D] ($cancerStr)", cancerOption.getDBManager)
  }



}
