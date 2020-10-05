import java.io.File
import java.time.Duration

import de.tu_dresden.epistemic_rewriter
import de.tu_dresden.epistemic_rewriter._
import de.tu_dresden.epistemic_rewriter.datatypes._
import org.phenoscape.scowl._
import org.scalatest._


trait TELHOntologyLoader extends BeforeAndAfterEach { this: Suite =>
  val prefix = "T#"
  val A = Class(s"${prefix}A")
  val A1 = Class(s"${prefix}A1")
  val A2 = Class(s"${prefix}A2")
  val A3 = Class(s"${prefix}A3")
  val B = Class(s"${prefix}B")
  val B1 = Class(s"${prefix}B1")
  val B2 = Class(s"${prefix}B2")
  val B3 = Class(s"${prefix}B3")
  val s = ObjectProperty(s"${prefix}s")
  val s1 = ObjectProperty(s"${prefix}s1")
  val r1 = ObjectProperty(s"${prefix}r1")
  val r2 = ObjectProperty(s"${prefix}r2")
  val r3 = ObjectProperty(s"${prefix}r3")



  var helper: OntologyHelper = _

  override def beforeEach() {
    val normalize = false
    helper = OntologyHelper.createOntologyHelper(new File(ConfigValues.TELH_ONTOLOGY_PATH), None, normalize)
    super.beforeEach() // To be stackable, must call super.beforeEach
  }

  override def afterEach() {
    try {
      super.afterEach() // To be stackable, must call super.afterEach
    }
    finally {
      helper.reasoner.dispose()
    }
  }
}


class TELHReasonerSpec extends FlatSpec with DiagrammedAssertions with TELHOntologyLoader {

  "Diamonds" should "compute correct inf and sup" in {

    val ds:List[Diamond] = List[Diamond](Diamond.MIN_CONVEX, BoundedConvex(Duration.parse("PT5S")), Convex(), Increasing(), Decreasing(), Rigid())

    // Symmetry
    for (d1 <- ds; d2 <- ds) {
      assert(Diamond.sup(d1, d2) == Diamond.sup(d2, d1), "Symmetry")
      assert(Diamond.inf(d1, d2) == Diamond.inf(d2, d1), "Symmetry")
    }

    // Reflexivity
    for (d1 <- ds) {
      assert(Diamond.sup(d1, d1) == d1, "Reflexivity")
      assert(Diamond.inf(d1, d1) == d1, "Reflexivity")
    }


    /*assert(Diamond.sup(NoDiamond(), Diamond.MIN_CONVEX) == Diamond.MIN_CONVEX)
    assert(Diamond.inf(NoDiamond(), Diamond.MIN_CONVEX) == NoDiamond())*/

    assert(Diamond.sup(Diamond.MIN_CONVEX, Convex()) == Convex())
    assert(Diamond.sup(Diamond.MIN_CONVEX, Increasing()) == Increasing())
    assert(Diamond.sup(Increasing(), Decreasing()) == Rigid())
    assert(Diamond.sup(Decreasing(), Increasing()) == Rigid())
    assert(Diamond.inf(Increasing(), Decreasing()) == Convex())



  }

  "The reasoner" should "parse the Temporal Facts" in {
    val reasoner = TELHReasonerFactory.createReasoner(helper.ontology)

    val subClassAssertions = reasoner.getTemporalSubClassOfAxioms
    subClassAssertions.contains(TemporalSubClassOfAxiom(Rigid(), SubClassOf(A,A)))
  }




  it should "saturate the TBOX" in {
    val reasoner = TELHReasonerFactory.createReasoner(helper.ontology)

    // Role Hierarchy
    val rAxioms = reasoner.saturate(reasoner.getTemporalSubObjectPropertyOfAxioms)
    rAxioms.foreach(println(_))
    assert(rAxioms.contains(epistemic_rewriter.TemporalSubObjectPropertyOfAxiom(Diamond.MIN_CONVEX, r1 SubPropertyOf  r1)), "T3")
    assert(rAxioms.contains(epistemic_rewriter.TemporalSubObjectPropertyOfAxiom(BoundedConvex(Duration.parse("PT5S")), r1 SubPropertyOf r3)), "T5")

    // Concept Hierarchy
    val axioms = reasoner.saturate(reasoner.getTemporalSubClassOfAxioms)(rAxioms)
    axioms.foreach(println(_))



    assert(axioms.contains(TemporalSubClassOfAxiom(Diamond.MIN_CONVEX, A1 SubClassOf A1)), "T1")
    assert(axioms.contains(TemporalSubClassOfAxiom(Rigid(), A1 SubClassOf OWLThing)), "T2")


    assert(axioms.contains(TemporalSubClassOfAxiom(Rigid(), A1 SubClassOf A3)), "T4")




    assert(axioms.contains(TemporalSubClassOfAxiom(Convex(), A SubClassOf B)), "T6")

    assert(axioms.contains(TemporalSubClassOfAxiom(Diamond.MIN_CONVEX, r1 some OWLNothing SubClassOf OWLNothing)), "T7")

    assert(axioms.contains(TemporalSubClassOfAxiom(Increasing(), A SubClassOf B2)), "T8a")

    assert(axioms.contains(TemporalSubClassOfAxiom(Rigid(), A SubClassOf B3)), "T8b")
  }

  it should "saturate the ABox" in {

  }




}
