package de.tu_dresden.epistemic_rewriter.test

import java.io.File

import de.tu_dresden.epistemic_rewriter.{ConfigValues, OntologyHelper}
import de.tu_dresden.epistemic_rewriter.test.CancerOntology._
import org.phenoscape.scowl._
import org.scalatest._

import scala.collection.JavaConverters._

trait OHBuilder extends BeforeAndAfterEach { this: Suite =>
  var helper: OntologyHelper = _

  override def beforeEach() {
    helper = OntologyHelper.createOntologyHelper(new File(ConfigValues.CANCER_ONTOLOGY_PATH), None, false)

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

class OntologyHelperSpec extends FlatSpec with DiagrammedAssertions with OHBuilder {

  "Loading and Completing of an Ontology" should "work" in {
    // Compute and save inferred axioms
    val outputfile = new File("data/ontologies/cancer-inferred.owl")
    var managerInferred = helper.addInferredAxioms(outputfile)
  }

  "Instance queries" should "be answered correctly" in {
    val query = diagnosedWith some(SkinCancer)
    val instances = helper.queryInstances(query).getFlattened.asScala
    assert(instances == Set(Alice, Anvil))
  }

  "Subclass computation" should "give correct results for classes" in {

  }
  it should "work with complex classes" in  {
    val query = diagnosedWith some(SkinCancer)
    val subClasses = helper.querySubClasses(query, false).asScala
    assert(subClasses == Set(SkinCancerPatient))

    assert(helper.isSubClassOf(diagnosedWith some SkinCancer, diagnosedWith some Cancer))
    assert(!helper.isSubClassOf(diagnosedWith some Cancer, diagnosedWith some SkinCancer))
    assert(helper.isSubClassOf(diagnosedWith some SkinCancer, dueTo some Cancer))
  }

  it should "give correct results for high level helper functions" in {
    assert( helper.queryComplexSuperClasses(SkinCancerPatient, true) == Set(diagnosedWith some SkinCancer, SkinCancerPatient))
    assert( helper.queryComplexSuperClasses(diagnosedWith some SkinCancer, true) == Set(diagnosedWith some SkinCancer, SkinCancerPatient))

    val exp = Set(diagnosedWith some Cancer, diagnosedWith some SkinCancer, SkinCancerPatient, CancerPatient, Human)
    val res = helper.queryComplexSuperClasses(SkinCancerPatient, false).toSet
    assert( res == exp)

  }

  "Subrole queries" should "work" in {
    assert(helper.querySubRoles(diagnosedWith, true) == Set())
    assert(helper.querySubRoles(diagnosedWith, false) == Set(diagnosedWith))
    assert(helper.querySubRoles(dueTo, true) == Set(diagnosedWith))
    assert(helper.querySubRoles(dueTo, false) == Set(diagnosedWith, dueTo, causedBy))
    assert(helper.queryEquivalentRoles(diagnosedWith) == Set(diagnosedWith))
    assert(helper.queryEquivalentRoles(dueTo) == Set(dueTo, causedBy))
    assert(helper.queryEquivalentRoles(causedBy) == Set(dueTo, causedBy))
  }

  "Sorting by subsumption" should "work" in {
    val sorted = List(PancreasCancer, SkinCancer, LungCancer, Cancer)
    assert(helper.sortClassesBySubsumption(sorted.reverse).last == Cancer)
    //assert(helper.getMinimalClasses(sorted) == List(PancreasCancer, SkinCancer, LungCancer))
    //print(helper.sortRolesBySubsumption(helper.ontology.getObjectPropertiesInSignature().asScala.toList))
  }

  "Justifications" should "work" in  {
    assert(helper.computeJustification(Cancer, BobsCancer) == Set(PancreasCancer))
  }



}
