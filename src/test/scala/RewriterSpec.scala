package de.tu_dresden.epistemic_rewriter.test

import de.tu_dresden.epistemic_rewriter._
import org.scalatest.FlatSpec

class RewriterSpec extends FlatSpec {
  val cancerOption = new OntologyOption(ConfigValues.CANCER_ONTOLOGY_PATH, true)
  val restaurantOption = new OntologyOption(ConfigValues.RESTAURANT_ONTOLOGY_PATH, true)

  "Rewriting" should "give correct results" in {

    val helper = restaurantOption.getHelper()
    val model = new DatabaseModel(restaurantOption.getDBManager, true)
    val ecq = FOQueryParser.parse("EX(y,z). serves(x,y) AND hasIngredient(y,z) AND Pasta(z) AND NOT Spicy(z)", restaurantOption.getDBManager)

    val rewritings = EpistemicConjunctiveQueryRewriter.transform(ecq, helper)

    assert(rewritings(1).foFilters.nonEmpty)

  }

  "Rewriting" should "work with constants" in {
    val helper = cancerOption.getHelper()
    val model = new DatabaseModel(cancerOption.getDBManager, true)
    val ecq = FOQueryParser.parse("∃(y). diagnosedWith(Bob,y) ∧ PancreasCancer(y)", cancerOption.getDBManager)
    val ecqExp = FOQueryParser.parse("∃(). PancreasCancerPatient(Bob)", cancerOption.getDBManager)
    val rewritings = EpistemicConjunctiveQueryRewriter.transform(ecq, helper)

    assert(rewritings(1).simplify == ecqExp.simplify )

  }
}
