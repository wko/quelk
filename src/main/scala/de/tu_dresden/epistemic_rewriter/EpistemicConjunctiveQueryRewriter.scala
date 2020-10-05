package de.tu_dresden.epistemic_rewriter

import de.tu_dresden.epistemic_rewriter.datatypes._
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model._
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.control.Breaks._

object EpistemicConjunctiveQueryRewriter {
  val rewriter = new EpistemicConjunctiveQueryRewriter()
  def transform(query: QueryPlusFO, ontologyHelper: OntologyHelper): List[QueryPlusFO] = {
    rewriter.transform(query, ontologyHelper)
  }
}

class EpistemicConjunctiveQueryRewriter extends IRewriter[QueryPlusFO, List[QueryPlusFO]] {


  val logger: Logger = LoggerFactory.getLogger(this.getClass)

  override def transform(query: QueryPlusFO, ontologyHelper: OntologyHelper): List[QueryPlusFO] = {

    transformR(mutable.Queue(query), ontologyHelper).toList
  }

  case class MinimalResult(property: OWLObjectProperty, filler: OWLClassExpression, subClasses: List[OWLClass]) {
    val objectSomeValuesFrom = property some filler
  }
  case class Constraint(posClasses: Set[OWLClass], negClasses: Set[OWLClass], posRoles: Set[OWLObjectProperty], negRoles: Set[OWLObjectProperty])

  /**
    * Find all M subclassOf (s some N) that satisfy all conditions in (S2)
    * @param constraints
    * @param helper
    * @return
    */
  def selectMinimalMs(constraints: Constraint, helper: OntologyHelper): Traversable[MinimalResult] = {
    val factory = helper.manager.getOWLDataFactory()

    // STEP 1: Find Minimal (s some N)

    // STEP 1.1: Find Minimal N candidates
    val classExpr : OWLClassExpression = constraints.posClasses.nonEmpty match {
      case true => factory.getOWLObjectIntersectionOf(constraints.posClasses.asJava)
      case false => helper.getOWLTopClass()
    }
    val Ns: Set[OWLClass] = helper.querySubClasses(classExpr, strict = false, direct = false).asScala
      .filterNot(helper.isSubClassOfAny(_, constraints.negClasses.toSeq: _*)).toSet

    // STEP 1.2: Find Minimal s candidates
    val roleExpr = constraints.posRoles.+(OWLTopObjectProperty())
    val Ss: Set[OWLObjectProperty] = roleExpr.map(helper.querySubRoles(_, false))
      .reduce(_.intersect(_)) // select only the roles that are subroles of all roles in rolesP
      .filterNot(helper.roleHierarchy.isSubRoleOfAny(_, constraints.negRoles.toSeq: _*)) // filter all roles that are subrole to any role in roleN


    // STEP 1.3: Find Minimal (s some N)
    var candidates: List[OWLObjectSomeValuesFrom] = (for {
      s <- helper.sortRolesBySubsumption(Ss)
      n <- helper.sortClassesBySubsumption(Ns)
      //n <- helper.getMinimalClasses(Ns)
    } yield (s some n))
    logger.debug(s"Found ${candidates.size} minimal right sides")
    logger.debug(candidates.toString)

    //var minimals: ListBuffer[OWLObjectSomeValuesFrom] = ListBuffer.empty
    val results: mutable.Set[MinimalResult] = mutable.Set.empty
    while (candidates.nonEmpty) {
      val sN = candidates.head
      // filter elements that are not subsumed by h

      val Ms = helper.querySubClasses(sN, false, true, true).asScala
      if (Ms.nonEmpty) {
        candidates = candidates.tail.filterNot(helper.isSubClassOf(sN,_))
        results += MinimalResult(sN.getProperty.asOWLObjectProperty(), sN.getFiller, Ms.toList)
      } else {
        candidates = candidates.tail
      }

    }

    // STEP 2: For each minimal (s some N) find Maximal Ms


    results
  }

  private def transformR(queue: mutable.Queue[QueryPlusFO], helper: OntologyHelper, results: Set[QueryPlusFO] = Set.empty): Set[QueryPlusFO] = {
    if (queue.isEmpty) return results
    val query = queue.dequeue()
    if (results.contains(query)) {
      logger.debug("Query already rewritten")
      return transformR(queue, helper, results)
    }
    // Do the rewriting
    logger.debug("Rewriting query " + query.pShow)

    val ecq = query.ecq
    val foFilters = query.foFilters

    var newResults = results.+(query)

    val ontology = helper.ontology
    // For each Leaf Term \hat{x}
    for (xhat: Variable <- ecq.getNonAnswerLeafVariables()) {
      // TODO: What if answer variables are the predecessors?
      val predTerms: Set[Term] = ecq.getPredecessors(xhat)

      // If there are more than two predeccessor constants, we can not rewrite
      val predConst: Set[Constant] = predTerms.filter(_.isInstanceOf[Constant]).map(_.asInstanceOf[Constant])
      if (predConst.size > 1) break
      val yhat: Term = if (predConst.size == 1) predConst.head else if (predTerms.isEmpty) {
        // Can only happen with unrooted CQs
        // In this case we simply add a fresh variable
        xhat
      } else {
        predTerms.head
      }


      // Collect the constraints
      val classesP = ecq.getClassAtoms(xhat, Polarity.Pos).map(_.cls)
      val classesN = ecq.getClassAtoms(xhat, Polarity.Neg).map(_.cls)
      var rolesP = ecq.getIncomingRoleAtoms(xhat, Polarity.Pos).map(_.role)
      var rolesN = ecq.getIncomingRoleAtoms(xhat, Polarity.Neg).map(_.role)

      val constraint: Constraint = Constraint(classesP, classesN, rolesP, rolesN)

      val factory = helper.manager.getOWLDataFactory()
      val minimalCandidates = selectMinimalMs(constraint, helper)
      logger.debug(s"Found ${minimalCandidates.size} possible minimal right sides")
      for (r <- minimalCandidates)
        breakable {
          logger.debug(s"There are ${r.subClasses.size} possible left sides for ${r.objectSomeValuesFrom}")
          if (r.subClasses.isEmpty) break

          // STEP (S3): find negative concepts
          val NPrimes: Set[OWLClassExpression] = constraint.negClasses.map {
            r.filler and (_)
          }
          val SPrimes: Set[OWLObjectProperty] = Set(r.property) // rolesN.foreach{ ontologyHelper.roleHierarchy. } // TODO: Not correct yet if we have negated (sub-)roles
          val possibleSPrimeNPrimes = for {
            nPrime <- NPrimes
            sPrime <- SPrimes
          } yield sPrime some (nPrime)


          for (m <- r.subClasses) {
            logger.debug(m.toString)
            val MPrimes: Set[OWLClass] = possibleSPrimeNPrimes.map(left =>
              helper.querySubClasses(left and (m), false).asScala.toSet
            ).flatten

            // Filter cases where the positive implies the negative part
            if (!helper.isSubClassOfAny(m, MPrimes.toList: _*)) {
              // Rewrite the query
              val newCQ = rewrite(ecq, xhat, yhat, predTerms, m, MPrimes)

              val filterContents = helper.queryComplexSuperClasses(m, false)
                .filter(_.isInstanceOf[OWLObjectSomeValuesFrom])
                .filter(helper.isSubClassOf(_, r.objectSomeValuesFrom))

              logger.debug(s"${filterContents.size} Filters found such that ${m} <= X <= ${r.objectSomeValuesFrom}")
              logger.debug(s"${filterContents.toString()}")

              val filters = for (c <- filterContents) yield {
                val filterContent = c.asInstanceOf[OWLObjectSomeValuesFrom]

                // Rewrite the filter
                rewriteFilter(query, filterContent, xhat, yhat)
                // Assemble result

                //result = result.++(transformR(newQueryPlusFO, helper, maxDepth - 1))
              }
              val newQueryPlusFO = QueryPlusFO(newCQ, List(FODisj(filters.map(FOConj(_)).toList))).simplify
              logger.debug(s"Found Rewriting to ${newQueryPlusFO.pShow}")
              queue += newQueryPlusFO
            }
          }
        }
      }
    transformR(queue, helper, newResults)
  }


  def rewriteFilter(query: QueryPlusFO, sn: OWLObjectSomeValuesFrom, xhat: Variable, yhat: Term) : List[FOQuery] = {
    val ecq = query.ecq
    val classesP = ecq.getClassAtoms(xhat, Polarity.Pos).map(_.cls)
    val classesN = ecq.getClassAtoms(xhat, Polarity.Neg).map(_.cls)
    logger.debug(s"Rewriting Filter with sN=${sn.toString}")
    var posFOFilter:List[FOQuery] = getClasses(sn.getFiller).map(ClassAtom(_, xhat)).toList.+:(
      RoleAtom(sn.getProperty.asOWLObjectProperty(), yhat, xhat)).map(PosLiteral(_))
    //var posFOFilter:List[FOQuery] = classesP.map(ClassAtom(_, xhat)).toList.++(
    //  ecq.getIncomingRoleAtoms(xhat, Polarity.Pos)).map(PosLiteral(_))
    var negFOFilter:List[FOQuery] = classesN.map(c => ClassAtom(c, xhat)).toList.++(
      ecq.getIncomingRoleAtoms(xhat, Polarity.Neg)).map(NegLiteral(_))
    if (posFOFilter.isEmpty) posFOFilter = List(FOTrue())
    if (negFOFilter.isEmpty) negFOFilter = List(FOTrue())

    def filtersVar(v: Variable, f: FOQuery): Boolean = {
      f.getAnswerVars().contains(v)
    }

    
    val (filters, others) = query.foFilters.partition(filtersVar(xhat, _))
    //val filters1 = filters //.map(removeVar(v,_))

    val poConj = FOConj(posFOFilter)
    val restConj = FOConj(posFOFilter.++(negFOFilter).++:(filters).reverse)
    val newFOFilter:FOQuery = FOImpl(FOEx(List(xhat), poConj), FOEx(List(xhat),restConj))
    val simplifiedFOFilter = newFOFilter.simplify
    val newFilters = others.:+(simplifiedFOFilter).reverse
    return newFilters
  }
  // Perform a single rewriting step
  def rewrite(query: FOQuery, oldTerm: Variable, newTerm: Term, predTerms: Set[Term], subclasses: OWLClassExpression, negSubclasses: Set[OWLClass]): FOQuery = {
    var cq = query
    // drop every atom containing term
    cq = cq.dropWhere(_.getTerms().contains(oldTerm))

    // replace all other occurrences of z in predTerms by y
    cq = cq.rename(predTerms.toList, newTerm)
    // add class assertions for each subclass in subclasses expression
    var ecq = EpistemicConjunctiveQuery(cq).++(getClasses(subclasses).map(x => PosLiteral(ClassAtom(x, newTerm))))
    // add negative class assertions
    ecq = ecq.++(negSubclasses.map(x => NegLiteral(ClassAtom(x, newTerm))))
    // Remove variable
    ecq = ecq.removeQuantifiedVariable(oldTerm)
    // return query
    return ecq.foq
  }

  def getClasses(given: OWLClassExpression): Set[OWLClass]= given match {
    case ObjectIntersectionOf(operands) => operands.flatMap(getClasses)
    case _ => Set(given.asOWLClass())
  }
}

