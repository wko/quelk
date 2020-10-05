package de.tu_dresden.epistemic_rewriter
import de.tu_dresden.epistemic_rewriter.datatypes.{Answer, AnswersPack, Query, QueryPlusFO}
import org.semanticweb.owlapi.model.{OWLClass, OWLNamedIndividual, OWLObjectProperty}

import scala.collection.immutable.Set

trait Modellike {

  def pShow: String

  //def getTimePoints(): Traversable[LocalDateTime]
  //def setTimePoint(t: Option[LocalDateTime]): Modellike

  def lookup(cls: OWLClass): Traversable[OWLNamedIndividual]
  def lookup(role: OWLObjectProperty): Traversable[(OWLNamedIndividual, OWLNamedIndividual)]

  def lookupOutgoing(role: OWLObjectProperty): Traversable[OWLNamedIndividual] = {
    return lookup(role).map { case (t1, t2) => t2 }
  }

  def lookupOutgoing(role: OWLObjectProperty, term: OWLNamedIndividual): Traversable[OWLNamedIndividual] = {
    return lookup(role).filter { case (t1, t2) => t1 == term }.map { case (t1, t2) => t2 }
  }

  def lookupIncoming(role: OWLObjectProperty): Traversable[OWLNamedIndividual] = {
    return lookup(role).map { case (t1, t2) => t1 }
  }

  def lookupIncoming(role: OWLObjectProperty, term: OWLNamedIndividual): Traversable[OWLNamedIndividual] = {
    return lookup(role).filter { case (t1, t2) => t2 == term }.map { case (t1, t2) => t1 }
  }

  def isSatisfied(cls: OWLClass, term: OWLNamedIndividual): Boolean

  def isSatisfied(role: OWLObjectProperty, term1: OWLNamedIndividual, term2: OWLNamedIndividual): Boolean

  def getSignatureCounts: (Int, Int, Int)
  def getIndividuals: Traversable[OWLNamedIndividual]
  def getClasses: Traversable[OWLClass]
  def getClasses(oWLNamedIndividual: OWLNamedIndividual): Iterable[OWLClass]

  def getRoles: Traversable[OWLObjectProperty]

  def getRoleAssertions: Traversable[(OWLObjectProperty,Traversable[(OWLNamedIndividual, OWLNamedIndividual)])] = getRoles.map(r => (r,lookup(r)))




  /*def getAnswers(f: QueryPlusFO) : Set[Answer] = {
    getAnswers(f.toFirstOrderQuery())
  }*/
  def getAnswers(f: Query) : Set[Answer]

  def getAnswersMap(cqs:List[QueryPlusFO]): Map[QueryPlusFO, Set[Answer]] = {
    return cqs.map(cq => (cq, getAnswers(cq))).toMap
  }

  def getAnswersPack(q: EpistemicConjunctiveQuery, cqs:List[QueryPlusFO]) : AnswersPack = {
    AnswersPack(q, getAnswersMap(cqs))
  }

  def getDBManager(): DatabaseManager

  def canBeVisualized: Boolean = getIndividuals.size < 30


  def getAnswers(qs:List[QueryPlusFO]) : Set[Answer] = {
    return qs.map(q => getAnswers(q)).flatten.toSet
  }
}
