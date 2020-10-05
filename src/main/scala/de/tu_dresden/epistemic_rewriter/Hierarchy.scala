package de.tu_dresden.epistemic_rewriter

import de.tu_dresden.epistemic_rewriter.datatypes.OWLTopClass
import org.semanticweb.owlapi.model._

import scala.collection.immutable.Set

trait Hierarchy {
  def getOWLTopClass(): OWLClass = {
    OWLTopClass(IRI.create("http://www.w3.org/2002/07/owl#Thing"))
  }

  def getLabels(cls: OWLClass): Seq[String]
  def getLabels(role: OWLObjectProperty) : Seq[String]
  def getLabels(iri: OWLNamedIndividual) : Seq[String]

  def querySubClasses(query: OWLClass, strict: Boolean = true, omitTop: Boolean = true, direct: Boolean = true) : Set[OWLClass]
  def isSubClassOf(cl: OWLClass, cls: OWLClass) : Boolean = querySubClasses(cls, false).contains(cl)
  def isSubClassOfAny(cl: OWLClass, cls: OWLClass*): Boolean = {
    cls.exists(querySubClasses(_, false).contains(cl))
  }

  def querySuperClasses(query: OWLClass, strict: Boolean = true) : Set[OWLClass]

  def isSuperClassOf(cl: OWLClass, cls: OWLClass) : Boolean = querySuperClasses(cls, false).contains(cl)

  def isSuperClassOfAny(cl: OWLClass, cls: OWLClass*): Boolean = {
    cls.exists(querySuperClasses(_, false).contains(cl))
  }
  def queryEquivalentClasses(cls: OWLClass): Set[OWLClass]

  def querySubRoles(query: OWLObjectProperty, strict: Boolean = true) : Set[OWLObjectProperty]
  def querySuperRoles(query: OWLObjectProperty, strict: Boolean = true) : Set[OWLObjectProperty]
  def queryEquivalentRoles(query: OWLObjectProperty) : Set[OWLObjectProperty]
}


