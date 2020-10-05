package de.tu_dresden.epistemic_rewriter

import de.tu_dresden.epistemic_rewriter.datatypes.{Answer, PrettyPrintable, Query}
import org.semanticweb.owlapi.model.{OWLClass, OWLNamedIndividual, OWLObjectProperty}

import scala.collection.immutable.Set

case class MemoryModel(classAssertions: Map[OWLClass, Set[OWLNamedIndividual]], roleAssertions: Map[OWLObjectProperty, Set[(OWLNamedIndividual, OWLNamedIndividual)]], helper: OntologyHelper) extends PrettyPrintable with Modellike {
  override def pShow: String = {
    var output = ""
    output += "-------MODEL----------\n"
    output += "----------------------\n"
    output += "------CONCEPTS--------\n"
    output += classAssertions.map { case (cls, inds) => (cls.getIRI.getShortForm, inds.map(x => x.getIRI.getShortForm))}.mkString("\n")
    output += "\n"
    output += "------ROLES--------\n"
    output += roleAssertions.map { case (cls, inds) => (cls.getIRI.getShortForm, inds.map{ case (x1,x2) => (x1.getIRI.getShortForm, x2.getIRI.getShortForm)})}.mkString("\n")
    output += "\n----------------------\n"
    return output
  }

  override def lookup(cls: OWLClass): Traversable[OWLNamedIndividual] = {
    classAssertions.getOrElse(cls, Set())
  }

  override def lookup(role: OWLObjectProperty): Traversable[(OWLNamedIndividual, OWLNamedIndividual)] = {
    roleAssertions.get(role) match {
      case Some(r) => r
      case None => {
        throw new Error("Role not found: " + role.getIRI.toString)
      }
    }
  }


  def getIndividuals: Traversable[OWLNamedIndividual] = {
    val ind1 = classAssertions.values.flatten.toSet
    val ind2 = roleAssertions.values.flatten.unzip
    return ind1.++(ind2._1).++(ind2._2)
  }

  override def getClasses = classAssertions.keySet

  override def getRoles: Traversable[OWLObjectProperty] = roleAssertions.keySet

  def getClasses(oWLNamedIndividual: OWLNamedIndividual): Iterable[OWLClass] = {
    classAssertions.filter{ case (cls, inds) => inds.contains(oWLNamedIndividual)}.keys
  }

  override def isSatisfied(cls: OWLClass, term: OWLNamedIndividual): Boolean= {
    return lookup(cls).toSet.contains(term)
  }

  override def isSatisfied(role: OWLObjectProperty, term1: OWLNamedIndividual, term2: OWLNamedIndividual): Boolean = {
    return lookup(role).toSet.contains((term1, term2))
  }

  override def getDBManager(): DatabaseManager = throw new NotImplementedError()
  override def getAnswers(f: Query): Set[Answer] = throw new NotImplementedError()

  override def getSignatureCounts: (Int, Int, Int) = {
    val i: Int = getIndividuals.toSet.size
    val c: Int = classAssertions.keySet.size
    val r: Int = roleAssertions.keySet.size
    (i,c,r)
  }
}


