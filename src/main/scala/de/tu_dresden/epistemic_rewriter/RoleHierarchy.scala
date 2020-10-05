package de.tu_dresden.epistemic_rewriter

import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.model.{OWLObjectProperty, OWLOntology}

import scala.collection.JavaConverters._
import scala.collection.immutable.{Map, Set}
import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

class RoleHierarchy {
  var roleHierarchy : Map[OWLObjectProperty, Set[OWLObjectProperty]] = Map()
  var roleHierarchyTransitive : Map[OWLObjectProperty, Set[OWLObjectProperty]] = Map()
  var ontology:OWLOntology = _

  def this(ontology: OWLOntology) = {
    this()
    this.ontology = ontology
    recomputeRoleHierarchy()
  }

  def recomputeRoleHierarchy(): Unit = {
    roleHierarchy = Map()
    // Get all roles in the signature of the ontology
    ontology.getObjectPropertiesInSignature(Imports.INCLUDED).forEach{ case role:OWLObjectProperty =>
      val subrolesBuilder: mutable.Set[OWLObjectProperty] = mutable.Set()
      for (o:OWLOntology <- ontology.getImportsClosure().asScala) {
        val newSubroles = o.getObjectSubPropertyAxiomsForSuperProperty(role).asScala.map( axiom => axiom.getSubProperty.asOWLObjectProperty())
        subrolesBuilder.++=(newSubroles)
      }
      val subroles:Set[OWLObjectProperty] = subrolesBuilder.toSet
      roleHierarchy = roleHierarchy.+((role, subroles))
    }
    // Compute the transitive closure
    roleHierarchyTransitive = transitiveClosure(roleHierarchy)
  }

  def transitiveClosure(inputHierarchy :Map[OWLObjectProperty, Set[OWLObjectProperty]]): Map[OWLObjectProperty, Set[OWLObjectProperty]]= {
    var roleH:Map[OWLObjectProperty, Set[OWLObjectProperty]] = inputHierarchy
    // Each role is a subrole of itself

    roleH = roleH.map{ case (k, v) => (k, v.+(k))}

    breakable {
      while (true) {
        val roleHNew = applyTransitive(roleH)
        if (roleH == roleHNew) {break}
        else { roleH = roleHNew }
      }
    }
    return roleH
  }

  private def applyTransitive(roleH :Map[OWLObjectProperty, Set[OWLObjectProperty]]) :Map[OWLObjectProperty, Set[OWLObjectProperty]] = {
    var roleHNew :Map[OWLObjectProperty, Set[OWLObjectProperty]] = Map()
    roleH.foreach { case (role, subroles) => {
      var newSubroles: Set[OWLObjectProperty] = Set()
      subroles.foreach( subrole => {
        newSubroles = newSubroles.++(roleH.getOrElse(subrole, Set.empty))
      })
      //println("ADDING TO " + role +" >= " + newSubroles.--(subroles))
      roleHNew = roleHNew.-(role).+((role, newSubroles))
    }}
    return roleHNew
  }


  def getSubRoles(role: OWLObjectProperty, strict: Boolean = true): Set[OWLObjectProperty] = {
    if (role.isOWLTopObjectProperty) {
      return roleHierarchy.keySet
    }
    val roleH = roleHierarchyTransitive
    val result = strict match {
      // Get only strict subroles by substracting all equivalent ones
      case true => roleH.getOrElse(role, Set.empty).--(getEquivalentRoles(role))
      case false => roleH.getOrElse(role, Set.empty)
    }
    return result
  }

  def getSuperRoles(role: OWLObjectProperty, strict: Boolean = true) : Set[OWLObjectProperty] = {
    val roleH = roleHierarchyTransitive
    val result = strict match {
      // Get only strict subroles by substracting all equivalent ones
      case true => roleH.filter{ case (r, subroles) => subroles.contains(role)}.keySet.--(getEquivalentRoles(role))
      case false => roleH.filter{ case (r, subroles) => subroles.contains(role)}.keySet
    }
    return result
  }

  def getEquivalentRoles(role: OWLObjectProperty) : Set[OWLObjectProperty] = {
    val m = roleHierarchyTransitive.get(role)
    m match {
      case Some(s) => roleHierarchyTransitive.filter { case (r,sr) => s.contains(r) && sr.contains(role) }.keySet
      case None => Set(role)
    }
  }

  def isSubRoleOfAny(role: OWLObjectProperty, roles: OWLObjectProperty*) : Boolean = {
    roles.exists(currole => getSubRoles(currole, false).contains(role))
  }

  def isSuperRoleOfAny(role: OWLObjectProperty, roles: OWLObjectProperty*) : Boolean = {
    roles.exists(currole => getSuperRoles(currole, false).contains(role))
  }

}