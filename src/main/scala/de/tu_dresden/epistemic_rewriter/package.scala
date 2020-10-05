package de.tu_dresden

import com.typesafe.scalalogging.StrictLogging
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.model._
import uk.ac.manchester.cs.owl.owlapi.OWLAnnotationPropertyImpl

import scala.collection.JavaConverters._
import scala.collection.mutable

package object OntologyUtils {
  implicit class OntologyTools(o: OWLOntology) extends StrictLogging{
    def getAnnotationAssertionAxioms(includeImportsClosure: Imports): List[OWLAnnotationAssertionAxiom] = {
      o.getABoxAxioms(includeImportsClosure).asScala.toList.filter(_.isInstanceOf[OWLAnnotationAssertionAxiom]).map(_.asInstanceOf[OWLAnnotationAssertionAxiom])
    }

    def getABoxAxioms(f: OWLAxiom => Boolean, includeImportsClosure: Imports): mutable.Set[OWLAxiom] = {
      o.getABoxAxioms(includeImportsClosure).asScala.filter(f)
    }

    def getABoxAxiomsAnnotatedByProperty(propertyType: IRI, includeImportsClosure: Imports): mutable.Set[OWLAxiom] = {
      val f = (a:OWLAxiom) => { !a.getAnnotations(new OWLAnnotationPropertyImpl(propertyType)).isEmpty }
      o.getABoxAxioms(f, includeImportsClosure)
    }

    def getABoxAxiomsNotAnnotatedByProperty(propertyType: IRI, includeImportsClosure: Imports): List[OWLAxiom] = {
      o.getABoxAxioms(includeImportsClosure).asScala.toList.filter{ a => a.getAnnotations(new OWLAnnotationPropertyImpl(propertyType)).isEmpty}
    }
  }
}
