package de.tu_dresden.epistemic_rewriter


import java.time.Duration

import com.typesafe.scalalogging.StrictLogging
import de.tu_dresden.epistemic_rewriter.datatypes.{Decreasing, Diamond, Increasing, Rigid}
import org.phenoscape.scowl._
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports

import scala.collection.JavaConverters._
import scala.collection.mutable

object TELHReasonerFactory {
  def createReasoner(ontology: OWLOntology): TELHReasoner = {
    TELHReasoner(ontology)
  }
}


/*case class NoDiamond() extends Diamond {
  override def identifier: String = "---"

  override def complete(timestamps: SortedSet[Timepoint])(implicit availableTimepoints: SortedSet[Timepoint]): SortedSet[Timepoint] = ???
}*/

case class TemporalClass(operator: Diamond, cls: OWLClass) {
}
case class TemporalObjectProperty(operator: Diamond, property: OWLObjectProperty)

trait TemporalAxiom

case class TemporalSubClassOfAxiom(operator: Diamond, axiom: OWLSubClassOfAxiom) extends TemporalAxiom {
  def subClass():OWLClassExpression = {
    axiom.getSubClass
  }
  def superClass():OWLClassExpression = {
    axiom.getSuperClass
  }

  override def toString: String = s"<${operator} ${axiom.getSubClass} ⊑ ${axiom.getSuperClass}>"

}


case class TemporalSubObjectPropertyOfAxiom(operator: Diamond, axiom: OWLSubObjectPropertyOfAxiom) extends TemporalAxiom {
  def subProperty():OWLObjectPropertyExpression = {
    axiom.getSubProperty
  }
  def superProperty():OWLObjectPropertyExpression = {
    axiom.getSuperProperty
  }

  override def toString: String = s"<${operator} ${axiom.getSubProperty} ⊑ ${axiom.getSuperProperty}>"
}



/**
  * A class for computing the saturation of a temporal ELH TBox.
  * @param ontology
  */
case class TELHReasoner(ontology: OWLOntology, resolution:Duration = Diamond.MIN_DIFF) extends StrictLogging {






  def run():(Seq[TemporalSubObjectPropertyOfAxiom], Seq[TemporalSubClassOfAxiom]) = {
    val rAxiomsSat = saturate(getTemporalSubObjectPropertyOfAxioms)
    val axiomsSat = saturate(getTemporalSubClassOfAxioms)(rAxiomsSat)
    (rAxiomsSat, axiomsSat)
  }

  def getTemporalSubObjectPropertyOfAxioms = getRoleAxiomsWithAnnotations(ontology).map{ case (a,b) => convertAxiom(a,b)}
  def getTemporalSubClassOfAxioms = getAxiomsWithAnnotations(ontology).map{ case (a,b) => convertAxiom(a,b)}


  /**
    * Get GCIs with their (temporal) annotations
    * @param ontology
    * @return
    */
  def getAxiomsWithAnnotations(ontology: OWLOntology): Seq[(OWLSubClassOfAxiom, Seq[OWLAnnotation])] = {
    val result = mutable.Buffer[(OWLSubClassOfAxiom, Seq[OWLAnnotation])]()
    ontology.getTBoxAxioms(Imports.EXCLUDED).asScala.toSeq.foreach( a => {
      val annotations = a.getAnnotations(OntologyHelper.DIAMOND_PROPERTY).asScala.toSeq
      a match {
        case SubClassOf(_, _, _) => {
          result += ((a.asInstanceOf[OWLSubClassOfAxiom],annotations))
        }
        case EquivalentClasses(_) => {
          a.asInstanceOf[OWLEquivalentClassesAxiom].asOWLSubClassOfAxioms().asScala.foreach{ ax =>
            result += ((ax,annotations))
          }
        }
        case _ =>
      }

    })
    result
  }

  /**
    * Get RIAs with their temporal annotations
    * @param ontology
    * @return
    */
  def getRoleAxiomsWithAnnotations(ontology: OWLOntology): Seq[(OWLSubObjectPropertyOfAxiom, Seq[OWLAnnotation])] = {
    val result = mutable.Buffer[(OWLSubObjectPropertyOfAxiom, Seq[OWLAnnotation])]()
    ontology.getRBoxAxioms(Imports.EXCLUDED).asScala.toSeq.foreach( a => {
      val annotations = a.getAnnotations(OntologyHelper.DIAMOND_PROPERTY).asScala.toSeq
      a match {
        case SubObjectPropertyOf(_, _, _) => {
          result += ((a.asInstanceOf[OWLSubObjectPropertyOfAxiom],annotations))
        }
        case EquivalentObjectProperties(_) => {
          a.asInstanceOf[OWLEquivalentObjectPropertiesAxiom].asSubObjectPropertyOfAxioms().asScala.foreach{ ax =>
            result += ((ax,annotations))
          }
        }
        case _ =>
      }

    })
    result
  }

  /**
    * Read in all TBox axioms and convert them to Temporal Class Axioms
    */
  def convertAxiom(axiom: OWLSubClassOfAxiom, annotations: Seq[OWLAnnotation]): TemporalSubClassOfAxiom = {
    val ds:Seq[Diamond] = annotations.map( a => {
      Diamond.fromString(a.getValue.asInstanceOf[OWLLiteral].getLiteral)
    })
    val strongestDiamond = ds.fold(Diamond.MIN_CONVEX)(Diamond.sup(_, _))
    TemporalSubClassOfAxiom(strongestDiamond, axiom.getAxiomWithoutAnnotations)
  }

  def convertAxiom(axiom: OWLSubObjectPropertyOfAxiom, annotations: Seq[OWLAnnotation]): TemporalSubObjectPropertyOfAxiom = {
    val ds:Seq[Diamond] = annotations.map( a => {
      Diamond.fromString(a.getValue.asInstanceOf[OWLLiteral].getLiteral)
    })
    val strongestDiamond = ds.fold(Diamond.MIN_CONVEX)(Diamond.sup(_, _))
    TemporalSubObjectPropertyOfAxiom(strongestDiamond, axiom.getAxiomWithoutAnnotations)
  }


  /**
    * Saturation of the RBox by rules T3 + T5
    * @param axioms
    * @return
    */
  def saturate(axioms:Seq[TemporalSubObjectPropertyOfAxiom]): Seq[TemporalSubObjectPropertyOfAxiom] = {
    implicit val todo: mutable.Queue[TemporalSubObjectPropertyOfAxiom] = mutable.Queue[TemporalSubObjectPropertyOfAxiom](axioms :_*)
    implicit val processed: mutable.Map[OWLSubObjectPropertyOfAxiom, Diamond] = mutable.Map[OWLSubObjectPropertyOfAxiom, Diamond](T3().map(x => (x.axiom, x.operator)).toSeq :_* )




    def updateNecessary(ax: TemporalSubObjectPropertyOfAxiom): Boolean = processed.get(ax.axiom) match {
      case None => true
      case Some(d) => Diamond.sup(ax.operator,d) == d && d != ax.operator
    }
    logger.debug("SATURATING ROLES")

    while ( todo.nonEmpty ) {
      val ax1 = todo.dequeue()
      logger.debug(s"Selected ${ax1}")
      if (updateNecessary(ax1)) {
        logger.debug(s"Update is necessary...")
        // Add axiom to the processed set
        processed.update(ax1.axiom, ax1.operator)
        // T5: saturate role axioms
        todo.enqueue(T5(ax1).toSeq :_*)
      }
    }
    processed.map{ case (ax, d) => TemporalSubObjectPropertyOfAxiom(d, ax) }.toSeq
  }

  /**
    * The TBox is saturated by keeping a list of processed Axioms and
    * @param axioms
    * @param rAxioms
    * @return
    */
  def saturate(axioms: Seq[TemporalSubClassOfAxiom])(rAxioms: Seq[TemporalSubObjectPropertyOfAxiom]): Seq[TemporalSubClassOfAxiom] = {
    implicit val todo: mutable.Queue[TemporalSubClassOfAxiom] = mutable.Queue[TemporalSubClassOfAxiom](axioms :_*)
    implicit val todoR: mutable.Queue[TemporalSubObjectPropertyOfAxiom] = mutable.Queue[TemporalSubObjectPropertyOfAxiom](rAxioms :_*)
    implicit val processed: mutable.Map[OWLSubClassOfAxiom, Diamond] = mutable.Map[OWLSubClassOfAxiom, Diamond]()
    implicit val processedR: mutable.Map[OWLSubObjectPropertyOfAxiom, Diamond] = mutable.Map[OWLSubObjectPropertyOfAxiom, Diamond](rAxioms.map(x => (x.axiom, x.operator)) :_* )

    // Rules T1, T2, T7
    todo.enqueue(T1().toSeq :_*)
    todo.enqueue(T2().toSeq :_*)
    todo.enqueue(T7().toSeq :_*)

    logger.debug("SATURATING CONCEPTS")

    def updateNecessary(ax: TemporalSubClassOfAxiom): Option[TemporalSubClassOfAxiom] = processed.get(ax.axiom) match {
      case None => {
        //logger.debug(s"Axiom not processed so far.")
        Some(ax)
      } // Axiom has not been processed so far
      case Some(d) => {
        val sup = Diamond.sup(ax.operator,d)
        if (sup != d) {
          //logger.debug(s"${sup} is more general than ${d}.")
          Some(TemporalSubClassOfAxiom(sup, ax.axiom))
        } else None
      }
    }



    while ( todo.nonEmpty ) {
      val ax = todo.dequeue()
      //
      updateNecessary(ax) match {
        case Some(ax1) => {
        logger.debug(s"Selected ${ax}")
        //logger.debug(s"Update is necessary...")
        // Add axiom to the processed set
        processed.update(ax1.axiom, ax1.operator)




        // Rule T4:
        todo.enqueue(T4(ax1).toSeq :_*)

        // Rule: T6:
        todo.enqueue(T6(ax1).toSeq :_*)

        // Rule: T8('):
        todo.enqueue(T8(ax1).toSeq :_*)

      }
      case _ =>

    }}

    processed.map{ case (ax, d) => TemporalSubClassOfAxiom(d, ax) }.toSeq
  }

  def T1(): Iterable[TemporalSubClassOfAxiom] = {
    ontology.getClassesInSignature().asScala.map(x => TemporalSubClassOfAxiom(Diamond.MIN_CONVEX, x SubClassOf(x)))
  }

  def T2(): Iterable[TemporalSubClassOfAxiom] = {
    ontology.getClassesInSignature().asScala.map(x => TemporalSubClassOfAxiom(Rigid(), x SubClassOf(OWLThing)))
  }

  def T3(): Iterable[TemporalSubObjectPropertyOfAxiom] = {
    ontology.getObjectPropertiesInSignature().asScala.map(x => TemporalSubObjectPropertyOfAxiom(Diamond.MIN_CONVEX, x SubPropertyOf(x)))
  }

  def T4(ax: TemporalSubClassOfAxiom)(implicit processed:mutable.Map[OWLSubClassOfAxiom, Diamond]): Iterable[TemporalSubClassOfAxiom] = {
    val r1 = processed.filterKeys{ax1 => ax1.getSubClass == ax.superClass}.map { case (ax2, d2) =>
      val ax3 = TemporalSubClassOfAxiom(Diamond.sup(ax.operator,d2), SubClassOf(ax.subClass, ax2.getSuperClass()))
      if (ax != ax3) {
        logger.debug(s"Applied Rule T4 on ${ax} with ${TemporalSubClassOfAxiom(d2, ax2)}")
        logger.debug(s"Produced: ${ax3.toString}")
      }
      ax3
    }

    val r2 = processed.filterKeys(_.getSuperClass == ax.subClass).map{ case (ax2, d2) =>
      val ax3 = TemporalSubClassOfAxiom(Diamond.sup(ax.operator,d2), SubClassOf(ax2.getSubClass, ax.superClass()))
      if (ax != ax3) {
        logger.debug(s"Applied Rule T4 on ${ax2} with ${TemporalSubClassOfAxiom(d2, ax.axiom)}")
        logger.debug(s"Produced: ${ax3.toString}")
      }
      ax3
    }
    r1 ++ r2
  }

  def T5(ax: TemporalSubObjectPropertyOfAxiom)(implicit processed:mutable.Map[OWLSubObjectPropertyOfAxiom, Diamond]): Iterable[TemporalSubObjectPropertyOfAxiom] = {
    val r1 = processed.filterKeys(_.getSubProperty() == ax.superProperty()).map { case (ax2, d2) =>
      val ax3 = TemporalSubObjectPropertyOfAxiom(Diamond.sup(ax.operator,d2), SubObjectPropertyOf(ax.subProperty(), ax2.getSuperProperty()))
      if (ax != ax3) {
        logger.debug(s"Applied Rule T5 on ${ax} with ${TemporalSubObjectPropertyOfAxiom(d2, ax2)}")
        logger.debug(s"Produced: ${ax3.toString}")
      }
      ax3
    }

    val r2 = processed.filterKeys(_.getSuperProperty == ax.subProperty).map{ case (ax2, d2) =>
      val ax3 = TemporalSubObjectPropertyOfAxiom(Diamond.sup(ax.operator,d2), SubObjectPropertyOf(ax2.getSubProperty(), ax.superProperty()))
      if (ax != ax3) {
        logger.debug(s"Applied Rule T5 on ${ax2} with ${TemporalSubObjectPropertyOfAxiom(d2, ax.axiom)}")
        logger.debug(s"Produced: ${ax3.toString}")
      }
      ax3
    }
    r1 ++ r2
  }

  def T6(ax1: TemporalSubClassOfAxiom)(implicit processed:mutable.Map[OWLSubClassOfAxiom, Diamond]): Iterable[TemporalSubClassOfAxiom] = {

    val r1:Iterable[TemporalSubClassOfAxiom] =
      for ( (ax2, d) <- processed.filterKeys(x => x.getSubClass == ax1.subClass && x.getSuperClass != ax1.superClass());
            (ax1Conj2, Diamond.MIN_CONVEX) <- processed.filterKeys( x => equiv(x.getSubClass(), ObjectIntersectionOf(Set(ax2.getSuperClass, ax1.superClass()))))) yield {
        val axN = TemporalSubClassOfAxiom(Diamond.inf(ax1.operator,d), SubClassOf(ax1.subClass, ax1Conj2.getSuperClass()))
        if (ax1 != axN) {
          logger.debug("Applied Rule T6")
          logger.debug(axN.toString)
        }
        axN
      }


    val r2:Iterable[TemporalSubClassOfAxiom] = ax1.axiom match {
      case SubClassOf(_,a1,a2) =>
        for ((ai,di) <- processed.filterKeys(_.getSuperClass == a1);
             (aj,dj) <- processed.filterKeys(_.getSuperClass == a2) if ai.getSubClass == aj.getSubClass) yield {
          val axN = TemporalSubClassOfAxiom(Diamond.inf(di,dj), SubClassOf(ai.getSubClass,ax1.axiom.getSuperClass))
          if (ax1 != axN) {
            logger.debug("Applied Rule T6")
            logger.debug(axN.toString)
          }
          axN
        }
      case _ => List()
    }

    r1 ++ r2
  }

  def T7(): Iterable[TemporalSubClassOfAxiom] = {
    ontology.getObjectPropertiesInSignature().asScala.map(x => TemporalSubClassOfAxiom(Diamond.MIN_CONVEX, (x some OWLNothing) SubClassOf(OWLNothing)))
  }

  def T8(ax: TemporalSubClassOfAxiom)(implicit processed:mutable.Map[OWLSubClassOfAxiom, Diamond], processedR: mutable.Map[OWLSubObjectPropertyOfAxiom, Diamond]): Iterable[TemporalSubClassOfAxiom] =
    for ((ax1@SubClassOf(_, a, ObjectSomeValuesFrom(r, a1)), d1) <- processed;
         (SubObjectPropertyOf(_, `r`, s), d2) <- processedR;
         (ax2@SubClassOf(_, `a1`, b1), d3) <- processed;
         (ax3@SubClassOf(_, ObjectSomeValuesFrom(`s`, `b1`), b), Diamond.MIN_CONVEX) <- processed;
         if (ax.axiom == ax1) || (ax.axiom == ax2) || (ax.axiom == ax3)
    ) yield
    {
      val di = Diamond.inf(d2,d3)
      val d = di match {
        case Increasing() | Decreasing() | Rigid() => Diamond.sup(di,d1) // T8'
        case _ => d1 // T8
      }

      val axN = TemporalSubClassOfAxiom(d, SubClassOf(a,b))
      if (ax != axN) {
        logger.debug("Applied Rule T8")
        logger.debug(axN.toString)
      }
      axN
    }



  def equiv(cl1: OWLClassExpression, cl2: OWLClassExpression): Boolean = (cl1, cl2) match {
    case (SubClassOf(_, a1, b1), SubClassOf(_, a2, b2)) => a1 == a2 && b1 == b2 || a1 == b2 && a2 == b1
    case _ => cl1 == cl2
  }





}
