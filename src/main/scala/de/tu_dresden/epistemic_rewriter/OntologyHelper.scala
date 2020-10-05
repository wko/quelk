package de.tu_dresden.epistemic_rewriter

import java.io.{Closeable, File, FileInputStream, InputStream}
import java.util
import java.util.ArrayList

import com.typesafe.scalalogging.StrictLogging
import de.tu_dresden.OntologyUtils._
import de.tu_dresden.epistemic_rewriter.datatypes._
import org.phenoscape.scowl._
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.model.{OWLObjectPropertyAssertionAxiom, _}
import org.semanticweb.owlapi.reasoner._
import org.semanticweb.owlapi.search.EntitySearcher
import org.semanticweb.owlapi.util._
import uk.ac.manchester.cs.owl.owlapi.OWLAnnotationPropertyImpl

import scala.collection.JavaConverters._
import scala.collection.immutable.Set
import scala.collection.{SortedSet, mutable}
import scala.util.control.Breaks._

object OntologyHelper {
  val TIME_PREFIX = "time"
  val TIME_INSTANT = IRI.create(s"${TIME_PREFIX}#instant")
  val TIME_INSTANT_PROPERTY = new OWLAnnotationPropertyImpl(TIME_INSTANT)
  val DIAMOND = IRI.create(s"${TIME_PREFIX}#diamond")
  val DIAMOND_PROPERTY = new OWLAnnotationPropertyImpl(DIAMOND)

  def createOntologyHelper(stream: InputStream, mapper: Option[OWLOntologyIRIMapper], normalized: Boolean) = new OntologyHelper(stream, mapper, normalized)

  def createOntologyHelper(file: File, mapper: Option[OWLOntologyIRIMapper], normalized: Boolean): OntologyHelper = {
    mapper match {
      case Some(m) => {
        new OntologyHelper(new FileInputStream(file), Some(m), normalized)
      }
      case None => {
        val m = new AutoIRIMapper(file.getParentFile, true)
        new OntologyHelper(new FileInputStream(file), Some(m), normalized)
      }
    }
  }

}

class OntologyHelper(stream: InputStream, mapper: Option[OWLOntologyIRIMapper], normalized: Boolean = false) extends Closeable with TimeAware with StrictLogging {

  val manager: OWLOntologyManager = OWLManager.createOWLOntologyManager()
  if (mapper.isDefined) manager.addIRIMapper(mapper.get)
  val ontology = manager.loadOntologyFromOntologyDocument(stream)
  if (normalized) normalizeOntology(ontology, Imports.INCLUDED)

  var reasoner:OWLReasoner = loadReasoner()
  var roleHierarchy: RoleHierarchy = new RoleHierarchy(ontology)


  val rGen = scala.util.Random


  def computeJustification(query: FOQuery, answer: Map[Term, OWLNamedIndividual]): AnswerWithJustifications = {
    val m = for ( (t,i) <- answer) yield {
      lazy val r = for (atom <- query.getClassAtoms(t, Polarity.Pos)) yield {
        val cls = atom.cls
        val classes = computeJustification(cls, i)
        logger.debug(s"Computing Justification for $cls ; $i")
        classes.map(c => ClassAssertion(c, i))
      }
      val e: Set[OWLClassAssertionAxiom] = Set.empty
      (t,(i, e))
    }
    AnswerWithJustifications(m)
  }

  /**
    * Compute the set of classes that was asserted for a given individual and was responsible for it belonging to the given class.
    * @param cls
    * @param ind
    * @return - The set of classes that were asserted for the given individual and were responsible for it belonging to the given class.
    */
  def computeJustification(cls: OWLClass, ind: OWLNamedIndividual): Set[OWLClass] = {
        val subconcepts = querySubClasses(cls, false, true, false).asScala
        val assertions = ontology.getClassAssertionAxioms(ind).asScala.map(_.getClassExpression.asOWLClass())
        val classes = subconcepts.intersect(assertions)
        classes.toSet
    }




  private var canonicalModel: Option[MemoryModel] = None


  private def loadReasoner() : OWLReasoner = {

    val reasonerFactory = new ElkReasonerFactory()
    val progressMonitor = new ConsoleProgressMonitor()
    val config = new SimpleConfiguration()
    val reasoner = reasonerFactory.createReasoner(ontology, config)
    reasoner.precomputeInferences(InferenceType.CLASS_HIERARCHY)

    //val consistent = reasoner.isConsistent()
    return reasoner
  }

  def addInferredAxioms(file: File = null): OWLOntologyManager = {
    val gens = new ArrayList[InferredAxiomGenerator[_ <: OWLAxiom]]()
    gens.add(new InferredSubClassAxiomGenerator())
    gens.add(new InferredEquivalentClassAxiomGenerator())
    gens.add(new InferredClassAssertionAxiomGenerator())
    //gens.add(new InferredPropertyAssertionGenerator())


    // Put the inferred axioms into a fresh empty ontology.
    val outputOntologyManager = OWLManager.createOWLOntologyManager()
    val infOnt = outputOntologyManager.createOntology()
    val iog = new InferredOntologyGenerator(reasoner,
      gens)
    iog.fillOntology(outputOntologyManager.getOWLDataFactory(), infOnt)
    if (file != null) {
      saveOntology(outputOntologyManager, infOnt, file)
    }

    return outputOntologyManager
  }



  def saveOntology(oWLOntologyManager: OWLOntologyManager, oWLOntology: OWLOntology, file: File): Unit = {
    val format = oWLOntologyManager.getOntologyFormat(oWLOntology)
    val nformat = new FunctionalSyntaxDocumentFormat()

    if (format.isPrefixOWLOntologyFormat) nformat.copyPrefixesFrom(format.asPrefixOWLOntologyFormat)
    nformat.setDefaultPrefix("<http://www.semanticweb.org/forkel/ontologies/2018/6/cancer#>")
    oWLOntologyManager.saveOntology(oWLOntology, nformat, IRI.create(file.toURI()))

  }

  def queryInstances(query: OWLClassExpression) : NodeSet[OWLNamedIndividual] = synchronized {
    // Query for Axioms with an existential quantification on the right hand side
    val dataFactory = manager.getOWLDataFactory()
    val queryName = dataFactory.getOWLClass(IRI.create("temp001"))
    val definition = dataFactory.getOWLEquivalentClassesAxiom(queryName, query)
    manager.addAxiom(ontology, definition)
    reasoner.flush()
    val result = reasoner.getInstances(queryName, false)
    manager.removeAxiom(ontology, definition)
    return result
  }

  def queryInstances(role: OWLObjectProperty) :NodeSet[OWLNamedIndividual] = {
    val clsExpr = role some(getOWLTopClass())
    queryInstances(clsExpr)
  }

  def getOWLTopClass(): OWLClass = {
    OWLTopClass(IRI.create("http://www.w3.org/2002/07/owl#Thing"))
  }

  def addAssertion(oWLClass: OWLClass, oWLNamedIndividual: OWLNamedIndividual): OWLClassAssertionAxiom = {
    val dataFactory = manager.getOWLDataFactory()

    val ax = dataFactory.getOWLClassAssertionAxiom(oWLClass, oWLNamedIndividual)
    manager.addAxiom(ontology, ax)
    reasoner.flush()
    return ax
  }

  def removeAssertion(ax: OWLClassAssertionAxiom) = {
    manager.removeAxiom(ontology, ax)
    reasoner.flush()
  }

  def getTimepoints(): List[TimePoint] = {
    logger.debug("Finding timepoints in Ontology")
    val times: mutable.Set[TimePoint] = mutable.Set.empty
    ontology.getABoxAxiomsAnnotatedByProperty(OntologyHelper.TIME_INSTANT, Imports.INCLUDED).foreach { a =>
      times ++= a.getAnnotations(OntologyHelper.TIME_INSTANT_PROPERTY).asScala
        .map(t => TimePoint.parse(((t.getValue.asInstanceOf[OWLLiteral].getLiteral))))
    }
    times.toList
  }

  def getTemporalClassAssertions():Map[(OWLClass, OWLIndividual),SortedSet[TimePoint]] = {
    val ft = (a: OWLAxiom) => !a.getAnnotations(OntologyHelper.TIME_INSTANT_PROPERTY).isEmpty

    val m:mutable.Map[(OWLClass, OWLIndividual),SortedSet[TimePoint]] = mutable.Map[(OWLClass, OWLIndividual),SortedSet[TimePoint]]()
    for {ax <- ontology.getABoxAxioms(ft, Imports.EXCLUDED)
         if ax.isInstanceOf[OWLClassAssertionAxiom] } {
        val aAX = ax.asInstanceOf[OWLClassAssertionAxiom]
        val cls = aAX.getClassExpression.asOWLClass()
        val ind = aAX.getIndividual.asOWLNamedIndividual()
        val ts = ax.getAnnotations.asScala.filter(_.getProperty ==OntologyHelper.TIME_INSTANT_PROPERTY).map(t => TimePoint.parse((t.getValue.asInstanceOf[OWLLiteral].getLiteral)))
        m.get((cls, ind)) match {
          case None => m.update((cls, ind), mutable.SortedSet[TimePoint](ts.toSeq :_*))
          case Some(ts1) => m.update((cls, ind), ts1 ++ ts)
        }
    }
    m.toMap
  }

  def getObjectPropertyAssertions(tp: TimePoint): Set[OWLObjectPropertyAssertionAxiom] = {
    val ft = (a: OWLAxiom) => {
      a.getAnnotations(OntologyHelper.TIME_INSTANT_PROPERTY).asScala.
        exists(a => a.getProperty == OntologyHelper.TIME_INSTANT_PROPERTY && TimePoint.parse(a.getValue.asInstanceOf[OWLLiteral].getLiteral) == tp)
    }
    ontology.getABoxAxioms(ft, Imports.EXCLUDED).filter(_.isInstanceOf[OWLObjectPropertyAssertionAxiom]).map(_.asInstanceOf[OWLObjectPropertyAssertionAxiom]).toSet
  }

  def getSubsumptions(): Traversable[OWLSubClassOfAxiom] = {
    val r = for { ax <- ontology.getTBoxAxioms(Imports.EXCLUDED).asScala } yield {
      ax match {
        case ax@SubClassOf(_, _, _) => List(ax)
        case ax@EquivalentClasses(_, _) => ax.asOWLSubClassOfAxioms().asScala.toList
        case _ => List()
      }
    }
    r.flatten.toList
  }

  def getExistentialSubsumptions(): Map[(OWLObjectProperty, OWLClass), OWLClass] = {
    val r = for { SubClassOf(_, ObjectSomeValuesFrom(property, filler), superClass) <- getSubsumptions()
                  if (property.isInstanceOf[OWLObjectProperty] && filler.isInstanceOf[OWLClass] && superClass.isInstanceOf[OWLClass])} yield {
      ((property.asOWLObjectProperty(), filler.asOWLClass()), superClass.asOWLClass())
    }
    r.toMap

  }

  def getComplexSubsumptions(): Map[Set[OWLClass],OWLClass] = {
    val r = for { SubClassOf(_, ObjectUnionOf(subClasses), superClass) <- getSubsumptions()
          if (subClasses.forall(_.isInstanceOf[OWLClass] && superClass.isInstanceOf[OWLClass]))
    } yield (subClasses.map(_.asInstanceOf[OWLClass]), superClass.asOWLClass())
    r.toMap
  }

  def getTemporalObjectPropertyAssertions():Map[(OWLObjectProperty, OWLIndividual, OWLIndividual),SortedSet[TimePoint]] = {
    val ft = (a: OWLAxiom) => !a.getAnnotations(OntologyHelper.TIME_INSTANT_PROPERTY).isEmpty

    val m:mutable.Map[(OWLObjectProperty, OWLIndividual, OWLIndividual),SortedSet[TimePoint]] = mutable.Map[(OWLObjectProperty, OWLIndividual, OWLIndividual),SortedSet[TimePoint]]()
    for {ax <- ontology.getABoxAxioms(ft, Imports.EXCLUDED)
         if ax.isInstanceOf[OWLObjectPropertyAssertionAxiom] } {
      val aAX = ax.asInstanceOf[OWLObjectPropertyAssertionAxiom]
      val prop = aAX.getProperty.asOWLObjectProperty()
      val ind1 = aAX.getSubject.asOWLNamedIndividual()
      val ind2 = aAX.getObject.asOWLNamedIndividual()
      val ts = ax.getAnnotations.asScala.filter(_.getProperty ==OntologyHelper.TIME_INSTANT_PROPERTY).map(t => TimePoint.parse((t.getValue.asInstanceOf[OWLLiteral].getLiteral)))
      m.get((prop, ind1, ind2)) match {
        case None => m.update((prop, ind1, ind2), mutable.SortedSet[TimePoint](ts.toSeq :_*))
        case Some(ts1) => m.update((prop, ind1, ind2), ts1 ++ ts)
      }
    }
    m.toMap
  }

  def getLabel(cls: OWLClass): Option[String] = {
    val factory = manager.getOWLDataFactory
    for (o <- ontology.getImportsClosure.asScala) {
      for (a <- EntitySearcher.getAnnotations(cls, o, factory.getRDFSLabel).asScala) {
        val value = a.getValue
        if (value.isInstanceOf[OWLLiteral])
          return Some(value.asInstanceOf[OWLLiteral].getLiteral)
      }
    }
    return None
  }

  def getLabel(role: OWLObjectProperty): Option[String] = {
    val factory = manager.getOWLDataFactory
    for (o <- ontology.getImportsClosure.asScala) {
      for (a <- EntitySearcher.getAnnotations(role, o, factory.getRDFSLabel).asScala) {
        val value = a.getValue
        if (value.isInstanceOf[OWLLiteral])
          return Some(value.asInstanceOf[OWLLiteral].getLiteral)
      }
    }
    return None
  }

  def querySubClasses(query: OWLClassExpression, strict: Boolean = true, omitTop: Boolean = true, direct: Boolean = true) : util.Set[OWLClass] = synchronized {
    /*if (query.isOWLThing) {
      // TODO:
      return ontology.getClassesInSignature(Imports.INCLUDED)
    }*/

    val tmpAxIRI = IRI.create("temp001")
    // Query for Axioms with an existential quantification on the right hand side
    val dataFactory = manager.getOWLDataFactory()
    val queryName = dataFactory.getOWLClass(tmpAxIRI)
    val definition = dataFactory.getOWLEquivalentClassesAxiom(queryName, query)
    manager.addAxiom(ontology, definition)
    reasoner.flush()

    var result : util.Set[OWLClass] = strict match {
      case true  => {
        // Return only strict subclasses
        reasoner.getSubClasses(queryName, direct).getFlattened()
      }
      case false => {
        // Return subclasses and equivalent classes
        val resultSub = reasoner.getSubClasses(queryName, direct)
        val resultEqu = reasoner.getEquivalentClasses(queryName)
        val resultSet = resultEqu.getEntities()
        resultSet.addAll(resultSub.getFlattened())
        resultSet
      }
    }

    result = result.asScala.filterNot(x => x.getIRI.toString == tmpAxIRI.toString).asJava


    manager.removeAxiom(ontology, definition)
    omitTop match {
      case true => return result.asScala.filterNot(x => x.isOWLNothing).asJava
      case false => return result
    }

  }


  /**
    *
    * @param cls Partially ordered set
    * @return
    */
  def sortClassesBySubsumption(cls: Traversable[OWLClass]): List[OWLClass] = {


    def comparator(a:OWLClass,b:OWLClass):Boolean = !isStrictSubClassOf(b,a)
    //val l = List(Class("http://snomed.info/id/61474001"), Class("http://snomed.info/id/262893009"))
    //(<http://snomed.info/id/61474001>,<http://snomed.info/id/262893009>), (<http://snomed.info/id/262893009>,<http://snomed.info/id/18796000>)
    //println(l.sortWith(comparator(_,_)))
    // search for two elements that are lt each other wrt. some comparator

    // Return a topological iterator?
    try {
      val r = cls.toList.sortWith(comparator)
      return r
    }
    catch {
      case e:Exception => {
        println(cls)
        val r = for {c1 <- cls; c2 <- cls; if comparator(c1,c2) && comparator(c2,c1)} yield(c1,c2)
        if (r.nonEmpty) {
          println("Found two classes that are strict subclasses of each other. This is not possible.")
          println(r)
          //throw new Exception("Found two classes that are strict subclasses of each other. This is not possible.")
        }
        throw e
      }
    }
  }

  def sortRolesBySubsumption(roles: Traversable[OWLObjectProperty]): List[OWLObjectProperty] = {
    roles.toList.sortWith((c,d) => !roleHierarchy.isSubRoleOfAny(d, c))
  }




  def isStrictSubClassOf(cl: OWLClass, cls: OWLClass) : Boolean = querySubClasses(cls, true).contains(cl)
  //def isSubClassOf(cl: OWLClass, cls: OWLClass, strict: Boolean = false) : Boolean = querySubClasses(cls, strict).contains(cl)

  def isSubClassOf(sub: OWLClassExpression, sup: OWLClassExpression, strict: Boolean = false) : Boolean = synchronized {
    val tmp1AxIRI = Class(s"temp001")
    val tmp2AxIRI = Class(s"temp002")

    val definition1 = EquivalentClasses(tmp1AxIRI, sub)
    val definition2 = EquivalentClasses(tmp2AxIRI, sup)
    manager.addAxiom(ontology, definition1)
    manager.addAxiom(ontology, definition2)


    reasoner.flush()

    lazy val subs = reasoner.getSubClasses(tmp2AxIRI, false)
    lazy val equiv = reasoner.getEquivalentClasses(tmp2AxIRI)

    val result =
      if (strict) subs.containsEntity(tmp1AxIRI)
      else subs.containsEntity(tmp1AxIRI) || equiv.contains(tmp1AxIRI)
    manager.removeAxiom(ontology, definition1)
    manager.removeAxiom(ontology, definition2)



    result

  }

  def isSubClassOfAny(cl: OWLClass, cls: OWLClass*): Boolean = {
    cls.exists(querySubClasses(_, false).contains(cl))
  }

  def isSuperClassOf(cl: OWLClass, cls: OWLClass) : Boolean = querySuperClasses(cls, false).contains(cl)

  def isSuperClassOfAny(cl: OWLClass, cls: OWLClass*): Boolean = {
    cls.exists(querySuperClasses(_, false).contains(cl))
  }

  def querySuperClasses(query: OWLClassExpression, strict: Boolean = true) : util.Set[OWLClass] = synchronized {
    val tmpAxIRI = IRI.create("temp001")
    // Query for Axioms with an existential quantification on the right hand side
    val dataFactory = manager.getOWLDataFactory()
    val queryName = dataFactory.getOWLClass(tmpAxIRI)
    val definition = dataFactory.getOWLEquivalentClassesAxiom(queryName, query)
    manager.addAxiom(ontology, definition)
    reasoner.flush()

    val result = strict match {
      case true  => {
        // Return only strict subclasses
        return reasoner.getSuperClasses(queryName, false).getFlattened()
      }
      case false => {
        // Return subclasses and equivalent classes
        val resultSub = reasoner.getSuperClasses(queryName, false)
        val resultEqu = reasoner.getEquivalentClasses(queryName)
        val resultSet = resultEqu.getEntities()
        resultSet.addAll(resultSub.getFlattened())
        return resultSet
      }
    }


    manager.removeAxiom(ontology, definition)
    return result
  }

  /**
    * Queries for complex superclasses that are
    * @param cls
    * @return
    */
  def queryComplexSuperClasses(cls: OWLClassExpression, direct: Boolean): Set[OWLClassExpression] = {
    val start = direct match {
      case true => reasoner.getEquivalentClasses(cls).getEntities.asScala
      case false => reasoner.getEquivalentClasses(cls).getEntities.asScala.++(reasoner.getSuperClasses(cls,false).getFlattened.asScala)
    }
    val result = start.flatMap { c =>
      EntitySearcher.getSuperClasses(c, ontology.getImportsClosure).asScala.++(
        EntitySearcher.getEquivalentClasses(c, ontology.getImportsClosure).asScala
      )
    }.toSet.+(cls)

    logger.debug(s"ComplexSuperClass for $cls")
    logger.debug(result.toString)
    result
  }

  def querySubRoles(query: OWLObjectProperty, strict: Boolean = true) : Set[OWLObjectProperty] = {
    roleHierarchy.getSubRoles(query, strict)
  }

  def querySuperRoles(query: OWLObjectProperty, strict: Boolean = true) : Set[OWLObjectProperty] = {
    roleHierarchy.getSuperRoles(query, strict)
  }



  def queryEquivalentRoles(query: OWLObjectProperty) : Set[OWLObjectProperty] = {
    roleHierarchy.getEquivalentRoles(query)
  }



  def getCanonicalModel: MemoryModel = synchronized {
    if (canonicalModel.isEmpty) {
      constructCanonicalModel()
    }
    return canonicalModel.get
  }
  private def constructCanonicalModel():MemoryModel = {

    // Class assertions
    var classAssertions: mutable.Map[OWLClass, Set[OWLNamedIndividual]] = mutable.Map()
    ontology.getClassesInSignature(Imports.INCLUDED).forEach { case cls:OWLClass => classAssertions.+=((cls, reasoner.getInstances(cls, false).getFlattened.asScala.toSet)) }


    // Construct Role Assertions
    var roleAssertions: mutable.Map[OWLObjectProperty, Set[(OWLNamedIndividual, OWLNamedIndividual)]] = mutable.Map()
    ontology.getABoxAxioms(Imports.EXCLUDED).forEach {
      ax:OWLAxiom => {
        if (ax.isInstanceOf[OWLObjectPropertyAssertionAxiom]) {
          val ax1 : OWLObjectPropertyAssertionAxiom = ax.asInstanceOf[OWLObjectPropertyAssertionAxiom]
          val role = ax1.getProperty.asOWLObjectProperty()
          val ind1 = ax1.getSubject.asOWLNamedIndividual()
          val ind2 = ax1.getObject.asOWLNamedIndividual()
          for (superRole <- roleHierarchy.getSuperRoles(role, false)) {
            val prevAssertions = roleAssertions.getOrElse(superRole, Set())
            roleAssertions.+=((superRole, prevAssertions.+((ind1, ind2))))
          }
        }
    }}

    canonicalModel = Some(MemoryModel(classAssertions.toMap, roleAssertions.toMap, this))
    return canonicalModel.get
  }



  override def close(): Unit = {
    reasoner.dispose()
  }

  /**
    * Normalizes an ontology to make it suitable for rewriting
    * @param o
    */
  def normalizeOntology(o: OWLOntology, importsIncluded: Imports) = {

    var changes:mutable.Set[OWLOntologyChange] = mutable.Set()

    // Loop through all axioms
    for (ax : OWLAxiom <- o.getTBoxAxioms(Imports.INCLUDED).asScala) {
      // Try to get the complex class expression from the axiom
      val cls: Set[_ <: OWLClassExpression] = ax match {
        case SubClassOf(_, superCl, subCl) => Set(superCl, subCl)
        case EquivalentClasses(_, cls) => cls
        case _ => break
      }

      def normalize(expr: OWLClassExpression): Set[OWLOntologyChange] = expr match {
        case ObjectIntersectionOf(operands) => operands.map(normalize).flatten
        case subExp@ObjectSomeValuesFrom(property, filler) => {
          val newC = Class(IRI.create(property.asOWLObjectProperty().getIRI.toString + filler.toString))
          logger.debug(newC.getIRI.toString)
          val newAx:AddAxiom = new AddAxiom(o,EquivalentClasses(subExp, newC))
          normalize(filler).+(newAx)
        }
        case _ => Set()
      }

      val newAxioms = cls.map(normalize).flatten
      println("Found axioms " + newAxioms.size)
      changes.++=(newAxioms)

    }
    val c = changes.size
    println(s"""Adding $c axioms to ontology""")
    manager.applyChanges(changes.toList.asJava)

  }

}

