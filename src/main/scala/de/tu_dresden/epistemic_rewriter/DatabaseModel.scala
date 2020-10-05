package de.tu_dresden.epistemic_rewriter





import com.typesafe.scalalogging.StrictLogging
import de.tu_dresden.epistemic_rewriter.datatypes.{Query => _, _}
import de.tu_dresden.epistemic_rewriter.datatypes.Query
import org.phenoscape.scowl._
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.{InferenceType, SimpleConfiguration}
import org.semanticweb.owlapi.util.{InferredAxiomGenerator, InferredOntologyGenerator, InferredSubClassAxiomGenerator}
import slick.jdbc.GetResult
import slick.jdbc.PostgresProfile.api._

import scala.collection.JavaConverters._
import scala.collection.immutable.Set
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object DatabaseModel {
  def saveToDatabase(helper: OntologyHelper, manager: DatabaseManager, withInference: Boolean): DatabaseModel = {
    val dModel = DatabaseModel(manager, withInference)
    dModel.saveToDatabase(helper)
    return dModel
  }
}

trait IRIStorage {
  implicit val owlClassToString = MappedColumnType.base[OWLClass, String](
    o => o.getIRI.toString,
    s => Class(s)
  )
  implicit val owlObjectPropertyToString = MappedColumnType.base[OWLObjectProperty, String](
    o => o.getIRI.toString,
    s => ObjectProperty(s)
  )
  implicit val owlNamedIndividualToString = MappedColumnType.base[OWLNamedIndividual, String](
    o => o.getIRI.toString,
    s => NamedIndividual(s)
  )
}

/**
  * A database model for a (temporal) ontology.
  * It adds all non-temporal facts to the database with a null entry as timestamp.
  * All temporal information are added to the database with a timestamp.
  * In between any two given timepoints, an artificial timepoint is introduced
  * acting as a representative for the whole intermediate interval.
  * When querying, a specific timepoint can be set at which the query should be evaluated.
  * Only information at this timepoint are then used to answer the query.
  * When no timepoint is given, the query is evaluated only over the atemporal facts
  * (that don't have any timestamp associated with them).
  * @param manager
  * @param withInference
  * @param timePoint If given queries only facts asserted at the given timePoint
  */
case class DatabaseModel(manager: DatabaseManager, withInference: Boolean) extends Modellike with IRIStorage with Hierarchy with StrictLogging {


  def getDBManager() = manager

  class Concepts(tag: Tag) extends Table[OWLClass](tag, "concepts") {
    def iri = column[OWLClass]("iri", O.PrimaryKey)

    def * = (iri)
  }

  class ConceptAnnotations(tag: Tag) extends Table[(OWLClass, String, Int)](tag, "concept_annotations") {
    def conceptiri = column[OWLClass]("concept_id")

    def label = column[String]("label")

    def ltype = column[Int]("type")

    def * = (conceptiri, label, ltype)

    def pk = index("pk_r", conceptiri)

    def concept = foreignKey("concept_fk", conceptiri, concepts)(_.iri)
  }

  class Roles(tag: Tag) extends Table[(OWLObjectProperty)](tag, "roles") {
    def iri = column[OWLObjectProperty]("iri", O.PrimaryKey)

    def * = (iri)
  }

  class RoleAnnotations(tag: Tag) extends Table[(OWLObjectProperty, String, Int)](tag, "role_annotations") {
    def roleiri = column[OWLObjectProperty]("role_id")

    def label = column[String]("label")

    def ltype = column[Int]("type")

    def * = (roleiri, label, ltype)

    def role = foreignKey("concept_fk", roleiri, roles)(_.iri)
  }

  class Individuals(tag: Tag) extends Table[(OWLNamedIndividual)](tag, "individuals") {
    def iri = column[OWLNamedIndividual]("iri", O.PrimaryKey)

    def * = (iri)
  }

  class IndividualAnnotations(tag: Tag) extends Table[(OWLNamedIndividual, String, Int)](tag, "individual_annotations") {
    def indiri = column[OWLNamedIndividual]("ind_id")

    def label = column[String]("label")

    def ltype = column[Int]("type")

    def * = (indiri, label, ltype)

    def role = foreignKey("ind_fk", indiri, individuals)(_.iri)
  }

  /*class Timestamps(tag: Tag) extends Table[(Timestamp, Boolean)](tag, "timestamps") {
    def timestamp = column[Timestamp]("timestamp", O.PrimaryKey)
    def real = column[Boolean]("real")
    def * = (timestamp, real)
  }
  */

  class ConceptAssertions(tag: Tag) extends Table[(OWLClass, OWLNamedIndividual)](tag, "concept_assertions") {
    def conceptiri = column[OWLClass]("concept_id")

    def indid = column[OWLNamedIndividual]("ind_id")

    def * = (conceptiri, indid)

    def pk = index("pk_c", (conceptiri, indid), unique = true)

    def idx = index("idx_c", (conceptiri), unique = false)

    def idx1 = index("idx_indid_name", (indid), unique = false)

    // A reified foreign key relation that can be navigated to create a join
    def concept = foreignKey("concept_fk", conceptiri, concepts)(_.iri)

    def individual = foreignKey("individual_fk", indid, individuals)(_.iri)
  }

  class RoleAssertions(tag: Tag) extends Table[(OWLObjectProperty, OWLNamedIndividual, OWLNamedIndividual)](tag, "role_assertions") {
    def roleiri = column[OWLObjectProperty]("role_id")

    def ind1id = column[OWLNamedIndividual]("ind_1_id")

    def ind2id = column[OWLNamedIndividual]("ind_2_id")

    def * = (roleiri, ind1id, ind2id)

    def pk = index("pk_ra", (roleiri, ind1id, ind2id), unique = true)

    def idx = index("idx_ra", (roleiri), unique = false)

    def idx1 = index("idx_indids_name", (ind1id, ind2id), unique = false)

    def role = foreignKey("role_fk", roleiri, roles)(_.iri)

    def individual1 = foreignKey("individual_fk_1", ind1id, individuals)(_.iri)

    def individual2 = foreignKey("individual_fk_2", ind2id, individuals)(_.iri)
  }

  class SubRoles(tag: Tag) extends Table[(OWLObjectProperty, OWLObjectProperty)](tag, "sub_roles") {
    def roleiri = column[OWLObjectProperty]("role_id")

    def subroleiri = column[OWLObjectProperty]("sub_role_id")

    def * = (roleiri, subroleiri)

    def pk = primaryKey("pk_sr", (roleiri, subroleiri))

    def idx1 = index("idx_r1", (roleiri), unique = false)

    def idx2 = index("idx_r2", (subroleiri), unique = false)

    def role = foreignKey("role_fk", roleiri, roles)(_.iri)

    def subrole = foreignKey("subrole_fk", subroleiri, roles)(_.iri)
  }

  class SubConcepts(tag: Tag) extends Table[(OWLClass, OWLClass)](tag, "sub_concepts") {
    def conceptiri = column[OWLClass]("concept_id")

    def subconceptiri = column[OWLClass]("sub_concept_id")

    def * = (conceptiri, subconceptiri)

    def pk = primaryKey("pk_sc", (conceptiri, subconceptiri))

    def idx1 = index("idx_c1", (conceptiri), unique = false)

    def idx2 = index("idx_c2", (conceptiri), unique = false)

    def concept = foreignKey("concept_fk", conceptiri, concepts)(_.iri)

    def subconcept = foreignKey("subconcept_fk", subconceptiri, concepts)(_.iri)
  }

  val concepts = TableQuery[Concepts]
  val conceptAnnotations = TableQuery[ConceptAnnotations]
  val roles = TableQuery[Roles]
  val roleAnnotations = TableQuery[RoleAnnotations]
  val individuals = TableQuery[Individuals]
  val individualAnnotations = TableQuery[IndividualAnnotations]
  val conceptAssertions = TableQuery[ConceptAssertions]
  val roleAssertions = TableQuery[RoleAssertions]
  val subConcepts = TableQuery[SubConcepts]
  val subRoles = TableQuery[SubRoles]

  val allRelations = List(
    individuals.schema,
    individualAnnotations.schema,
    roles.schema,
    roleAnnotations.schema,
    concepts.schema,
    conceptAnnotations.schema,
    conceptAssertions.schema,
    roleAssertions.schema,
    subConcepts.schema,
    subRoles.schema)


  def setup(resetSchema: Boolean = false) = {
    resetSchema match {
      case false => {
        logger.debug("Truncating the tables if existing")
        try {
          val truncateStmt =
            """
            TRUNCATE TABLE timestamps, individuals, concepts, roles CASCADE;
          """.stripMargin
          manager.raw(truncateStmt)
        }
        catch {
          case e: org.postgresql.util.PSQLException => {
            //logger.debug(s"PSQL Error ${e.getSQLState}")
            e.getSQLState match {
              case "42P01" => {
                logger.info("Table don't exist. Creating them..")
                manager.withDatabase(_.run(allRelations.reduce(_ ++ _).create))
              }
              case _ => throw e
            }
          }
        }
      }
      case true => {
        logger.debug("Dropping the schema")
        val resetStmt =
          """DROP SCHEMA public CASCADE;
                      CREATE SCHEMA public;
                    GRANT ALL ON SCHEMA public TO postgres;
                      GRANT ALL ON SCHEMA public TO public;"""
        manager.raw(resetStmt)
        logger.debug("Creating the tables")
        manager.withDatabase(_.run(allRelations.reduce(_ ++ _).create))
      }
    }


    //println(allRelations.reverse.reduce(_ ++ _).drop.statements.mkString("\n"))
    //manager.withDatabase( _.run(allRelations.reverse.reduce(_ ++ _).drop))

  }


  /**
    * Append the signature of a given ontology to the database
    *
    * @param ontology
    */
  def appendSignatureToDatabase(ontology: OWLOntology): Unit = {
    logger.debug("Saving Individuals Signature")
    val inds = ontology.getIndividualsInSignature(Imports.INCLUDED).asScala
    manager.run(DBIO.seq(individuals ++= inds))

    logger.debug("Saving Concepts Signature")
    val cls = ontology.getClassesInSignature(Imports.INCLUDED).asScala.toSeq
    manager.run(DBIO.seq(concepts ++= cls))

    logger.debug("Saving Roles Signature")
    val rls = ontology.getObjectPropertiesInSignature(Imports.INCLUDED).asScala
    manager.run(DBIO.seq(roles ++= rls))
  }

  def writeToDBInChunks[A, B <: Table[_]](elems: Iterable[A], table: TableQuery[B], chunk_size: Int = 100)(f: A => Option[_ <: B#TableElementType]) = {
    logger.debug(s"Writing to DB in chunks of $chunk_size")
    var stmts = mutable.MutableList.empty
    var i = 1
    elems.foreach { el =>
      f(el) match {
        case Some(v) => {
          i += 1
          if (i % chunk_size == 0) {
            logger.debug(s"$i elements processed, saving to DB")
            manager.run(DBIO.seq(table ++= stmts))
            stmts.clear()
          }
        }
        case None =>
      }
    }
    manager.run(DBIO.seq(table ++= stmts))
    stmts.clear()
    logger.debug(s"Saved $i elements to DB.")
  }

  def appendAnnotationsToDatabase(ontology: OWLOntology) =  {}
  /*{
    val queue = List(
      (for {i <- individuals} yield (i.iri), individualAnnotations),
      (for {i <- concepts} yield (i.iri), conceptAnnotations),
      (for {i <- roles} yield (i.iri), roleAnnotations))
    var stmts: mutable.MutableList[(String, String, Int)] = mutable.MutableList.empty
    var i = 1
    for (a <- ontology.getAnnotationAssertionAxioms(Imports.INCLUDED);
         if a.getProperty.isLabel && a.getSubject.isInstanceOf[IRI] && a.getValue.isInstanceOf[OWLLiteral]) {

      val value = a.getAnnotation.getValue
      val iri = a.getSubject.asInstanceOf[IRI]
      stmts.+:=(iri.toString, value.asInstanceOf[OWLLiteral].getLiteral, 0)

      i += 1
      if (i % 1000 == 0) {
        logger.debug(s"$i elements processed, saving to DB")
        manager.run(DBIO.seq(conceptAnnotations ++= stmts).asTry)
        manager.run(DBIO.seq(roleAnnotations ++= stmts).asTry)
        manager.run(DBIO.seq(individualAnnotations ++= stmts).asTry)
        stmts.clear()
      }
    }

    logger.debug(s"$i elements processed, saving to DB")
    manager.run(DBIO.seq(conceptAnnotations ++= stmts).asTry)
    manager.run(DBIO.seq(roleAnnotations ++= stmts).asTry)
    manager.run(DBIO.seq(individualAnnotations ++= stmts).asTry)
    stmts.clear()
  }*/

  def appendHierarchyToDatabase(helper: OntologyHelper): Unit = {
    logger.debug("Creating concepts hierarchy")
    val cls: Seq[OWLClass] = manager.withDatabase(db => db.run((for {c <- concepts} yield (c.iri)).result))
    val s = cls.size
    var i = 0
    for (c <- cls) {

      i += 1; if (i % 10 == 0) logger.debug(s"$i / $s")
      val stmts = for (sc <- helper.querySubClasses(c, false, true, true).asScala;
                       if sc != c) yield {
        subConcepts += (c, sc)
      }
      manager.run(DBIO.seq(stmts.toSeq :_*))
    }
    logger.debug(s"Added $s concepts to the hierarchy.")

    logger.debug("Creating roles hierarchy")
    val rolesList: Seq[(OWLObjectProperty)] = manager.withDatabase(db => db.run((for {c <- roles} yield (c.iri)).result))
    for (c <- rolesList) {
      val stmts = for (sc <- helper.querySubRoles(c, false);
                       if c != sc) yield {
        subRoles += (c, sc)
      }
      manager.run(DBIO.seq(stmts.toSeq :_*))
    }
  }



  def appendAssertionsToDatabase(helper: OntologyHelper): Unit = {

    //val base_iri = helper.ontology.getOntologyID.getOntologyIRI.get()
    val m = OWLManager.createOWLOntologyManager
    val ontology = m.createOntology(helper.ontology.getOntologyID)

    m.setIRIMappers(helper.manager.getIRIMappers.asScala.toSet.asJava)
    helper.ontology.getImportsDeclarations.forEach(i => {
      m.makeLoadImportRequest(i)
      m.applyChange(new AddImport(ontology, i))
    })

    m.addAxioms(ontology, helper.ontology.getTBoxAxioms(Imports.EXCLUDED))
    // Empty the ABox
    m.removeAxioms(ontology, ontology.getABoxAxioms(Imports.EXCLUDED))

    // STEP 1: Add all assertions that have no timestamp
    m.addAxioms(ontology, helper.ontology.getABoxAxioms(Imports.EXCLUDED))

    save2db(ontology, withInference)


    logger.debug("Done..")
  }

  def save2db(ontology: OWLOntology, withInference: Boolean) = withInference match {
    case true => {
      logger.debug("Saving to DB all inferred Axioms.")
      save2dbWithInference(ontology)
    }
    case false => {
      logger.debug("Saving to DB only stated Axioms.")
      save2dbWithoutInference(ontology)
    }
  }

  def save2dbWithoutInference(ontology: OWLOntology) = {
    // Load Reasoner and do the inference
    //val reasoner = new ElkReasonerFactory().createReasoner(ontology, new SimpleConfiguration())
    //reasoner.precomputeInferences(InferenceType.CLASS_ASSERTIONS, InferenceType.DATA_PROPERTY_ASSERTIONS)

    val cls = ontology.getClassesInSignature(Imports.INCLUDED)
    var i = 1
    var stmts: mutable.MutableList[(OWLClass, OWLNamedIndividual)] = mutable.MutableList.empty
    cls.forEach { case cls: OWLClass =>
      i += 1;

      stmts.++=(ontology.getClassAssertionAxioms(cls).asScala.toSeq.map(i => (cls, i.getIndividual.asOWLNamedIndividual())))
      if (i % 1000 == 0) {
        logger.debug(s"$i concept assertions processed. Saving to DB.")
        //manager.insert("concept_assertions", stmts.map(_.toString))
        manager.run(DBIO.seq(conceptAssertions ++= stmts))
        stmts.clear
      }
    }
    logger.debug(s"$i concept assertions processed. Saving to DB.")

    manager.run(DBIO.seq(conceptAssertions ++= stmts))
    stmts.clear

    logger.debug("Adding role assertions")
    // Construct Role Assertions

    //val roleHierarchy = new RoleHierarchy(ontology)
    i = 1
    var stmtsR: mutable.MutableList[(OWLObjectProperty, OWLNamedIndividual, OWLNamedIndividual)] = mutable.MutableList.empty
    for {ax <- ontology.getABoxAxioms(Imports.INCLUDED).asScala;
         if ax.isInstanceOf[OWLObjectPropertyAssertionAxiom]} {
      val ax1: OWLObjectPropertyAssertionAxiom = ax.asInstanceOf[OWLObjectPropertyAssertionAxiom]
      val role = ax1.getProperty.asOWLObjectProperty()
      val ind1 = ax1.getSubject.asOWLNamedIndividual()
      val ind2 = ax1.getObject.asOWLNamedIndividual()
      val x = ax1.getAnnotations() //(new OWLAnnotationPropertyImpl(IRI.create(":timestamp"))).asScala
      for (t <- x.asScala) {
        logger.debug(t.toString)
      }
      stmtsR.++=(for (superRole <- Seq(role)) yield {
        (superRole, ind1, ind2)
      })
      if (i % 1000 == 0) {
        logger.debug(s"$i role assertions processed. Saving to DB.")

        manager.run(DBIO.seq(roleAssertions ++= stmtsR))
        stmtsR.clear
      }
    }
    logger.debug(s"$i role assertions processed. Saving to DB.")

    manager.run(DBIO.seq(roleAssertions ++= stmtsR))
    stmtsR.clear
  }

  def save2dbWithInference(ontology: OWLOntology) = {
    // Load Reasoner and do the inference
    val reasoner = new ElkReasonerFactory().createReasoner(ontology, new SimpleConfiguration())
    reasoner.precomputeInferences(InferenceType.CLASS_ASSERTIONS, InferenceType.DATA_PROPERTY_ASSERTIONS)

    val cls = ontology.getClassesInSignature(Imports.INCLUDED)
    var i = 1
    var stmts: mutable.MutableList[(OWLClass, OWLNamedIndividual)] = mutable.MutableList.empty
    cls.forEach { case cls: OWLClass =>
      i += 1;

      stmts.++=(reasoner.getInstances(cls, false).getFlattened.asScala.toSeq.map(i => (cls, i)))
      if (i % 1000 == 0) {
        logger.debug(s"$i concept assertions processed. Saving to DB.")
        //manager.insert("concept_assertions", stmts.map(_.toString))
        manager.run(DBIO.seq(conceptAssertions ++= stmts))
        stmts.clear
      }
    }
    logger.debug(s"$i concept assertions processed. Saving to DB.")
    //manager.insert("concept_assertions", stmts.map(_.toString))
    manager.run(DBIO.seq(conceptAssertions ++= stmts))
    stmts.clear

    logger.debug("Adding role assertions")
    // Construct Role Assertions

    val roleHierarchy = new RoleHierarchy(ontology)
    i = 1
    var stmtsR: mutable.MutableList[(OWLObjectProperty, OWLNamedIndividual, OWLNamedIndividual)] = mutable.MutableList.empty
    for {ax <- ontology.getABoxAxioms(Imports.INCLUDED).asScala;
         if ax.isInstanceOf[OWLObjectPropertyAssertionAxiom]} {
      val ax1: OWLObjectPropertyAssertionAxiom = ax.asInstanceOf[OWLObjectPropertyAssertionAxiom]
      val role = ax1.getProperty.asOWLObjectProperty()
      val ind1 = ax1.getSubject.asOWLNamedIndividual()
      val ind2 = ax1.getObject.asOWLNamedIndividual()
      val x = ax1.getAnnotations() //(new OWLAnnotationPropertyImpl(IRI.create(":timestamp"))).asScala
      for (t <- x.asScala) {
        logger.debug(t.toString)
      }
      stmtsR.++=(for (superRole <- roleHierarchy.getSuperRoles(role, false).toSeq) yield {
        (superRole, ind1, ind2)
      })
      if (i % 1000 == 0) {
        logger.debug(s"$i role assertions processed. Saving to DB.")
        //manager.insert("role_assertions", stmtsR.map(_.toString))
        manager.run(DBIO.seq(roleAssertions ++= stmtsR))
        stmtsR.clear
      }
    }
    logger.debug(s"$i role assertions processed. Saving to DB.")
    //manager.insert("role_assertions", stmtsR.map(_.toString))
    manager.run(DBIO.seq(roleAssertions ++= stmtsR))
    stmtsR.clear
    reasoner.dispose()
  }

  def setupToDatabase(helper: OntologyHelper) = {
    logger.info("Setting up database schema")
    setup()
    logger.info("Saving signature to database")
    appendSignatureToDatabase(helper.ontology)

  }

  def saveAnnotations(helper: OntologyHelper) = {
    logger.info("Saving annotations to database")
    manager.run(individualAnnotations.schema.truncate)
    manager.run(conceptAnnotations.schema.truncate)
    manager.run(roleAnnotations.schema.truncate)
    appendAnnotationsToDatabase(helper.ontology)
  }

  def saveHierarchy(helper: OntologyHelper) = {
    logger.info("Saving hierarchy to database")
    manager.run(subConcepts.schema.truncate)
    manager.run(subRoles.schema.truncate)
    appendHierarchyToDatabaseFast(helper)

  }



  def appendHierarchyToDatabaseFast(helper: OntologyHelper) = {

    // To generate an inferred ontology we use implementations of
    // inferred axiom generators

    // Put the inferred axioms into a fresh empty ontology.
    val gens: collection.mutable.Buffer[InferredAxiomGenerator[_ <: OWLAxiom]] = collection.mutable.Buffer(new InferredSubClassAxiomGenerator)
    val iog = new InferredOntologyGenerator(helper.reasoner, gens.asJava)

    val outputOntologyManager = OWLManager.createOWLOntologyManager
    val infOnt = outputOntologyManager.createOntology
    iog.fillOntology(outputOntologyManager.getOWLDataFactory, infOnt)


    logger.debug("Creating concepts hierarchy")
    val axioms = infOnt.getAxioms(AxiomType.SUBCLASS_OF).asScala

    def f(ax: OWLSubClassOfAxiom): Option[(OWLClass, OWLClass)] = {
      if (ax.getSubClass.isInstanceOf[OWLClass] && ax.getSuperClass.isInstanceOf[OWLClass]) {
        val sup = ax.getSuperClass.asOWLClass()
        val sub = ax.getSubClass.asOWLClass()
        if (sup.isOWLThing) None
        else Some((sup, sub))
      } else None
    }

    logger.debug(s"Writing to DB in chunks of 1000")
    var stmts: mutable.MutableList[(OWLClass, OWLClass)] = mutable.MutableList.empty
    var i = 1
    axioms.foreach { el =>
      f(el) match {
        case Some(v) => {
          i += 1
          stmts.+:=(v)
          if (i % 1000 == 0) {
            logger.debug(s"$i elements processed, saving to DB")
            manager.run(DBIO.seq(subConcepts ++= stmts))
            stmts.clear()
          }
        }
        case None =>
      }
    }

    manager.run(DBIO.seq(subConcepts ++= stmts))
    stmts.clear()

    logger.debug(s"Saved $i elements to DB.")

    //writeToDBInChunks(axioms, subConcepts, 1000)(f)
    logger.debug(s"Added subclass assertions to the hierarchy.")

    logger.debug("Creating roles hierarchy")
    val rolesList: Seq[(OWLObjectProperty)] = manager.withDatabase(db => db.run((for {c <- roles} yield (c.iri)).result))
    for (c <- rolesList) {
      val stmts = for (sc <- helper.querySubRoles(c, false);
                       if c != sc) yield {
        subRoles += (c, sc)
      }
      manager.run(DBIO.seq(stmts.toSeq : _*))
    }
  }

  def saveAssertions(helper: OntologyHelper) = {
    logger.info("Saving assertions to database")
    manager.run(conceptAssertions.schema.truncate)
    manager.run(roleAssertions.schema.truncate)
    appendAssertionsToDatabase(helper)
  }

  def saveToDatabase(helper: OntologyHelper): Unit = {
    setupToDatabase(helper)
    saveAnnotations(helper)
    saveAssertions(helper)
    saveHierarchy(helper)
  }


  override def pShow: String = throw new NotImplementedError("Please implement pShow")

  def s2Ind(s: String): OWLNamedIndividual = NamedIndividual(s)

  override def lookup(cls: OWLClass): Traversable[OWLNamedIndividual] = {
    val q = for {
      c <- concepts; if c.iri === cls
      a <- conceptAssertions; if a.conceptiri === c.iri
      i <- individuals; if i.iri === a.indid
    } yield i.iri
    manager.withDatabase(db => db.run(q.result))
  }

  override def lookup(role: OWLObjectProperty): Traversable[(OWLNamedIndividual, OWLNamedIndividual)] = {
    val q = for {
      r <- roles; if r.iri === role
      a <- roleAssertions; if a.roleiri === r.iri
      i1 <- individuals; if i1.iri === a.ind1id
      i2 <- individuals; if i2.iri === a.ind2id
    } yield (i1.iri, i2.iri)
    manager.withDatabase(db => db.run(q.result))
  }

  override def getIndividuals: Traversable[OWLNamedIndividual] = {
    val q = for {
      i <- individuals
    } yield i.iri
    manager.withDatabase(db => db.run(q.result))
  }

  override def getClasses: Traversable[OWLClass] = {
    val q = for {
      i <- concepts
    } yield i.iri
    manager.withDatabase(db => db.run(q.result))
  }


  override def getRoles: Traversable[OWLObjectProperty] = {
    val q = for {
      i <- roles
    } yield i.iri
    manager.withDatabase(db => db.run(q.result))
  }


  override def getClasses(oWLNamedIndividual: OWLNamedIndividual): Iterable[OWLClass] = {
    val q = for {
      a <- conceptAssertions; if a.indid === oWLNamedIndividual
    } yield a.conceptiri
    manager.withDatabase(db => db.run(q.result))
  }


  override def isSatisfied(cls: OWLClass, term: OWLNamedIndividual): Boolean = {
    val q = for {
      a <- conceptAssertions; if a.indid === term && a.conceptiri === cls
    } yield a.indid
    manager.withDatabase(db => db.run(q.exists.result))
  }


  override def isSatisfied(role: OWLObjectProperty, term1: OWLNamedIndividual, term2: OWLNamedIndividual): Boolean = {
    val q = for {
      a <- roleAssertions; if a.ind1id === term1 && a.ind2id === term2 && a.roleiri === role
    } yield a.roleiri
    manager.withDatabase(db => db.run(q.exists.result))
  }

  override def getLabels(cls: OWLClass): Seq[String] = {
    val q = for {
      l <- conceptAnnotations; if l.conceptiri === cls
    } yield l.label
    manager.withDatabase(db => db.run(q.result))
  }

  override def getLabels(role: OWLObjectProperty): Seq[String] = {
    val q = for {
      l <- roleAnnotations; if l.roleiri === role
    } yield l.label
    manager.withDatabase(db => db.run(q.result))
  }

  override def getLabels(ind: OWLNamedIndividual): Seq[String] = {
    val q = for {
      l <- individualAnnotations; if l.indiri === ind
    } yield l.label
    manager.withDatabase(db => db.run(q.result))
  }


  override def getSignatureCounts: (Int, Int, Int) = {
    val i: Int = manager.withDatabase(_.run(individuals.length.result))
    val c: Int = manager.withDatabase(_.run(concepts.length.result))
    val r: Int = manager.withDatabase(_.run(roles.length.result))
    (i, c, r)
  }

  def isInitialized(ontology: OWLOntology): Boolean = {
    try {
      logger.debug(s"Checking if model is initalized for db ${manager.dbname}..")
      logger.debug(manager.dbparams.toString)
      val i = ontology.getIndividualsInSignature(Imports.INCLUDED).size()
      val c = ontology.getClassesInSignature(Imports.INCLUDED).size()
      val r = ontology.getObjectPropertiesInSignature(Imports.INCLUDED).size()
      val ocnts = (i, c, r)
      val cnts = getSignatureCounts
      logger.debug(s"Comparing signature dbcounts == ontologycounts; $cnts == $ocnts")
      cnts == ocnts
    } catch {
      case e: org.postgresql.util.PSQLException => {
        logger.debug("Database does not exist; Model is not initialized")
        e.getSQLState match {
          case "42P01" => return false
          case _ => throw e
        }
      }
    }
  }

  implicit val getListStringResult = GetResult[List[String]](
    prs => (1 to prs.numColumns).map(_ => prs.nextString).toList
  )


  override def getAnswers(fi: Query): Set[Answer] = {
    var query = ""
    var answerVars: List[String] = List.empty
    var temporalVars: List[String] = List.empty
    fi match {
      case f1: FOQuery => {
        val f = f1.simplify()
        answerVars = f.getAnswerVars().map(_.name).toList
        query = FOQueryTranslator.toSQLStatement(f).toSQL()
      }
      case f1: TemporalFOQuery => throw new Error("Temporal queries are not supported")
      case f1: QueryPlusFO => {
        return getAnswers(f1.toFirstOrderQuery())
      }
      case _ => throw new Exception(s"Query Answering for ${fi.getClass} is not supported in DatabaseModel.")
    }
    logger.debug(s"QUERY:\n $query")
    manager.withConnection { sql_connection =>
      // TODO: Reformulate to return also non answer vars
      //logger.debug("\n\n COMPLETE QUERY:\n" + query + "\n")
      try {
        val statement = sql_connection.prepareStatement(query)
        val resultSet = statement.executeQuery()
        var answers = new ListBuffer[Answer]()
        while (resultSet.next()) {
          val a = new mutable.MapBuilder[Term, OWLNamedIndividual, Map[Term, OWLNamedIndividual]](Map.empty)
          answerVars.foreach(v => a.+=((Variable(v), NamedIndividual(resultSet.getString(v)))))
          temporalVars.foreach(v => a.+=((Variable(v), NamedIndividual(resultSet.getString(v)))))
          answers.+=:(Answer(a.result()))
        }

        val ansR = answers.result().toSet
        /*logger.debug("\n\n COMPLETE QUERY:\n" + query + "\n")
        logger.info("Query answered.")
        logger.debug(ansR.toString)
        */
        return ansR
      } catch {
        case e: org.postgresql.util.PSQLException => {
          logger.error("Malformed SQL Query For ")
          logger.error("\n\n QUERY:\n" + fi.pShow() + "\n")
          logger.error("\n\n SQL QUERY:\n" + query + "\n")
          throw e
        }
      }
    }
  }

  private def subClassQuery(cls: Rep[OWLClass]) = for
    (c <- subConcepts; if c.conceptiri === cls)
    yield (c.subconceptiri)

  val subclassQueryCompiled = Compiled(subClassQuery _)

  override def querySubClasses(query: OWLClass, strict: Boolean, omitTop: Boolean, direct: Boolean): Set[OWLClass] = {
    var cls:Set[OWLClass] = direct match {
      case true => {
        manager.run(subclassQueryCompiled(query).result).toSet
      }
      case false => {
        querySubClassesRecursive(query)
      }
    }

    if (omitTop) cls = cls.filter(!_.isOWLThing)
    if (strict) cls = cls.diff(queryEquivalentClasses(query))
    else cls = cls.+(query)
    return cls
  }



  private def querySubClassesRecursive(cls: OWLClass): Set[OWLClass] = {
    val r: Set[OWLClass] = manager.run(subclassQueryCompiled(cls).result).toSet
    r++(r.flatMap(querySubClassesRecursive(_)))
  }

  override def queryEquivalentClasses(cls: OWLClass): Set[OWLClass] = {
    val q = for
      { c <- subConcepts if c.conceptiri === cls
        sr <- subConcepts if sr.conceptiri === c.subconceptiri && sr.subconceptiri === c.conceptiri }
      yield (c.subconceptiri)
    manager.run(q.result).toSet

    //val subCls = querySubClasses(cls, false, false, true)
    //subCls.filter(a => querySubClasses(a, false, false, true).contains(cls))
  }

  override def querySuperClasses(query: OWLClass, strict: Boolean): Set[OWLClass] = throw new NotImplementedError()

  private def subRolesQuery(role: Rep[OWLObjectProperty]) = for
    (c <- subRoles; if c.roleiri === role)
    yield (c.subroleiri)

  val subRolesQueryCompiled = Compiled(subRolesQuery _)

  override def querySubRoles(query: OWLObjectProperty, strict: Boolean): Set[OWLObjectProperty] = {
    var cls:Set[OWLObjectProperty] = querySubRolesRecursive(List(query))

    if (strict) cls = cls.diff(queryEquivalentRoles(query))
    else cls = cls.+(query)
    return cls
  }

  private def querySubRolesRecursive(queue: List[OWLObjectProperty], processed: Set[OWLObjectProperty] = Set()): Set[OWLObjectProperty] = {
    if (queue.isEmpty) processed
    else {
      val e = queue.head
      if (processed.contains(e)) querySubRolesRecursive(queue.tail, processed)
      else {
        val newRoles = manager.run(subRolesQueryCompiled(queue.head).result).toSet
        val newQueue = queue.tail ++ newRoles
        val newProcessed = processed + e
        querySubRolesRecursive(newQueue, newProcessed)
      }
    }
  }

  override def querySuperRoles(query: OWLObjectProperty, strict: Boolean): Set[OWLObjectProperty] = throw new NotImplementedError()

  override def queryEquivalentRoles(query: OWLObjectProperty): Set[OWLObjectProperty] = {
    val q = for
      { c <- subRoles if c.roleiri === query
        sr <- subRoles if sr.roleiri === c.subroleiri && sr.subroleiri === c.roleiri }
      yield (c.subroleiri)
    manager.run(q.result).toSet + query
    //val subRoles = querySubRoles(query, false)
    //subRoles.filter(a => querySubRoles(a, false).contains(query))
  }
}


