package de.tu_dresden.epistemic_rewriter

import java.time.{Instant, LocalDateTime, ZoneId}

import com.typesafe.scalalogging.StrictLogging
import de.tu_dresden.epistemic_rewriter.datatypes.{Query, _}
import org.phenoscape.scowl._
import org.semanticweb.elk.owlapi.ElkReasonerFactory
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.model._
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.reasoner.{InferenceType, OWLReasoner, SimpleConfiguration}
import slick.jdbc.GetResult
import slick.jdbc.PostgresProfile.api._

import scala.collection.JavaConverters._
import scala.collection.immutable.Set
import scala.collection.mutable.ListBuffer
import scala.collection.{SortedSet, mutable}


object TemporalDatabaseModel {
  def saveToDatabase(helper: OntologyHelper, manager: DatabaseManager, withInference: Boolean): TemporalDatabaseModel = {
    val dModel = TemporalDatabaseModel(manager, withInference, helper)
    dModel.saveToDatabase()
    return dModel
  }
}


/**
  * A database model for a (temporal) ontology.
  * It adds all non-temporal facts to the database with a null entry as timepoint.
  * All temporal information are added to the database with a timepoint.
  * In between any two given timepoints, an artificial timepoint is introduced
  * acting as a representative for the whole intermediate interval.
  * When querying, a specific timepoint can be set at which the query should be evaluated.
  * Only information at this timepoint are then used to answer the query.
  * When no timepoint is given, the query is evaluated only over the atemporal facts
  * (that don't have any timepoint associated with them).
  * @param manager
  * @param withInference
  * @param timePoint If given queries only facts asserted at the given timePoint
  */
case class TemporalDatabaseModel(manager: DatabaseManager, withInference: Boolean, helper: OntologyHelper) extends Modellike with TimeAwareStorage with IRIStorage with DiamondStorage with Hierarchy with StrictLogging {


  private var timePoint: TimePoint = _

  def setTimePoint(t: TimePoint) = timePoint = t //getRepresentative(t)
  def setToLastRealTimePoint() = timePoint = getRealTimePoints().last
  def getTimePoint() = timePoint



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

  class Timepoints(tag: Tag) extends Table[(Long, TimePoint, Boolean, ExtTimePoint, ExtTimePoint)](tag, "timepoints") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def timepoint = column[TimePoint]("timepoint")

    def real = column[Boolean]("real")

    def from = column[ExtTimePoint]("dt_from")
    def to = column[ExtTimePoint]("dt_to")

    def idxT = index("idx_c_t", (timepoint), unique = true)

    def * = (id, timepoint, real, from, to)
    def forInsert = (timepoint, real, from, to)
  }

  class ConceptAssertions(tag: Tag) extends Table[(OWLClass, OWLNamedIndividual, Long)](tag, "concept_assertions") {
    def conceptiri = column[OWLClass]("concept_id")

    def indid = column[OWLNamedIndividual]("ind_id")

    def timepoint_id= column[Long]("timepoint_id")

    def * = (conceptiri, indid, timepoint_id)

    def pk = index("pk_c", (conceptiri, indid, timepoint_id), unique = true)

    def idx = index("idx_c", (conceptiri), unique = false)

    def idx1 = index("idx_indid_name", (indid), unique = false)

    // A reified foreign key relation that can be navigated to create a join
    def concept = foreignKey("concept_fk", conceptiri, concepts)(_.iri)

    def individual = foreignKey("individual_fk", indid, individuals)(_.iri)

    def ts = foreignKey("ts_fk", timepoint_id, timepoints)(_.id)
  }

  class RoleAssertions(tag: Tag) extends Table[(OWLObjectProperty, OWLNamedIndividual, OWLNamedIndividual, Long)](tag, "role_assertions") {
    def roleiri = column[OWLObjectProperty]("role_id")

    def ind1id = column[OWLNamedIndividual]("ind_1_id")

    def ind2id = column[OWLNamedIndividual]("ind_2_id")

    def timepoint_id = column[Long]("timepoint_id")

    def * = (roleiri, ind1id, ind2id, timepoint_id)

    def pk = index("pk_ra", (roleiri, ind1id, ind2id, timepoint_id), unique = true)

    def idx = index("idx_ra", (roleiri), unique = false)

    def idxT = index("idx_r_t", (timepoint_id), unique = false)

    def idx1 = index("idx_indids_name", (ind1id, ind2id), unique = false)

    def role = foreignKey("role_fk", roleiri, roles)(_.iri)

    def individual1 = foreignKey("individual_fk_1", ind1id, individuals)(_.iri)

    def individual2 = foreignKey("individual_fk_2", ind2id, individuals)(_.iri)

    def ts = foreignKey("ts_fk", timepoint_id, timepoints)(_.id)
  }

  class SubRoles(tag: Tag) extends Table[(Diamond, OWLObjectProperty, OWLObjectProperty)](tag, "sub_roles") {
    def operator = column[Diamond]("operator")

    def roleiri = column[OWLObjectProperty]("role_id")

    def subroleiri = column[OWLObjectProperty]("sub_role_id")

    def * = (operator, roleiri, subroleiri)

    def pk = primaryKey("pk_sr", (roleiri, subroleiri))

    def idx1 = index("idx_r1", (roleiri), unique = false)

    def idx2 = index("idx_r2", (subroleiri), unique = false)

    def role = foreignKey("role_fk", roleiri, roles)(_.iri)

    def subrole = foreignKey("subrole_fk", subroleiri, roles)(_.iri)
  }

  class SubConcepts(tag: Tag) extends Table[(Diamond, OWLClass, OWLClass)](tag, "sub_concepts") {
    def operator = column[Diamond]("operator")

    def conceptiri = column[OWLClass]("concept_id")

    def subconceptiri = column[OWLClass]("sub_concept_id")

    def * = (operator, conceptiri, subconceptiri)

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
  val timepoints = TableQuery[Timepoints]

  val allRelations = List(
    timepoints.schema,
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
            TRUNCATE TABLE timepoints, individuals, concepts, roles CASCADE;
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


  def addFunctionsToDatabase():Unit = {
    val changeType =
      """
        |ALTER TABLE timepoints
        |ALTER COLUMN dt_from TYPE timestamp USING timepoint::timestamp without time zone,
        |ALTER COLUMN dt_to TYPE timestamp USING timepoint::timestamp without time zone;
      """.stripMargin
    manager.raw(changeType)

    val relevant_timestamps =
      """
        CREATE OR REPLACE FUNCTION relevant_timepoints(a timestamp, b timestamp) RETURNS SETOF timepoints AS $$
        	SELECT * FROM timepoints t1 WHERE t1.timepoint NOT IN (SELECT t.timepoint
        	FROM timepoints t
        	WHERE t.dt_from > b
        	OR t.dt_to < a)
        $$ LANGUAGE SQL;
      """.stripMargin
    manager.raw(relevant_timestamps)
  }

  /**
    * Append the signature of a given ontology to the database
    *
    * @param ontology
    */
  def appendSignatureToDatabase(ontology: OWLOntology): Unit = {
    logger.debug("Saving Individuals Signature")
    val inds = ontology.getIndividualsInSignature(Imports.INCLUDED).asScala.toSet
    manager.run(DBIO.seq(individuals ++= inds))


    logger.debug("Saving Concepts Signature")
    val cls = ontology.getClassesInSignature(Imports.INCLUDED).asScala.toSet.+(OWLThing, OWLNothing)
    manager.run(DBIO.seq(concepts ++= cls))

    logger.debug("Saving Roles Signature")
    val rls = ontology.getObjectPropertiesInSignature(Imports.INCLUDED).asScala.toSet
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

  def appendAnnotationsToDatabase(ontology: OWLOntology) = {}
  /*{
    val queue = List(
      (for {i <- individuals} yield (i), individualAnnotations),
      (for {i <- concepts} yield (i), conceptAnnotations),
      (for {i <- roles} yield (i), roleAnnotations))
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
  }
  */

  /*def appendHierarchyToDatabase(helper: OntologyHelper): Unit = {
    logger.debug("Creating concepts hierarchy")
    val cls: Seq[(String)] = manager.withDatabase(db => db.run((for {c <- concepts} yield (c.iri)).result))
    val s = cls.size
    var i = 0
    for (iri <- cls) {

      i += 1; if (i % 10 == 0) logger.debug(s"$i / $s")
      val c = Class(iri)
      val stmts = for (sc <- helper.querySubClasses(c, false, true, true).asScala;
                       if sc != iri) yield {
        subConcepts += (iri, sc)
      }
      manager.run(DBIO.seq(stmts: _*))
    }
    logger.debug(s"Added $s concepts to the hierarchy.")

    logger.debug("Creating roles hierarchy")
    val rolesList: Seq[(String)] = manager.withDatabase(db => db.run((for {c <- roles} yield (c.iri)).result))
    for ((iri) <- rolesList) {
      val c = ObjectProperty(iri)
      val stmts = for (sc <- helper.querySubRoles(c, false);
                       if iri != sc) yield {
        subRoles += (iri, sc)
      }
      manager.run(DBIO.seq(stmts: _*))
    }
  }*/

  def appendTimepointsToDatabase(helper: OntologyHelper) = {



    val realTimepoints = helper.getTimepoints().sorted


    val delta = Diamond.MIN_DIFF.dividedBy(2)
    var stmts: mutable.MutableList[(Long,TimePoint, Boolean, ExtTimePoint, ExtTimePoint)] = mutable.MutableList.empty
    if (realTimepoints.isEmpty) {
      stmts += ((0, TimePoint(LocalDateTime.now()), false, NegInfinity(), PosInfinity()))
    } else {
      // NegInfinity()()
      stmts += ((0, realTimepoints.head.minus(Diamond.MIN_DIFF), false, NegInfinity(), Point(realTimepoints.head.minus(delta))))
      // future
      stmts += ((0, realTimepoints.last.plus(Diamond.MIN_DIFF), false, Point(realTimepoints.last.plus(delta)), PosInfinity()))
    }

    // Create all real timepoints
    for {tp <- realTimepoints} {
      stmts += ((0, TimePoint(tp.p), true, Point(tp.minus(delta)), Point(tp.plus(delta))))
    }

    // Create all intermediate timepoints
    val r = for {i <- 0 to realTimepoints.size-2} yield {
      val a = realTimepoints(i)
      val b = realTimepoints(i+1)
      if (a.plus(Diamond.MIN_DIFF).isBefore(b)) {
        stmts += ((0, a.plus(Diamond.MIN_DIFF), false, Point(a.plus(delta)), Point(b.minus(delta))))
      }
      else {
        None
        throw new Error("The distance between two timepoints is smaller than the resolution..")
      }
    }
    val sql =
      s"""
        |INSERT INTO "timepoints" ("timepoint","real","dt_from","dt_to")  VALUES \n${stmts.map{ case (_, t, r, from, to) => s"('${t.toString}', '${r.toString}', '${from.toString}', '${to.toString}')"}.mkString(",\n")}
      """.stripMargin

    //logger.debug(sql)


    manager.raw(sql)
  }

  /**
    * Append assertions to the database
    * For each (real and representative) timepoint all assertions are added.
    * @param helper
    */
  def appendAssertionsToDatabaseM(helper: OntologyHelper): Unit = {




    logger.debug("Computing subsumptions Map")
    val reasoner = TELHReasonerFactory.createReasoner(helper.ontology)
    val (roleAxioms, clsAxioms) = reasoner.run()

    // The temporal subsumption hierarchy
    val subsumptionsR:mutable.Map[OWLObjectProperty, Set[OWLObjectProperty]] = mutable.Map[OWLObjectProperty, Set[OWLObjectProperty]]()
    for (roleAxiom <- roleAxioms
         if roleAxiom.subProperty().isInstanceOf[OWLObjectProperty] && roleAxiom.superProperty().isInstanceOf[OWLObjectProperty]) {
      val sub = roleAxiom.subProperty().asOWLObjectProperty()
      val sup = roleAxiom.superProperty().asOWLObjectProperty()
      val old = subsumptionsR.getOrElse(sub, Set[OWLObjectProperty]())
      subsumptionsR.update(sub, old.+(sup))
    }



    // The temporal subsumption hierarchy
    val subsumptionsC:mutable.Map[OWLClass, mutable.Map[Diamond, Set[OWLClass]]] = mutable.Map[OWLClass, mutable.Map[Diamond, Set[OWLClass]]]()
    for (clsAxiom <- clsAxioms
         if clsAxiom.subClass().isInstanceOf[OWLClass] && clsAxiom.superClass().isInstanceOf[OWLClass]) {
      val cls = clsAxiom.subClass().asOWLClass()
      val sup = clsAxiom.superClass().asOWLClass()
      subsumptionsC.get(cls) match {
        case None => {
          val m:mutable.Map[Diamond, Set[OWLClass]] = mutable.Map[Diamond, Set[OWLClass]]((clsAxiom.operator, Set(sup)))
          subsumptionsC.update(cls, m)
        }
        case Some(m) => {
          m.get(clsAxiom.operator) match {
            case None => {
              m.update(clsAxiom.operator, Set(sup))
            }
            case Some(sups) => m.update(clsAxiom.operator, sups.+(sup))
          }
        }
      }
    }



    logger.debug("Completing ABox")
    val tps = SortedSet(getTimePoints().toSeq :_*)
    val initC:Map[(OWLClass, OWLIndividual),SortedSet[TimePoint]] = helper.getTemporalClassAssertions()
    val initR:Map[(OWLObjectProperty, OWLIndividual, OWLIndividual),SortedSet[TimePoint]] = helper.getTemporalObjectPropertyAssertions()
    // The completed set of assertions.
    val assertionsR = complete(initR, subsumptionsR.toMap)
    val assertionsC = complete(initC, assertionsR, subsumptionsC, helper.getExistentialSubsumptions(), helper.getComplexSubsumptions())(tps)

    logger.debug("Saving ABox to DB")
    // Create a base ontology that contains only the a temporal parts, ie the TBox and RBox
    val m = OWLManager.createOWLOntologyManager
    val ontology = m.createOntology(helper.ontology.getOntologyID)

    m.setIRIMappers(helper.manager.getIRIMappers.asScala.toSet.asJava)
    /*helper.ontology.getImportsDeclarations.forEach(i => {
      m.makeLoadImportRequest(i)
      m.applyChange(new AddImport(ontology, i))
    })*/

    // TODO: Is EXCLUDED correct here?
    m.addAxioms(ontology, helper.ontology.getTBoxAxioms(Imports.INCLUDED))
    m.addAxioms(ontology, helper.ontology.getRBoxAxioms(Imports.INCLUDED))

    logger.debug("Created Ontology. Loading reasoner...")
    // Load Reasoner and do the inference
    val elkReasoner = new ElkReasonerFactory().createReasoner(ontology, new SimpleConfiguration())
    elkReasoner.precomputeInferences(InferenceType.CLASS_HIERARCHY, InferenceType.DATA_PROPERTY_HIERARCHY)
    /**
      * STEP 2:
      * For each time point t (real and virtual) add all ABox axioms/assertions that hold at t.
      * The resulting ontology contains all valid information for the given time point.
      * It is then save2db. If inference is required, the ontology is loaded into ELK and the completed axioms are then saved to DB.
      */
    logger.debug("Reasoner Loaded. Starting to write temporal assertions to DB.")

    /**
      * For each time point go through all the patients
      */

    val patients_IRIs = for {i <- 100 to 399 } yield helper.ontology.getIndividualsInSignature().asScala.filter(_.getIRI.toString.contains("n2c2#p"+i.toString))

    val list_to_iterate = for { p <- patients_IRIs; t <- tps } yield (p.toSet,t)

    forWithProgress(list_to_iterate, "Writing Temporal Assertions") { item =>
      val relevant_iris = item._1
      val time:TimePoint = item._2
      // Add ObjectPropertyAssertions
      val axiomsR = helper.getObjectPropertyAssertions(time).filter(_.getIndividualsInSignature.asScala.forall(relevant_iris.contains(_))).asJava

      // Add Class Assertions
      val axiomsC = for {((c, a), ts) <- assertionsC
                        if ts.contains(time) && relevant_iris.contains(a.asOWLNamedIndividual())} yield ClassAssertion(c, a)

      val axiomsJ = axiomsC.toSet.asJava

      m.addAxioms(ontology, axiomsJ)
      m.addAxioms(ontology, axiomsR)

      if (withInference) {
        // Add axioms to reasoner
        //m.addAxioms(ontology, axiomsJ)
        //m.addAxioms(ontology, axiomsR)
        // This is a buffering reasoner, so you need to flush the changes
        elkReasoner.flush()
        save2dbWithInference(elkReasoner, ontology, time)
        //reasoner.dispose()
      } else {
        save2dbWithoutInference(ontology, time)
      }


      m.removeAxioms(ontology, axiomsJ)
      m.removeAxioms(ontology, axiomsR)
    }

    elkReasoner.dispose()

    //
    //manager.raw("DELETE FROM concept_assertions \nWHERE timepoint NOTNULL \nAND (concept_id, ind_id) IN \n(SELECT concept_id, ind_id\nFROM concept_assertions \nWHERE timepoint ISNULL);")
    logger.debug("Done..")
  }

  /**
    * Append assertions to the database
    * For each (real and representative) timepoint all assertions are added.
    * @param helper
    */
  def appendAssertionsToDatabase(helper: OntologyHelper): Unit = {





    logger.debug("Computing subsumptions Map")
    val reasoner = TELHReasonerFactory.createReasoner(helper.ontology)
    val (roleAxioms, clsAxioms) = reasoner.run()

    // The temporal subsumption hierarchy
    val subsumptionsR:mutable.Map[OWLObjectProperty, Set[OWLObjectProperty]] = mutable.Map[OWLObjectProperty, Set[OWLObjectProperty]]()
    for (roleAxiom <- roleAxioms
         if roleAxiom.subProperty().isInstanceOf[OWLObjectProperty] && roleAxiom.superProperty().isInstanceOf[OWLObjectProperty]) {
      val sub = roleAxiom.subProperty().asOWLObjectProperty()
      val sup = roleAxiom.superProperty().asOWLObjectProperty()
      val old = subsumptionsR.getOrElse(sub, Set[OWLObjectProperty]())
      subsumptionsR.update(sub, old.+(sup))
    }



    // The temporal subsumption hierarchy
    val subsumptionsC:mutable.Map[OWLClass, mutable.Map[Diamond, Set[OWLClass]]] = mutable.Map[OWLClass, mutable.Map[Diamond, Set[OWLClass]]]()
    for (clsAxiom <- clsAxioms
         if clsAxiom.subClass().isInstanceOf[OWLClass] && clsAxiom.superClass().isInstanceOf[OWLClass]) {
      val cls = clsAxiom.subClass().asOWLClass()
      val sup = clsAxiom.superClass().asOWLClass()
      subsumptionsC.get(cls) match {
        case None => {
          val m:mutable.Map[Diamond, Set[OWLClass]] = mutable.Map[Diamond, Set[OWLClass]]((clsAxiom.operator, Set(sup)))
          subsumptionsC.update(cls, m)
        }
        case Some(m) => {
          m.get(clsAxiom.operator) match {
            case None => {
              m.update(clsAxiom.operator, Set(sup))
            }
            case Some(sups) => m.update(clsAxiom.operator, sups.+(sup))
          }
        }
      }
    }



    logger.debug("Completing ABox")
    val tps = SortedSet(getTimePoints().toSeq :_*)
    val initC:Map[(OWLClass, OWLIndividual),SortedSet[TimePoint]] = helper.getTemporalClassAssertions()
    val initR:Map[(OWLObjectProperty, OWLIndividual, OWLIndividual),SortedSet[TimePoint]] = helper.getTemporalObjectPropertyAssertions()
    // The completed set of assertions.
    val assertionsR = complete(initR, subsumptionsR.toMap)
    val assertionsC = complete(initC, assertionsR, subsumptionsC, helper.getExistentialSubsumptions(), helper.getComplexSubsumptions())(tps)

    logger.debug("Saving ABox to DB")
    // Create a base ontology that contains only the a temporal parts, ie the TBox and RBox
    val m = OWLManager.createOWLOntologyManager
    val ontology = m.createOntology(helper.ontology.getOntologyID)

    m.setIRIMappers(helper.manager.getIRIMappers.asScala.toSet.asJava)
    /*helper.ontology.getImportsDeclarations.forEach(i => {
      m.makeLoadImportRequest(i)
      m.applyChange(new AddImport(ontology, i))
    })*/

    // TODO: Is EXCLUDED correct here?
    m.addAxioms(ontology, helper.ontology.getTBoxAxioms(Imports.INCLUDED))
    m.addAxioms(ontology, helper.ontology.getRBoxAxioms(Imports.INCLUDED))

    logger.debug("Created Ontology. Loading reasoner...")
    // Load Reasoner and do the inference
    val elkReasoner = new ElkReasonerFactory().createReasoner(ontology, new SimpleConfiguration())
    elkReasoner.precomputeInferences(InferenceType.CLASS_HIERARCHY, InferenceType.DATA_PROPERTY_HIERARCHY)
    /**
      * STEP 2:
      * For each time point t (real and virtual) add all ABox axioms/assertions that hold at t.
      * The resulting ontology contains all valid information for the given time point.
      * It is then save2db. If inference is required, the ontology is loaded into ELK and the completed axioms are then saved to DB.
      */
    logger.debug("Reasoner Loaded. Starting to write temporal assertions to DB.")

    forWithProgress(getTimePoints().toList, "Writing Temporal Assertions") { time =>
      // Add ObjectPropertyAssertions
      val axiomsR = helper.getObjectPropertyAssertions(time).asJava

      // Add Class Assertions
      val axiomsC = for {((c, a), ts) <- assertionsC
                         if ts.contains(time)} yield ClassAssertion(c, a)

      val axiomsJ = axiomsC.toSet.asJava

      m.addAxioms(ontology, axiomsJ)
      m.addAxioms(ontology, axiomsR)

      if (withInference) {
        // Add axioms to reasoner
        //m.addAxioms(ontology, axiomsJ)
        //m.addAxioms(ontology, axiomsR)
        // This is a buffering reasoner, so you need to flush the changes
        elkReasoner.flush()
        save2dbWithInference(elkReasoner, ontology, time)
        //reasoner.dispose()
      } else {
        save2dbWithoutInference(ontology, time)
      }


      m.removeAxioms(ontology, axiomsJ)
      m.removeAxioms(ontology, axiomsR)
    }

    elkReasoner.dispose()

    //
    //manager.raw("DELETE FROM concept_assertions \nWHERE timepoint NOTNULL \nAND (concept_id, ind_id) IN \n(SELECT concept_id, ind_id\nFROM concept_assertions \nWHERE timepoint ISNULL);")
    logger.debug("Done..")
  }

  case class TemporalClassAssertion(timepoint: TimePoint, cls: OWLClass, ind: OWLIndividual)
  case class TemporalRoleAssertion(timepoint: TimePoint, role: OWLObjectProperty, ind1: OWLIndividual, ind2: OWLIndividual)

  // Complete Roles

  def complete(initR: Map[(OWLObjectProperty, OWLIndividual, OWLIndividual),SortedSet[TimePoint]], subsumptionsR: Map[OWLObjectProperty, Set[OWLObjectProperty]])
              : Map[(OWLObjectProperty, OWLIndividual, OWLIndividual),SortedSet[TimePoint]] = {
    val assertions: mutable.Map[(OWLObjectProperty, OWLIndividual, OWLIndividual), SortedSet[TimePoint]] =
      mutable.Map[(OWLObjectProperty, OWLIndividual, OWLIndividual), SortedSet[TimePoint]](initR.toSeq: _*)
    val todo: mutable.Queue[(OWLObjectProperty, OWLIndividual, OWLIndividual)] = mutable.Queue[(OWLObjectProperty, OWLIndividual, OWLIndividual)](initR.keys.toSeq: _*)

    while (todo.nonEmpty) {
      val (c, i1, i2) = todo.dequeue()
      val ts = assertions.getOrElse((c, i1, i2), SortedSet[TimePoint]())

      // A3
      for (d <- subsumptionsR.getOrElse(c, Set[OWLObjectProperty]())) {
        val ts1 = assertions.getOrElse((d, i1, i2), SortedSet[TimePoint]())
        if (!ts.subsetOf(ts1)) {
          assertions.update((d, i1, i2), ts1 ++ ts)
          todo.enqueue((d, i1, i2))
        }
      }
    }
    assertions.toMap
  }

  /**
    * Complete classes
    * @param initC
    * @param subsumptionsR
    * @param subsumptionsC
    * @param existentialSubs
    * @param complexSubs
    * @param availableTimePoints
    * @return
    */
  def complete(initC: Map[(OWLClass, OWLIndividual),SortedSet[TimePoint]],
               subsumptionsR: Map[(OWLObjectProperty, OWLIndividual, OWLIndividual),SortedSet[TimePoint]],
               subsumptionsC: mutable.Map[OWLClass, mutable.Map[Diamond, Set[OWLClass]]],
               existentialSubs: Map[(OWLObjectProperty, OWLClass), OWLClass],
               complexSubs: Map[Set[OWLClass],OWLClass])
              (implicit availableTimePoints:SortedSet[TimePoint]): Map[(OWLClass, OWLIndividual),SortedSet[TimePoint]]  = {
    val assertions: mutable.Map[(OWLClass, OWLIndividual),SortedSet[TimePoint]] =
      mutable.Map[(OWLClass, OWLIndividual),SortedSet[TimePoint]](initC.toSeq :_*)
    val todo: mutable.Queue[(OWLClass, OWLIndividual)] = mutable.Queue[(OWLClass, OWLIndividual)](initC.keys.toSeq :_*)

    // Complete Roles
    // A3



    // Complete Concepts
    while (todo.nonEmpty) {
      val (c,i) = todo.dequeue()
      val ts = assertions.getOrElse((c,i),SortedSet[TimePoint]())

      // A2
      subsumptionsC.get(c) match {
        case Some(m) =>
          for {(d,subs) <- m
                s <- subs} {
            val completedTs = d.complete(ts)
            val ts1 = assertions.getOrElse((s,i),SortedSet[TimePoint]())
            // Compute completed timepoints with diamond d
            if (!completedTs.subsetOf(ts1)) {
              assertions.update((s,i), ts1 ++ completedTs)
              todo.enqueue((s,i))
            }
          }
        case None =>
      }

      // A4
      for { (k,e) <- complexSubs.filterKeys(_.contains(c)) } {
        val common_ts = k.map(d => assertions.getOrElse((d,i),SortedSet[TimePoint]())).fold(SortedSet[TimePoint]())(_.intersect(_))
        val ts1 = assertions.getOrElse((e,i),SortedSet[TimePoint]())
        if (!common_ts.subsetOf(ts1)) {
          assertions.update((e, i), ts1 ++ common_ts)
          todo.enqueue((e, i))
        }
      }

      // A5
      for { ((r,_),e) <- existentialSubs.filterKeys(_._2 == c)
            ((_, i1, i2), ts1) <- subsumptionsR.filterKeys{ case (r1, i1, i2) => r1 == r && i2 == i}
      } {

        val common_ts = ts.intersect(ts1)
        val ts2 = assertions.getOrElse((e,i1),SortedSet[TimePoint]())
        if (!common_ts.subsetOf(ts2)) {
          assertions.update((e, i1), ts2 ++ common_ts)
          todo.enqueue((e, i1))
        }
      }

    }
    Map[(OWLClass, OWLIndividual),SortedSet[TimePoint]](assertions.toSeq :_*)
  }

  // TEMPORAL ROLES Are not supported for query answering.. therefore this part is obsolete..
  /*def complete(init: Map[(OWLObjectProperty, OWLIndividual, OWLIndividual),SortedSet[TimePoint]], subsumptions: mutable.Map[OWLObjectProperty, (Diamond, Set[OWLObjectProperty])])
              (implicit availableTimePoints:SortedSet[TimePoint]): Map[(OWLObjectProperty, OWLIndividual, OWLIndividual),SortedSet[TimePoint]]  = {
    val assertions: mutable.Map[(OWLObjectProperty, OWLIndividual, OWLIndividual),SortedSet[TimePoint]] = mutable.Map[(OWLObjectProperty, OWLIndividual, OWLIndividual),SortedSet[TimePoint]](init.toSeq :_*)
    val todo: mutable.Queue[(OWLObjectProperty, OWLIndividual, OWLIndividual)] = mutable.Queue[(OWLObjectProperty, OWLIndividual, OWLIndividual)](init.keys.toSeq :_*)

    while (todo.nonEmpty) {
      val (c,i1, i2) = todo.dequeue()
      val ts = assertions.getOrElse((c,i1, i2),SortedSet[TimePoint]())
      subsumptions.get(c) match {
        case Some((d, subs)) => {
          for (s <- subs) {
            val completedTs = d.complete(ts)
            val ts1 = assertions.getOrElse((s,i1, i2),SortedSet[TimePoint]())
            // Compute completed timepoints with diamond d
            if (!completedTs.subsetOf(ts1)) {
              assertions.update((s,i1, i2), ts1 ++ ts)
              todo.enqueue((s,i1, i2))
            }
          }
        }
        case None =>
      }
    }
    Map[(OWLObjectProperty, OWLIndividual, OWLIndividual),SortedSet[TimePoint]](assertions.toSeq :_*)
  }
  */
/*
  def save2db(ontology: OWLOntology, timepoint: TimePoint, withInference: Boolean) = withInference match {
    case true => {
      logger.debug("Saving to DB all inferred Axioms.")
      save2dbWithInference(ontology, timepoint)
    }
    case false => {
      logger.debug("Saving to DB only stated Axioms.")
      save2dbWithoutInference(ontology, timepoint)
    }
  }
*/
  /**
    *
    * @param ontology
    * @param timepoint
    */
  def save2dbWithoutInference(ontology: OWLOntology, timepoint: TimePoint) = {
    // Load Reasoner and do the inference
    //val reasoner = new ElkReasonerFactory().createReasoner(ontology, new SimpleConfiguration())
    //reasoner.precomputeInferences(InferenceType.CLASS_ASSERTIONS, InferenceType.DATA_PROPERTY_ASSERTIONS)

    val timepoint_id = getTimePointID(timepoint)
    val cls = ontology.getClassesInSignature(Imports.INCLUDED)
    var i = 1
    var stmts: mutable.MutableList[(OWLClass, OWLNamedIndividual, Long)] = mutable.MutableList.empty
    cls.forEach { case cls: OWLClass =>
      i += 1;

      stmts.++=(ontology.getClassAssertionAxioms(cls).asScala.toSeq.map(i => (cls, i.getIndividual.asOWLNamedIndividual(), timepoint_id)))
      if (i % 100000 == 0) {
        logger.debug(s"$i concept assertions processed. Saving to DB.")
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
    var stmtsR: mutable.MutableList[(OWLObjectProperty, OWLNamedIndividual, OWLNamedIndividual, Long)] = mutable.MutableList.empty
    for {ax <- ontology.getABoxAxioms(Imports.INCLUDED).asScala;
         if ax.isInstanceOf[OWLObjectPropertyAssertionAxiom]} {
      val ax1: OWLObjectPropertyAssertionAxiom = ax.asInstanceOf[OWLObjectPropertyAssertionAxiom]
      val role = ax1.getProperty.asOWLObjectProperty()
      val ind1 = ax1.getSubject.asOWLNamedIndividual()
      val ind2 = ax1.getObject.asOWLNamedIndividual()
      //val x = ax1.getAnnotations() //(new OWLAnnotationPropertyImpl(IRI.create(":timepoint"))).asScala
      //for (t <- x.asScala) {
      //  logger.debug(t.toString)
      //}
      stmtsR.++=(for (superRole <- Seq(role)) yield {
        i += 1
        (superRole, ind1, ind2, timepoint_id)
      })

      if (i % 100000 == 0) {
        logger.debug(s"$i role assertions processed. Saving to DB.")
        manager.insert("role_assertions", stmtsR.map(_.toString))
        //manager.run(DBIO.seq(roleAssertions ++= stmtsR))
        stmtsR.clear
      }
    }
    logger.debug(s"$i role assertions processed. Saving to DB.")
    manager.insert("role_assertions", stmtsR.map(_.toString))
    //manager.run(DBIO.seq(roleAssertions ++= stmtsR))
    stmtsR.clear
  }

  def save2dbWithInference(reasoner: OWLReasoner, ontology: OWLOntology, timepoint: TimePoint) = {
    val timepoint_id = getTimePointID(timepoint)



    reasoner.precomputeInferences(InferenceType.CLASS_ASSERTIONS, InferenceType.DATA_PROPERTY_ASSERTIONS)

    var stmts: mutable.MutableList[(OWLClass, OWLNamedIndividual, Long)] = mutable.MutableList.empty
    val inds = ontology.getIndividualsInSignature(Imports.INCLUDED)
    val report_distance = List(inds.size()/ 5, 1).sorted.last
    //cls.remove(OWLThing)
    var i = 1

    inds.forEach { case ind: OWLNamedIndividual =>
      i += 1;
      stmts.++=(reasoner.getTypes(ind, false).getFlattened.asScala.toSeq.map(cls => (cls, ind, timepoint_id)))
      if (i % report_distance == 0) {
        logger.debug(s"$i concept assertions processed. Saving to DB.")
        manager.run(DBIO.seq(conceptAssertions ++= stmts))
        stmts.clear
      }
    }
    /*
    val cls = ontology.getClassesInSignature(Imports.INCLUDED)
    //cls.remove(OWLThing)
    var i = 1

    cls.forEach { case cls: OWLClass =>
      i += 1;
      reasoner.getTypes()
      stmts.++=(reasoner.getInstances(cls, false).getFlattened.asScala.toSeq.map(i => (cls, i, timepoint_id)))
      if (i % 100000 == 0) {
        logger.debug(s"$i concept assertions processed. Saving to DB.")
        manager.run(DBIO.seq(conceptAssertions ++= stmts))
        stmts.clear
      }
    }*/
    logger.debug(s"$i concept assertions processed. Saving to DB.")
    manager.run(DBIO.seq(conceptAssertions ++= stmts))
    stmts.clear

    logger.debug("Adding role assertions")
    // Construct Role Assertions

    val roleHierarchy = new RoleHierarchy(ontology)
    i = 1
    var stmtsR: mutable.MutableList[(OWLObjectProperty, OWLNamedIndividual, OWLNamedIndividual, Long)] = mutable.MutableList.empty
    for {ax <- ontology.getABoxAxioms(Imports.INCLUDED).asScala;
         if ax.isInstanceOf[OWLObjectPropertyAssertionAxiom]} {
      val ax1: OWLObjectPropertyAssertionAxiom = ax.asInstanceOf[OWLObjectPropertyAssertionAxiom]
      val role = ax1.getProperty.asOWLObjectProperty()
      val ind1 = ax1.getSubject.asOWLNamedIndividual()
      val ind2 = ax1.getObject.asOWLNamedIndividual()
      /*val x = ax1.getAnnotations() //(new OWLAnnotationPropertyImpl(IRI.create(":timepoint"))).asScala
      for (t <- x.asScala) {
        logger.debug(t.toString)
      }*/
      stmtsR.++=(for (superRole <- roleHierarchy.getSuperRoles(role, false).toSeq) yield {
        i += 1
        (superRole, ind1, ind2, timepoint_id)
      })

      if (i % 100000 == 0) {
        logger.debug(s"$i role assertions processed. Saving to DB.")
        manager.run(DBIO.seq(roleAssertions ++= stmtsR))
        stmtsR.clear
      }
    }
    logger.debug(s"$i role assertions processed. Saving to DB.")
    manager.run(DBIO.seq(roleAssertions ++= stmtsR))
    stmtsR.clear

  }

  def setupToDatabase(helper: OntologyHelper) = {
    logger.info("Setting up database schema")
    setup()
    logger.info("Saving signature to database")
    appendSignatureToDatabase(helper.ontology)
    logger.info("Creating required DB functions.")
    addFunctionsToDatabase()

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


  // TODO: Use ELH algorithm to save only direct subsumptions
  def appendHierarchyToDatabaseFast(helper: OntologyHelper) = {
    val reasoner = TELHReasonerFactory.createReasoner(helper.ontology)
    val (roleAxioms, conceptAxioms) = reasoner.run()

    logger.debug("Creating concepts hierarchy")
    val axioms = conceptAxioms

    def f(tax: TemporalSubClassOfAxiom): Option[(Diamond, OWLClass, OWLClass)] = {
      val ax = tax.axiom
      if (ax.getSubClass.isInstanceOf[OWLClass] && ax.getSuperClass.isInstanceOf[OWLClass]) {
        val sup = ax.getSuperClass.asOWLClass()
        val sub = ax.getSubClass.asOWLClass()
        val d = tax.operator
        if (sup.isOWLThing) None
        else Some((d, sup, sub))
      } else None
    }

    logger.debug(s"Writing to DB in chunks of 1000")
    var stmts: mutable.MutableList[(Diamond , OWLClass, OWLClass)] = mutable.MutableList.empty
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


    def g(tax: TemporalSubObjectPropertyOfAxiom): Option[(Diamond, OWLObjectProperty, OWLObjectProperty)] = {
      val ax = tax.axiom
      if (ax.getSubProperty.isInstanceOf[OWLObjectProperty] && ax.getSuperProperty.isInstanceOf[OWLObjectProperty]) {
        val sup = ax.getSuperProperty.asOWLObjectProperty()
        val sub = ax.getSubProperty.asOWLObjectProperty()
        val d = tax.operator
        if (sup.isOWLTopObjectProperty) None
        else Some((d, sup, sub))
      } else None
    }

    logger.debug("Creating roles hierarchy")

    var rstmts: mutable.MutableList[(Diamond, OWLObjectProperty, OWLObjectProperty)] = mutable.MutableList.empty
    i = 1
    roleAxioms.foreach { el =>
      g(el) match {
        case Some(v) => {
          i += 1
          rstmts.+:=(v)
          if (i % 1000 == 0) {
            logger.debug(s"$i elements processed, saving to DB")
            manager.run(DBIO.seq(subRoles ++= rstmts))
            rstmts.clear()
          }
        }
        case None =>
      }
    }
    manager.run(DBIO.seq(subRoles ++= rstmts))
    rstmts.clear()
    logger.debug(s"Added subrole assertions to the hierarchy.")

    /*val rolesList: Seq[(String)] = manager.withDatabase(db => db.run((for {c <- roles} yield (c.iri)).result))
    for ((iri) <- rolesList) {
      val c = ObjectProperty(iri)
      val stmts = for (sc <- helper.querySubRoles(c, false);
                       if iri != sc) yield {
        val d = tax.operator.toDB
        subRoles += (iri, sc)
      }
      manager.run(DBIO.seq(stmts: _*))
    }*/
  }

  def saveAssertions(helper: OntologyHelper) = {
    logger.info("Saving assertions to database")
    manager.run(conceptAssertions.schema.truncate)
    manager.run(roleAssertions.schema.truncate)
    appendAssertionsToDatabase(helper)
  }

  def time[R](block: => R, task: String): R = {
    val t0 = System.currentTimeMillis()
    val result = block    // call-by-name
    val t1 = System.currentTimeMillis()
    logger.info(s"Elapsed time for $task: ${((t1 - t0)/1000)}s")
    result
  }

  def forWithProgress[A,R](col: Iterable[A], task: String)(block: A => R): Traversable[R] = {
    logger.debug(s"Starting $task with progress monitoring:")
    val total_size = col.size
    val t0 = System.currentTimeMillis()
    val results:mutable.MutableList[R] = mutable.MutableList()

    for ((c,i) <- col.zipWithIndex) {
      val tim1 = System.currentTimeMillis()
      results += block(c)
      val ti = System.currentTimeMillis()
      val time_needed = (ti - tim1).toDouble
      val average_duration = (ti - t0) / (i+1)
      val total_estimated_duration = average_duration * total_size
      val completion_estimation = Instant.ofEpochMilli(t0 + total_estimated_duration).atZone(ZoneId.systemDefault())
      val percent_done = (i+1).toDouble / total_size * 100
      logger.debug(s"$task:")
      logger.debug(s"${i+1}/$total_size completed; AVG: ${average_duration / 1000}s; ETC: $completion_estimation")
    }
    return results.toList

  }

  def saveToDatabase(): Unit = {
    time(setupToDatabase(helper),"DB Setup")
    time(saveAnnotations(helper), "Saving of annotations")
    time(appendTimepointsToDatabase(helper), "Saving of time points")
    time(saveAssertions(helper), "Saving of Assertions")
    time(saveHierarchy(helper), "Saving of Subsumptions")
  }


  override def pShow: String = throw new NotImplementedError("Please implement pShow")

  def s2Ind(s: String): OWLNamedIndividual = NamedIndividual(s)



  override def lookup(cls: OWLClass): Traversable[OWLNamedIndividual] = {
    val extTp:ExtTimePoint = Point(timePoint)
    val q = for {
      c <- concepts; if c.iri === cls
      t <- timepoints; if t.from <= extTp && t.to >= extTp
      a <- conceptAssertions; if a.conceptiri === c.iri && a.timepoint_id === t.id
      i <- individuals; if i.iri === a.indid
    } yield i.iri
    manager.withDatabase(db => db.run(q.result))
  }

  override def lookup(role: OWLObjectProperty): Traversable[(OWLNamedIndividual, OWLNamedIndividual)] = {
    val extTp:ExtTimePoint = Point(timePoint)
    val q = for {
      r <- roles; if r.iri === role
      t <- timepoints; if t.from <= extTp && t.to >= extTp
      a <- roleAssertions; if a.roleiri === r.iri && a.timepoint_id === t.id
      i1 <- individuals; if i1.iri === a.ind1id
      i2 <- individuals; if i2.iri === a.ind2id
    } yield (i1.iri, i2.iri)
    manager.withDatabase(db => db.run(q.result))
  }

  def lookup(cls: OWLClass, tp: TimePoint): Traversable[OWLNamedIndividual] = {
    timePoint = getRepresentative(tp)
    lookup(cls)
  }

  def lookup(role: OWLObjectProperty, tp: TimePoint): Traversable[(OWLNamedIndividual,OWLNamedIndividual)] = {
    timePoint = getRepresentative(tp)
    lookup(role)
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

  def getRepresentative(tp:TimePoint): TimePoint = {
    val extTp:ExtTimePoint = Point(tp)
    val q = for {t <- timepoints; if t.from <= extTp && t.to >= extTp} yield t.timepoint
    manager.withDatabase(_.run(q.result.head))
  }

  def getTimePoints(): Traversable[TimePoint] = {
    val q = for {t <- timepoints} yield t.timepoint
    manager.withDatabase(_.run(q.result))
  }

  def getRealTimePoints(): Traversable[TimePoint] = {
    val q = for {
      t <- timepoints; if t.real === true
    } yield t.timepoint
    manager.withDatabase(_.run(q.result))
  }

  def getVirtualTimePoints(): Traversable[TimePoint] = {
    val q = for {
      t <- timepoints; if t.real === false
    } yield t.timepoint
    manager.withDatabase(_.run(q.result))
  }


  def getTimePointID(tp: TimePoint): Long = {
    val q = for {t <- timepoints; if t.timepoint === tp} yield t.id
    //logger.debug(q.result.statements.toString())
    manager.withDatabase(_.run(q.result.head))
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

  def isSatisfied(cls: OWLClass, term: OWLNamedIndividual): Boolean = {
    val q = for {
      t <- timepoints; if t.timepoint === timePoint
      a <- conceptAssertions; if a.indid === term && a.conceptiri === cls && a.timepoint_id === t.id
    } yield a.indid
    manager.withDatabase(db => db.run(q.exists.result))
  }

  def isSatisfied(role: OWLObjectProperty, term1: OWLNamedIndividual, term2: OWLNamedIndividual): Boolean = {
    val q = for {
      t <- timepoints; if t.timepoint === timePoint
      a <- roleAssertions; if a.ind1id === term1 && a.ind2id === term2 && a.roleiri === role && a.timepoint_id === t.id
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
      val c = ontology.getClassesInSignature(Imports.INCLUDED).asScala.+(OWLThing, OWLNothing).size
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

  def getAnswers(fi: Query, tp: TimePoint): Set[Answer] = {
    timePoint = getRepresentative(tp)
    getAnswers(fi)
  }

  def getAnswers(fi: Query): Set[Answer] = {
    if (timePoint == null) {
      throw new NullPointerException("Before computing answers, please set the timepoint in a Temporal Model.")
    }
    var query = ""
    var answerVars: List[String] = List.empty
    var temporalVars: List[String] = List.empty
    fi match {
      case f1: FOQuery => {
        val f = f1.simplify()
        answerVars = f.getAnswerVars().map(_.name).toList
        query = FOQueryTranslator.toSQLStatement(f).toSQL()
      }
      case f1: TemporalFOQuery => {
        val tVar = "t"
        val f = f1.simplify()
        answerVars = f.getAnswerVars().map(_.name).toList
        query = TemporalFOQueryTranslator.toTemporalSQLStatement(f)(helper).toTemporalSQL(timePoint)
      }
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
          logger.error(fi.pShow())
          logger.error("SQL QUERY:\n" + query + "\n")
          throw e
        }
      }
    }
  }
  /*
  private def subClassQuery(operator: Rep[Option[String]], interval: Rep[Option[Long]], cls: Rep[String]) = for
    (c <- subConcepts; if c.conceptiri === cls && c.operator === operator && c.interval === interval)
    yield (c.subconceptiri)

  val subclassQueryCompiled = Compiled(subClassQuery _)

  // TODO: With the diamonds we need to be careful when querying..
  def querySubClasses(query: TemporalClass, strict: Boolean, omitTop: Boolean, direct: Boolean): Set[OWLClass] = {
    val (o, i) = query.operator.toDB
    val clsT:Set[String] = direct match {
      case true => {
        manager.run(subclassQueryCompiled((o, i, query.cls.toDB)).result).toSet
      }
      case false => {
        querySubClassesRecursive(query)
      }
    }

    var cls = clsT.map(Class(_))
    if (omitTop) cls = cls.filter(!_.isOWLThing)
    if (strict) cls = cls.diff(queryEquivalentClasses(query))
    else cls = cls.+(query.cls)
    return cls
  }



  private def querySubClassesRecursive(query:TemporalClass): Set[String] = {
    val (o, i) = query.operator.toDB
    val r: Set[String] = manager.run(subclassQueryCompiled((o,i,query.cls.toDB)).result).toSet
    r++(r.flatMap(s => querySubClassesRecursive(TemporalClass(NoDiamond(),new OWLClassImpl(new IRI(s))))))
  }

  def queryEquivalentClasses(query: TemporalClass): Set[OWLClass] = {
    val q = for
      { c <- subConcepts if c.conceptiri === query.cls.toDB
        sr <- subConcepts if sr.conceptiri === c.subconceptiri && sr.subconceptiri === c.conceptiri }
      yield (c.subconceptiri)
    manager.run(q.result).map(Class(_)).toSet

    //val subCls = querySubClasses(cls, false, false, true)
    //subCls.filter(a => querySubClasses(a, false, false, true).contains(cls))
  }

  override def querySuperClasses(query: OWLClass, strict: Boolean): Set[OWLClass] = throw new NotImplementedError()

  private def subRolesQuery(role: Rep[String]) = for
    (c <- subRoles; if c.roleiri === role)
    yield (c.subroleiri)

  val subRolesQueryCompiled = Compiled(subRolesQuery _)

  override def querySubRoles(query: OWLObjectProperty, strict: Boolean): Set[OWLObjectProperty] = {
    val clsT:Set[String] = querySubRolesRecursive(List(query.toDB))

    var cls = clsT.map(ObjectProperty(_))
    if (strict) cls = cls.diff(queryEquivalentRoles(query))
    else cls = cls.+(query)
    return cls
  }

  private def querySubRolesRecursive(queue: List[String], processed: Set[String] = Set()): Set[String] = {
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
      { c <- subRoles if c.roleiri === query.toDB
        sr <- subRoles if sr.roleiri === c.subroleiri && sr.subroleiri === c.roleiri }
      yield (c.subroleiri)
    manager.run(q.result).map(ObjectProperty(_)).toSet + query
    //val subRoles = querySubRoles(query, false)
    //subRoles.filter(a => querySubRoles(a, false).contains(query))
  }
  */
  override def querySubClasses(query: OWLClass, strict: Boolean, omitTop: Boolean, direct: Boolean): Set[OWLClass] = ???

  override def querySuperClasses(query: OWLClass, strict: Boolean): Set[OWLClass] = ???

  override def queryEquivalentClasses(cls: OWLClass): Set[OWLClass] = ???

  override def querySubRoles(query: OWLObjectProperty, strict: Boolean): Set[OWLObjectProperty] = ???

  override def querySuperRoles(query: OWLObjectProperty, strict: Boolean): Set[OWLObjectProperty] = ???

  override def queryEquivalentRoles(query: OWLObjectProperty): Set[OWLObjectProperty] = ???
}


