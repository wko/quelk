package de.tu_dresden.epistemic_rewriter.test

import de.tu_dresden.epistemic_rewriter.ConfigValues
import org.phenoscape.scowl._

object CancerOntology {
  val Alice = NamedIndividual(s"${ConfigValues.CANCER_ONTOLOGY_PREFIX}Alice")
  val Bob = NamedIndividual(s"${ConfigValues.CANCER_ONTOLOGY_PREFIX}Bob")
  val Anvil = NamedIndividual(s"${ConfigValues.CANCER_ONTOLOGY_PREFIX}Anvil")
  val LungCancer = Class(s"${ConfigValues.CANCER_ONTOLOGY_PREFIX}LungCancer")
  val Human = Class(s"${ConfigValues.CANCER_ONTOLOGY_PREFIX}Human")
  val skinCancerPatient = Class(s"${ConfigValues.CANCER_ONTOLOGY_PREFIX}SkinCancerPatient")

  val diagnosedWith = ObjectProperty(s"${ConfigValues.CANCER_ONTOLOGY_PREFIX}diagnosedWith")
  val dueTo = ObjectProperty(s"${ConfigValues.CANCER_ONTOLOGY_PREFIX}dueTo")
  val causedBy = ObjectProperty(s"${ConfigValues.CANCER_ONTOLOGY_PREFIX}causedBy")
  val SkinCancer = Class(s"${ConfigValues.CANCER_ONTOLOGY_PREFIX}SkinCancer")
  val Cancer = Class(s"${ConfigValues.CANCER_ONTOLOGY_PREFIX}Cancer")
  val PancreasCancer = Class(s"${ConfigValues.CANCER_ONTOLOGY_PREFIX}PancreasCancer")
  val SkinCancerPatient = Class(s"${ConfigValues.CANCER_ONTOLOGY_PREFIX}SkinCancerPatient")
  val PancreasCancerPatient = Class(s"${ConfigValues.CANCER_ONTOLOGY_PREFIX}PancreasCancerPatient")
  val CancerPatient = Class(s"${ConfigValues.CANCER_ONTOLOGY_PREFIX}CancerPatient")
  val BobsCancer = NamedIndividual(s"${ConfigValues.CANCER_ONTOLOGY_PREFIX}BobsCancer")

  private val cancerS:String = """Prefix(:=<cancer#>)
  Prefix(owl:=<http://www.w3.org/2002/07/owl#>)
  Prefix(rdf:=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>)
  Prefix(xml:=<http://www.w3.org/XML/1998/namespace>)
  Prefix(xsd:=<http://www.w3.org/2001/XMLSchema#>)
  Prefix(rdfs:=<http://www.w3.org/2000/01/rdf-schema#>)
  Prefix(time:=<http://www.w3.org/2006/time#>)

  Ontology(<cancer>
    Import(<http://www.w3.org/2006/time#2016>)


    Declaration(Class(:Cancer))
    Declaration(Class(:CancerPatient))
  Declaration(Class(:PancreasCancer))
  Declaration(Class(:PancreasCancerPatient))
  Declaration(Class(:SkinCancer))
  Declaration(Class(:LungCancer))
  Declaration(Class(:SkinCancerPatient))
  Declaration(ObjectProperty(:causedBy))
  Declaration(ObjectProperty(:diagnosedWith))
  Declaration(ObjectProperty(:dueTo))
  Declaration(ObjectProperty(:someThing))
  Declaration(NamedIndividual(:Alice))
  Declaration(NamedIndividual(:Bob))
  Declaration(NamedIndividual(:Anvil))
  Declaration(NamedIndividual(:BobsCancer))
  Declaration(AnnotationProperty(:begin))
  Declaration(AnnotationProperty(:end))
  Declaration(AnnotationProperty(rdfs:datetime))



  AnnotationAssertion(rdfs:label <:CancerPatient> "A patient that is diagnosed with Cancer"@en)

  ############################
  #   Object Properties
    ############################

  # Object Property: :causedBy (:causedBy)

  SubObjectPropertyOf(:causedBy :dueTo)
  SubObjectPropertyOf(:causedBy :someThing)

  # Object Property: :diagnosedWith (:diagnosedWith)

  SubObjectPropertyOf(:diagnosedWith :dueTo)

  # Object Property: :dueTo (:dueTo)

  SubObjectPropertyOf(:dueTo :causedBy)



  ############################
  #   Classes
  ############################

  # Class: :CancerPatient (:CancerPatient)

  EquivalentClasses(:CancerPatient ObjectSomeValuesFrom(:diagnosedWith :Cancer))

  # Class: :PancreasCancer (:PancreasCancer)

  SubClassOf(:PancreasCancer :Cancer)

  # Class: :PancreasCancerPatient (:PancreasCancerPatient)

  SubClassOf(:PancreasCancerPatient ObjectSomeValuesFrom(:diagnosedWith :PancreasCancer))

  # Class: :SkinCancer (:SkinCancer)

  SubClassOf(:SkinCancer :Cancer)

  SubClassOf(:LungCancer :Cancer)

  # Class: :SkinCancerPatient (:SkinCancerPatient)

  SubClassOf(:SkinCancerPatient ObjectSomeValuesFrom(:diagnosedWith :SkinCancer))


  ############################
  #   Named Individuals
    ############################

  # Individual: :Alice (:Alice)


  ClassAssertion(Annotation(time:instant "2011-10-02 18:48:05.123456"^^xsd:dateTime) :SkinCancerPatient :Alice)


  # Individual: :Bob (:Bob)

  ObjectPropertyAssertion(Annotation(time:instant"1994-10-02 18:48:05.123456"^^xsd:dateTime) :diagnosedWith :Bob :BobsCancer)

  # Individual: :BobsCancer (:BobsCancer)

  ClassAssertion(Annotation(time:instant "1994-10-02 18:48:05.123456"^^xsd:dateTime) :PancreasCancer :BobsCancer)

  ClassAssertion(:CancerPatient :Anvil)
  ClassAssertion(:SkinCancerPatient :Anvil)

  SubClassOf(ObjectSomeValuesFrom(:diagnosedWith :PancreasCancer) :PancreasCancerPatient)
  SubClassOf(ObjectSomeValuesFrom(:diagnosedWith :SkinCancer) :SkinCancerPatient)
  )"""

  //val cancer = new ByteArrayInputStream(cancerS.getBytes(StandardCharsets.UTF_8))
}

object RestaurantOntology {
  val BobsDiner = NamedIndividual(s"${ConfigValues.RESTAURANT_ONTOLOGY_PREFIX}BobsDiner")
  val EvesDiner = NamedIndividual(s"${ConfigValues.RESTAURANT_ONTOLOGY_PREFIX}EvesDiner")
  val BobsPizza = NamedIndividual(s"${ConfigValues.RESTAURANT_ONTOLOGY_PREFIX}BobsPizza")
  val EvesPastaMeal = NamedIndividual(s"${ConfigValues.RESTAURANT_ONTOLOGY_PREFIX}EvesPastaMeal")
}
