# QUELK - A Reasoner for Temporal Ontology Mediated Query Answering in OWL-EL

This is the source code accompanying my Dissertation.
QUELK is a prototypical implementation of temporal question answering over temporalized OWL-EL ontologies.
The theory is contained in [[1]](#1) and [[2]](#2).

QUELK offers two different interaction modes:
1. *Interactive Web Browser mode*
2. *Console Tool Reasoner Mode*

## 1. Interactive Mode
### Build & Run in Docker

The easiest way to run the web interface is to use `Docker` docker-compose:
```shell
docker-compose up --build
```

The website is then be available under: ```localhost:8080```.
The ontologies to query have to be available in `./data/ontologies` in OWL Functional Syntax (*.ofn).

## Temporal Ontologies and Queries
### Ontology Format

Ontologies have to be in OWL functional style syntax (https://www.w3.org/TR/owl2-syntax/#Functional-Style_Syntax).
Temporal operators can be expressed by annotations on the axioms.
An example file can be found in ```data/ontologies/TemporalTests.ofn```


### Temporal Queries

Queries have to be provided in the following format


## N2C2 Experiment

The reasoner was used to test patient selection for clinical trials in [[1]](#1) on data from the N2C2 Cohort Selection Task in [[3]](#3).
The ontology `temporal_n2c2.ofn` was generated from the patient records in the N2C2 dataset [[3]](#3) through the following procedure:
In the first step the record of each patient was split by date. For each date, Metamap was used to extract Snomed CT Concepts.
Each concept extracted this way is connected with a `diagnosedWith` role from the patient individual to an object representing the snomed concept at the timepoint 
of the document they were extracted from.
Apart from the ABox assertions the ontology contains two rigid concepts: `Patient` and Diabetes Mellitus (`snomed:73211009`).

For the queries from the N2C2 challenge, the ground truth annotations have been added to the temporal ontology as well using additional concepts.
For each query, an XML file in `./data/queries/` defines the query through several subqueries. Formally, this defines a *metric temporal conjuctive query with negation* (MTNCQ) as defined in [[1]](#1). 

The computation of answers are computed in two steps:
1. The `temporal minimal universal model` of the patient database is constructed and written to a postgres database.
   Depending on the hardware this step can take several hours and requires about 10GB of RAM.
2. The model is then used to answer the queries using a temporal rewriting. 
   The matching patients are saved in XML format in `./data/queries/results`.
   
### Running
Depending on the hardware this can take several hours and requires about 10GB of RAM.
```shell
docker-compose up -d db
export _JAVA_OPTIONS: -Xms1024m -Xmx10G -Xss256m
sbt "runMain de.tu_dresden.epistemic_rewriter.cli.ConsoleTool data/queries/*.xml"
scripts/viewResults.sh data/queries/results data/queries/html
docker-compose down
```
After this step, the results are contained in the XML files in `./data/queries/results` and html pages are generated in `./data/queries/html` for nicer browsing.


## References

<a id="1">[1]</a>
Walter Forkel: Closed-World Semantics for Query Answering in Temporal Description Logics. Doctoral Thesis, Technische Universität Dresden, Dresden, Germany, 2020.
https://nbn-resolving.org/urn:nbn:de:bsz:14-qucosa2-737736 </br>
<a id="2">[2]</a>
Stefan Borgwardt, Walter Forkel, and Alisa Kovtunova: ‘Finding New Diamonds: Temporal Minimal- World Query Answering over Sparse ABoxes’. In Proc. of the 3rd International Joint Conference on Rules and Reasoning (RuleML+RR’19). Edited by Paul Foder, Marco Montali, Diego Calvanese, and Dumitru Roman. Bolzano, Italy: Springer, 2019
<doi:10.1007/978-3-030-31095-0_1> </br>
<a id="3">[3]</a>
Stubbs A, Filannino M, Soysal E, Henry S, Uzuner Ö. Cohort selection for clinical trials: n2c2 2018 shared task track 1. J Am Med Inform Assoc. 2019 Nov 1;26(11):1163-1171. <doi:10.1093/jamia/ocz163>. PMID: 31562516; PMCID: PMC6798568.
https://pubmed.ncbi.nlm.nih.gov/31562516/
