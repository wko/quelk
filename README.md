# QUELK - A Reasoner for Temporal Ontology Mediated Query Answering in OWL-EL

This is the source code accompanying my Dissertation. 
QUELK is a prototypical implementation of temporal question answering over temporalized OWL-EL ontologies. The theory is contained in [[1]](#1). 
 
Disclaimer: The source code is not well documented and might therefore be difficult to understand.  

## Build & Run in Docker

The easiest way to run the web interface is to use docker-compose: 

    docker-compose build
    docker-compose up
    

The website should then be available under: ```localhost:8080```.

Alternatively you can use the Dockerfile directly: 

1. ```docker build --tag temporal-rewriter .```  
2. ```docker run -i -v /path/to/your/ontologies:/root/data/ontologies -p 8080:8080 temporal-rewriter```
3. Go to your web browser to ```localhost:8080```.

## Ontology Format 

Ontologies have to be in OWL functional style syntax.
Temporal operators can be expressed by annotations on the axioms.
 
An example file can be found in ```data/ontologies/TemporalTests.ofn``` 

 

## Temporal Queries

Queries have to be provided in the following format


## References 

<a id="1">[1]</a> 
Stefan Borgwardt, Walter Forkel, and Alisa Kovtunova: ‘Finding New Diamonds: Temporal Minimal- World Query Answering over Sparse ABoxes’. In Proc. of the 3rd International Joint Conference on Rules and Reasoning (RuleML+RR’19). Edited by Paul Foder, Marco Montali, Diego Calvanese, and Dumitru Roman. Bolzano, Italy: Springer, 2019 
<doi:10.1007/978-3-030-31095-0_1>