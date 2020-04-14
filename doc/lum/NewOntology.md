1. Download examples and patterns.
1. Place them in separate folders near the Eidos project like ../icm/examples and ../icm/patterns.
1. Change TableDomainOntology.main to use the ../icm directory and file icm.tbl and call convertFromFilesToResource.
1. Run TableDomainOntology and account for any assertions.
1. Take icm.tbl and place in resources where causeex ontologies go like src\main\resources\org\clulab\causeex\eidos\english\ontologies.
1. In causeex.conf add icm to ontologies in  and record icm = ${ontologies.path}/icm.tbl.
1. Add ICM_NAMESPACE to DomainOntologies.scala
1. Route ICM_NAMESPACE to TableDomainOntologyBuilder in DomainOntologies.

Ontology should now be usable.  One thing to use it for is in recreating a vector
file which will be certain to use the words contained in the new ontology.

1. Change CullVectors to use new ontology and run it.  Lots of extra files are required.
1. Take resulting vector file and put it in src/main/resources/org/clulab/causeex/eidos/english/w2v.
1. Make sure causeex.conf is configured to use the file with the correct wordToVecPath.
1. Turn off useCacheForW2V if it isn't.
1. Run CacheOntologies to get the new, cached vector file in the cache directory.
1. Change useCacheForW2V back on.
1. Make vector file accessible for others by placing on google drive.
