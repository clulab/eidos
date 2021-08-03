# org.clulab.wm.eidos.apps.cache

Some kinds of resources can be preprocessed and then cached so that Eidos components can be loaded more quickly, and the programs to do that are located here.

* CacheGeonames - This caches the geonames by extracting them from the jar file included as a library dependency.
* CacheOntologies - This reads the ontologies from their jar files and additionally pre-processes them and caches the result in a quickly readable format.
