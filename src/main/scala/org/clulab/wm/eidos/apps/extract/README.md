# org.clulab.wm.eidos.apps.extract

These programs extract mentions from data in various formats.  Several are used in tandem with exporters which are collected in their own package.

* ExtractAndExport - Input all the files in a directory and export them with the Exporter that can be configured via the conf file (apps.conf) to produce different formats. 
* ExtractDocumentFromDirectory - This inputs files in various formats based on command line specifications and outputs jsonld.
* ExtractfromDirectory - Input text files from a directory and output jsonld files.
* ExtractFromFile - Input text files, but output some specific grounding values.
* ExtractFromText - Extract from a string variable into a string and print the result.
* ExtractJsonLDFromFile - Input text from a single file and output a jsonld file.
* ExtractSemanticRoles - Exercise the processor by annotating forever, expecting a crash eventually. 
