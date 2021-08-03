# org.clulab.wm.eidos.apps

This package contains numerous collections of files which are all executable in that they include an object that inherits from App or otherwise provides a static main() method so that they can be run.  The package is slated for conversion into a subproject.  The collections are briefly described here but each should have its own README.md file describing the individual apps.

* batch - Files here are involved with batch processing of large document collections, usually the kinds related to Two Six's DART system and its CDR files.
* cache - Some kinds of resources can be preprocessed and then cached so that Eidos components can be loaded more quickly, and the programs to do that are located here.
* eval - Various components of Eidos need to be evaluated occasionally and these program do that.
* examples - Files are placed here because they are used in the documentation and should not be changed without coordination with that documentation.
* extract - These programs extract mentions from data in various formats.  Several are used in tandem with exporters which are collected in their own package.
* filter - This are similar to the extractions, but the output is generally not jsonld but some sort of filtered version of it.  For example, perhaps just the canonical names are output.
* groundings - Here is a small collection of apps dealing with grounding.
* misc - These didn't fit anywhere else.
* reconstitute - This is a collection of apps that start with deserialization of documents already read.
* shell - This includes the EidosShell and also a template for creating new shells.
* xsv - Apps primarily producing CSV and TSV output are collected here.
