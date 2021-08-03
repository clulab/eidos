# org.clulab.wm.eidos.apps.reconstitute

This is a collection of apps that start with deserialization of documents already read.

* ReconstituteAndExport - Export as is done elsewhere with an Exporter, but start with a jsonld file.
* ReconstituteCombineAndExport - Include a PrintWriter in the Exporter so that exported values can be combined when output.
* RoundTrip - Read text from a file, serialize it once, deserialize it and serialize it again, and compare the two serializations.
