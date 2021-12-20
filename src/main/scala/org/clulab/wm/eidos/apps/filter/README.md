# org.clulab.wm.eidos.apps.filter

This are similar to the extractions, but the output is generally not jsonld but some sort of filtered version of it.  For example, perhaps just the canonical names are output.

* FilterJson - Read json files (serialized Documents) and extract information, most recently the geolocations.
* FilterJsonCanonicalNames - In this case extract the canonical names.
* FilterJsonExtractions - In this case extract the extractions themselves.
* FilterJsonGeoAndTime - Extract both the geolocations and times.
* FilterJsonLigatures - Check the text for certain kinds of ligatures that were causing problems.
* FilterJsonPretty - Reformat the json to make it pretty and more legible to humans.
* FilterJsonSource - Extract the _source field from json in order to convert between early CDR-like formats related to a short-lived elastic search implementation.
* FilterJsonText - Extract the text from CDRs for programs that work with straight text.
