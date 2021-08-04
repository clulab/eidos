# org.clulab.wm.eidos.apps.batch

Files here are involved with batch processing of large document collections, usually the kinds related to Two Six's DART system and its CDR files.

* DiffDirs - Take two directories of files and say which files are in the one but not the other.
* DiffFiles - Take two lists of files, each in a file itself, and say which files are in one list but not the other.
* ExtractCdrMetaFromDirectory - Read all the CDR files in an input directory with Eidos and write jsonld to the output directory.
* ExtractCdrProcOnlyFromDirectory - Process all the CDR files in an input directory with Processors and write json to the output directory.
* ExtractCluFromDirectoryFiltered - Bin a very large set of files by file size and then processes the bins, each in parallel, starting from the bins of the smallest files to the bins of largest files.
* ExtractCluMetaFromDirectory - Read files with CLU metadata which is stored separately from the text.
* ExtractCluMetaFromDirectoryFiltered - Combine some of the above options and read files with CLU metadata, but by sorting into bins and reading the short files first.
* ExtractCluMetaFromDirectoryWithId - Read for an exercise in which an id was used to map between text files and metadata files.
* Fewsnet - Processs the fewsnet document collection.
* FilterCluDateFromDirectory - Extract, i.e., print, the document dates from files that include CLU metadata.
* FilterCluKeysFromDirectory - Extract, i.e., print, all the CLU metadata fields.
* FilterWithMetaFromDirectory - Processes only the documents within a certain date range.
* FindMissingFiles - Figure out which files were missing from bins.
* ReExtractCdrMetaFromDirectory - Read in CDR files and serialize the jsonld in memory, then deserialize and reserialize them to find out whether the two serializations are identical.
* RegroundCdrMetaFromDirectory - Read in jsonld files, rather than CDRs, reground, and then reserialize.  This is much faster than reading the CDRs from scratch.
* RenameMetaFiles - Convert file names between naming conventions used long ago.
* SeparateCdrTextFromDirectory - Separate out the text from CDR files so that programs that run on plain text files can use the CDR corpora.
