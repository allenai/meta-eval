# Meta-Eval

This is a tool to evaluate performance of various (research papers') metadata extraction
libraries (see for example [this article]((https://medium.com/@vha14/extracting-metadata-from-research-articles-metatagger-versus-grobid-9249e4364734) for background). It has been verified to run on Mac OS X and Linux Ubuntu.

Currently, two libraries are supported:

- [Grobid](https://github.com/kermitt2/grobid) by Patrice Lopez.
- [Metatagger](https://github.com/iesl/rexa1-metatagger) by the University of Massachusetts's Information Extraction and Synthesis Laboratory led by Andrew McCallum. This library uses a custom PDF processor called [pstotext](https://github.com/iesl/rexa1-pstotext) which is based on DEC library of the same name.

This tool currently supports using the [ACL Anthology](http://www.aclweb.org/anthology/), which contains about twenty thousand paper PDFs (as of 2014), together with the metadata provided by the the University of Michigan's Computational Linguistics and Information Retrieval led by Dragomir Radev.

For a comparison of Grobid and Metatagger on the ACL dataset, please read [this article] (https://medium.com/@vha14/extracting-metadata-from-research-articles-metatagger-versus-grobid-9249e4364734) on Medium.

## User Guide

Below is the guide to set up and run meta-eval on your own machine. We are in the process of creating an Amazon AMI that is ready to use. 

### Requirements

To run this evaluation tool, the following are needed:

- Mac OS X or Linux. Linux Ubuntu has been tested.
- Java 7+ runtime.
- Scala 2.11 or later.
- Git.
- Simple build tool (sbt), version 0.13.7 or later.
- Maven (for building Grobid). 
- Ghostscript (for building pstotext).

### Setting Up Software

- Create a directory called `eval`, [for example](https://github.com/allenai/meta-eval/blob/master/src/main/resources/application.conf#L1), in `$HOME` directory. 
- Git clone the following four repositories: [Grobid](https://github.com/kermitt2/grobid), [Metatagger](https://github.com/iesl/rexa1-metatagger), [pstotext](https://github.com/iesl/rexa1-pstotext), and [Meta-Eval](https://github.com/allenai/meta-eval) in `eval`. 
- Build Grobid with `mvn install` in Grobid's home. This should produce the Grobid jar, for example `eval/grobid/grobid-core/target/grobid-core-0.3.1-SNAPSHOT.one-jar.jar`. Note that Grobid's version may be different. 
- Build Metatagger with `./sbt compile` in Metatagger's home.
- Build pstotext with `bin/setup` in pstotext's home. 
- Buidl Meta-Eval with `make stage` in Meta-Eval's home. This will call SBT to produce the necessary binaries in `meta-eval/universal/stage`.

### Download and Setting Up Evaluation Data

- Download the compressed [ACL dataset's PDFs](https://s3-us-west-2.amazonaws.com/ai2-s2/pipeline/source-data/acl-pdf/acl-pdf-2014-08-27.zip) (warning: about 7.2GB) from Amazon S3. Extract the PDFs to `~/eval/data/acl/pdfs/' folder.
- Download the [ACL metadata](https://s3-us-west-2.amazonaws.com/ai2-s2/pipeline/source-data/metadata-2014-08-25.json) (about 4.6M) from Amazon S3 to `~/eval/data/acl` folder. 
- Create the following folders in `~/eval/data/acl/extracted`: `pstotext`, `metatagger`, `grobid`.

### Running Meta-Eval

You are now ready to run and evaluate Metatagger and Grobid on the ACL data set using the commands in the Makefile.

```
# This is needed to run 'make conf'.
.PHONY: reports

# Rebuild meta-eval after code change.
stage:
	sbt stage

# Print out the content of meta-eval's current configuration.
conf: reports
	cat src/main/resources/application.conf

# Run Grobid.
runGrobid: 
	target/universal/stage/bin/meta-eval runGrobid

# Evaluate Grobid. It is required to run Grobid first.
evalGrobid:
	target/universal/stage/bin/meta-eval evalGrobid

# Run pstotext. This is required before running Metatagger.
runPsToText:
	target/universal/stage/bin/meta-eval runPsToText

# Run Metatagger.
runMetatagger:
	target/universal/stage/bin/meta-eval runMetatagger

# Evaluate Metatagger. It is required to run Metatagger first.
evalMetatagger:
	target/universal/stage/bin/meta-eval evalMetatagger
```
