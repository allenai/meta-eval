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


