.PHONY: reports

stage:
	sbt stage

conf: reports
	cat src/main/resources/application.conf

runGrobid: 
	target/universal/stage/bin/meta-eval runGrobid

evalGrobid:
	target/universal/stage/bin/meta-eval evalGrobid

runPsToText:
	target/universal/stage/bin/meta-eval runPsToText

runMetatagger:
	target/universal/stage/bin/meta-eval runMetatagger

evalMetatagger:
	target/universal/stage/bin/meta-eval evalMetatagger


