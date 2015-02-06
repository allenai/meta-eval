.PHONY: reports

stage:
	sbt stage

conf: reports
	cat src/main/resources/application.conf

evalGrobid:
	target/universal/stage/bin/meta-eval evalGrobid

evalMetatagger:
	target/universal/stage/bin/meta-eval evalMetatagger

runGrobid: 
	target/universal/stage/bin/meta-eval runGrobid

runMetatagger:
	target/universal/stage/bin/meta-eval runMetatagger

runPstotext:
	target/universal/stage/bin/meta-eval runPstotext

