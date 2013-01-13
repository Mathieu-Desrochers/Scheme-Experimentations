scheme-experimentations : jansson

jansson : json.o json-format.o json-parse.o

json.o : sources/infrastructure/json/json.scm
	csc -c sources/infrastructure/json/json.scm

json-format.o : sources/infrastructure/json/json-format.scm
	csc -c sources/infrastructure/json/json-format.scm

json-parse.o : sources/infrastructure/json/json-parse.scm
	csc -c sources/infrastructure/json/json-parse.scm

