#!/bin/bash

OUTPUT=yyyy
ENCODING="UTF-8"

find $1 -name "*.java" |
grep -v "src/test" |
grep -v "/target/" |
sed "s|^\(.*\/\)\([^\/]*\.java\)$|mkdir -p $OUTPUT/\1 \&\& java -jar ~/Development/git/AIC4.5/upgqsl/target/upgqsl-0.1.0-SNAPSHOT-standalone.jar -e ${ENCODING} \1\2 $OUTPUT/\1\2|" |
xargs -i sh -c {}
