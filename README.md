# upgqsl

A Clojure library designed to upgrade java code written in querydsl 2.0 to querydsl 4.0.

## Usage

mkdir yyyy

uqgqsl.sh <dir-to-java-src>

The converted java source code should be found in yyyy/dir-to-java-src. You can inspect the generated code for correctness. After that you can use the generated code to override your source code.
You can override all your source codes with one sh command:

cd yyyy

find <dir-to-java-src> -name "*.java" | xargs -i mv {} ../{}

## License

Copyright © 2017 FIXME

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
