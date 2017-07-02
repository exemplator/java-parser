java-parser
=============

[![BSD](http://b.repl.ca/v1/license-BSD-blue.png)](http://en.wikipedia.org/wiki/BSD_licenses)
[![Haskell](http://b.repl.ca/v1/language-haskell-lightgrey.png)](http://haskell.org)

Haskell parser and pretty printer for the java language.
Based on [language-java](https://github.com/vincenthz), for which we are thankful, this project is expected to diverge while we add new features that are needed for [java-usage-finder](https://github.com/exemplator/java8-usage-finder).


How to use
----------

Simple compilation unit parser:

    parser compilationUnit "import java.util.*; public class MyClass {}"

or from a file:

    ast <- parser compilationUnit `fmap` readFile "myClass.java"
