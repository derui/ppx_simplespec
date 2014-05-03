# What this? #
This library helps you to write test on OCaml that is like BDD style.

Simplespec is inspired by [OSpec](https://github.com/andrenth/ospec0).

Install
-------
You have to install OMake before install Simplespec, then do steps below to install!

1. `git clone https://github.com/derui/simplespec.git`
2. `omake install`
3. You can use Simplespec with ocamlfind.

Usage
=====
Simplespec having a command line inteface named **simplespec** is install into your ocamlfind path after install simplespec.

You type following command, do run spec and print result of test.
( **simplespec** command must be contained in your PATH)
```
simplespec test_using_simplespec.ml
```

If your test have some dependencies, for instance str, bigarray, or other installed package via ocamlfind,
**simplespec** command can load these and run spec.
```
simplespec -package str,bigarray test_using_simplespec.ml
```

Simplespec library include Camlp4 syntax, then you want to compile and run test including C interface program, you can add simplespec and use simplespec syntax.
```
ocamlfind ocamlc -package simplespec, simplespec.syntax -syntax camlp4o ...
```
But you choise to use simplespec with ocamlfind, you have to write running spec and priting result by yourself.


