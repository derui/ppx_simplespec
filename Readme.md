# What this? #
This library helps you to write test with annotation-based DSL.

## Install ##

You have to install OMake before install Simplespec, then do steps below to install!

1. `git clone https://github.com/derui/simplespec.git`
2. `omake install`
3. You can use Simplespec with ocamlfind.

### Usage ###

Simplespec providing ppx extension is used to with -ppx flag for ocamlc/ocamlopt

```
ocamlfind ocamlc -package ppx_simplespec -linkpkg test_using_simplespec.ml
```

Notice, you must add a option `-linkpkg` to compile source using ppx_simplespec. Therefore you get compile error if you forget it.

### Assertion annotations and A extension ###
ppx_simplespec provide some annotations to write assertion and a extension to write unit test via OUnit with ppx.

#### spec extension ####
**spec extension** is add function to use ppx_simplespec annotations in it.

It is only place to expression in source, DSL sample below.

```
let%spec "spec name" =
  ...
```

Or you can write below.

```
[%spec "spec name"
  ...
]
```

I recommend to use first sample when you write unit test via ppx_simplespec, syntax using it is popular syntax to use extension.

#### Assertion annotations ####
**Assertion annotations** are add assertion to some expression. There are wraps OUnit's assertions.

##### Boolean assertions #####

*Boolean assertions* are wraps OUnit's `assert_true` and `assert_false`.

```
List.for_all ((=) true) [true] [@true]

or

List.for_all ((=) true) [true] [@true "All elements are true"]
```

```
List.for_all ((=) true) [false] [@false]

or

List.for_all ((=) true) [false] [@false "Contain false element"]
```

##### Equal assertions #####
*Equal assertions* are wraps OUnit's `assert_equal`, and define negate assertion.

```
(* Straight forward OUnit2.assert_equal *)
1 + 2 [@eq 3]

(* not equal assertion *)
1 + 3 [@ne 3]
```

##### Exception assertion #####
*Exception assertion* is wrap OUnit's `assert_raises`.

```
(* Straight forward OUnit2.assert_raises *)
List.find ((=) 0) [1;2;3] [@raises Not_found]
```
