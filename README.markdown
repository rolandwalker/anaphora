[![Build Status](https://secure.travis-ci.org/rolandwalker/anaphora.png?branch=master)](http://travis-ci.org/rolandwalker/anaphora)

# Overview

Anaphoric expressions for Emacs Lisp, providing implicit temporary variables.

 * [Quickstart](#quickstart)
 * [anaphora](#anaphora)
 * [See Also](#see-also)
 * [Notes](#notes)
 * [Compatibility and Requirements](#compatibility-and-requirements)

## Quickstart

```elisp
(require 'anaphora)
 
(awhen (big-long-calculation)
  (foo it)      ; `it` is provided as
  (bar it))     ; a temporary variable
 
;; anonymous function to compute factorial using `self`
(alambda (x) (if (= x 0) 1 (* x (self (1- x)))))
```

## anaphora

Anaphoric expressions implicitly create one or more temporary
variables which can be referred to during the expression.  This
technique can improve clarity in certain cases.  It also enables
recursion for anonymous functions.

To use anaphora, place the `anaphora.el` library somewhere
Emacs can find it, and add the following to your `~/.emacs` file:

```elisp
(require 'anaphora)
```

The following macros are made available

	aand
	ablock
	acase
	acond
	aecase
	aetypecase
	aif
	alambda
	alet
	aprog1
	aprog2
	atypecase
	awhen
	awhile
	a+
	a-
	a*
	a/

The following macros are experimental

	anaphoric-set
	anaphoric-setq
	
## Macro rundown...

### `anaphoric-if` aka. `aif`

Like `if`, but the result of evaluating `COND` is bound to `it`.

The variable `it` is available within `THEN` and `ELSE`.

`COND`, `THEN`, and `ELSE` are otherwise as documented for `if`.

### `anaphoric-prog1` aka. `aprog1`

Like `prog1`, but the result of evaluating `FIRST` is bound to `it`.

The variable `it` is available within `BODY`.

`FIRST` and `BODY` are otherwise as documented for `prog1`.

The final return value of `aprog1` is `it` (ie. `FIRST`).

### `anaphoric-prog2` aka. `aprog2`

Like `prog2`, but the result of evaluating `FORM2` is bound to `it`.

The variable `it` is available within `BODY`.

`FORM1`, `FORM2`, and `BODY` are otherwise as documented for `prog2`.

The final return value of `aprog2` is `it` (ie. `FORM2`).

### `anaphoric-when` aka. `awhen`

Like `when`, but the result of evaluating `COND` is bound to `it`.

The variable `it` is available within `BODY`.

`COND` and `BODY` are otherwise as documented for `when`.

### `anaphoric-while` aka. `awhile`

Like `while`, but the result of evaluating `TEST` is bound to `it`.

The variable `it` is available within `BODY`.

`TEST` and `BODY` are otherwise as documented for `while`.

### `anaphoric-and` aka. `aand`

Like `and`, but the result of the previous condition is bound to `it`.

The variable `it` is available within all `CONDITIONS` after the
initial one.

`CONDITIONS` are otherwise as documented for `and`.

Note that some implementations of this macro bind only the first
condition to `it`, rather than each successive condition.

### `anaphoric-cond` aka. `acond`

Like `cond`, but the result of each condition is bound to `it`.

The variable `it` is available within the remainder of each of `CLAUSES`.

`CLAUSES` are otherwise as documented for `cond`.

### `anaphoric-lambda` aka. `alambda`

Like `lambda`, but the function may refer to itself as `self`.

`ARGS` and `BODY` are otherwise as documented for `lambda`.

### `anaphoric-block` aka. `ablock`

Like `block`, but the result of the previous expression is bound to `it`.

The variable `it` is available within all expressions of BODY
except the initial one.

`NAME` and `BODY` are otherwise as documented for `block`.

### `anaphoric-case` aka. `acase`

Like `case`, but the result of evaluating `EXPR` is bound to `it`.

The variable `it` is available within `CLAUSES`.

`EXPR` and `CLAUSES` are otherwise as documented for `case`.

### `anaphoric-ecase` aka. `aecase`

Like `ecase`, but the result of evaluating `EXPR` is bound to `it`.

The variable `it` is available within `CLAUSES`.

`EXPR` and `CLAUSES` are otherwise as documented for `ecase`.

### `anaphoric-typecase` aka. `atypecase`

Like `typecase`, but the result of evaluating `EXPR` is bound to `it`.

The variable `it` is available within `CLAUSES`.

`EXPR` and `CLAUSES` are otherwise as documented for `typecase`.

### `anaphoric-etypecase` aka. `aetypecase`

Like `etypecase`, but result of evaluating `EXPR` is bound to `it`.

The variable `it` is available within `CLAUSES`.

`EXPR` and `CLAUSES` are otherwise as documented for `etypecase`.

### `anaphoric-let` aka. `alet`

Like `let`, but the content of `VARLIST` is bound to `it`.

`VARLIST` as it appears in `it` is not evaluated.  The variable `it`
is available within `BODY`.

`VARLIST` and `BODY` are otherwise as documented for `let`.

### `anaphoric-+` aka. `a+`

Like `+`, but the result of evaluating the previous expression is bound to `it`.

The variable `it` is available within all expressions after the
initial one.

`NUMBERS-OR-MARKERS` are otherwise as documented for `+`.

### `anaphoric--` aka. `a-`

Like `-`, but the result of evaluating the previous expression is bound to `it`.

The variable `it` is available within all expressions after the
initial one.

`NUMBER-OR-MARKER` and `NUMBERS-OR-MARKERS` are otherwise as
documented for `-`.

### `anaphoric-*` aka. `a*`

Like `*`, but the result of evaluating the previous expression is bound to `it`.

The variable `it` is available within all expressions after the
initial one.

`NUMBERS-OR-MARKERS` are otherwise as documented for `*`.

### `anaphoric-/` aka. `a/`

Like `/`, but the result of evaluating the previous divisor is bound to `it`.

The variable `it` is available within all expressions after the
first divisor.

`DIVIDEND`, `DIVISOR`, and `DIVISORS` are otherwise as documented for `/`.

### `anaphoric-set`

**EXPERIMENTAL**

Like `set`, except that the value of `SYMBOL` is bound to `it`.

The variable `it` is available within `VALUE`.

`SYMBOL` and `VALUE` are otherwise as documented for `set`.

Note that if this macro followed traditional naming for
anaphoric expressions, it would conflict with the existing
(quite different) function `aset`.

### `anaphoric-setq`

**EXPERIMENTAL**

Like `setq`, except that the value of `SYM` is bound to `it`.

The variable `it` is available within each `VAL`.

`ARGS` in the form `[SYM VAL]` ... are otherwise as documented for `setq`.

No alias `asetq` is provided, because it would be easily mistaken
for the pre-existing `aset`, and because `anaphoric-setq` is not
likely to find frequent use.


## See Also

 * <http://en.wikipedia.org/wiki/Anaphoric_macro>

## Notes

Partially based on examples from the book "On Lisp", by Paul Graham.

When this library is loaded, the provided anaphoric forms are
registered as keywords in font-lock. This may be disabled via
`customize`.

## Compatibility and Requirements

	GNU Emacs version 24.4-devel     : yes, except macros marked experimental
	GNU Emacs version 24.3           : yes, except macros marked experimental
	GNU Emacs version 23.3           : yes, except macros marked experimental
	GNU Emacs version 22.2           : yes, except macros marked experimental
	GNU Emacs version 21.x and lower : unknown

No external dependencies
