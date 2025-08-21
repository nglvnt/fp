# Lists: theory & practice

## Summary

## Basic theory of lists

A **list** is a linearly ordered collection of items of the same type.

It can contain finite or infinite number of items, however we will consider finite lists only. Lists will be denoted by brackets, with items separated by commas: `[0, 1, 2]` is the list containing the integer numbers 0, 1, and 2 in this order. A list might contain other lists as items, too: `[[0, 1], [2]]` is the list that contains two lists.

A list may *not* contain any element at all, in this case it is called an *empty list*, and it is denoted by `[]`.

The key properties of lists are:

* items of a list must be of the same type: `[0, "zero"]` is not a valid list for us,
* the same item can be contained more than once in a list: `[0]` and `[0, 0]` are different lists,
* the order of items is important: `[0, 1]` and `[1, 0]` are different lists.

The type of a list of items with common type `t` will be denoted by `[t]` or `List(t)`. So `[0, 1, 2, 3]` has type `[Int]`, and `[True, False]` has type `[Bool]`. The empty list `[]` might be considered as having the type `[t]` for all possible types `t`, it has generic type. This property allows the empty list to specialize to the right type: in the list `[[], [0]]`, the empty list has type `[Int]`, while in `[[], [True]]`, it has type `[Bool]`.

When we have a single element `x` (of type `t`), we may form the single-item list `[x]`, which gives the `unit` function from `t` to `[t]`. These single-element lists (together with the empty list) serve as the building blocks of general lists, through the following procedure.

Two lists of the same type can be concatenated together to form a longer, combined list: given lists `xs = [x_0, x_1, ..., x_{n-1}]` and `ys = [y_0, y_1, ..., y_{m-1}]`, their concatenation `xs ++ ys` is the list `[x_0, x_1, ..., x_{n-1}, y_0, y_1, ..., y_{m-1}]`.

List concatenation has the following properties:

* it is *associative*: `(xs ++ ys) ++ zs` is the same list as `xs ++ (ys ++ zs)`,
* it has a left and right *unit*, the empty list: `xs ++ []` and `[] ++ xs` are just the original list `xs`,
* it is *non-commutative*: in general, `xs ++ ys` and `ys ++ xs` will be different lists, e.g. `[0] ++ [1] = [0, 1] != [1, 0] = [1] ++ [0]`,
* it is left and right *cancellative*: if `xs ++ ys = xs ++ zs` resp. `ys ++ xs = zs ++ xs` for lists `xs, ys, zs`, then `ys = zs`.

Concatenation allows us to write a general list `[x_0, x_1, ..., x_{n-1}]` as `[x_0] ++ [x_1] ++ ... ++ [x_{n-1}]` (no need for parentheses as concatenation is associative), so a general list can be formed from single-item lists `[x]`.

This motivates the function `cons` (or `prepend`), that prepends an element to a list: `cons(y, [x_0, x_1, ..., x_{n-1}]) = [y, x_0, x_1, ..., x_{n-1}])`, and can be thought of as an alias for `[y] ++ [x_0, x_1, ..., x_{n-1}]`. The usual infix notation for `cons` is `:`, so we can write `y : xs` for prepending `y` to a list `xs`. Note that a single-item list `[x]` is just `x : []`, so a general list can be written as `x_0 : x_1 : ... : x_{n-1} : []`.

We have chosen to build lists by prepending elements to it, but we could do it by appending elements to the end, this is just a matter of convention.

Concatenation can be generalized to more (or less) than two lists as well: to allow an arbitrary number of inputs lists `xs_0, xs_1, ..., xs_{n-1}`, we organize them into a list of lists `[xs_0, xs_1, ..., xs_{n-1}]` and call as `flatten` the function that gives back the list `xs_0 ++ xs_1 ++ ... ++ xs_{n-1}` as result. Hence `flatten: [[t]] -> [t]` is a function from lists of lists of type `t` to lists of type `t`.

So far we have considered how lists can be composed, let's see decomposition! A general list `[x_0, x_1, ..., x_{n-1}]` might be decomposed in many ways as the concatenation of two smaller lists (e.g. `[0, 1, 2, 3, 4] = [0, 1] ++ [2, 3, 4] = [0, 1, 2] ++ [3, 4]`), however there is only way to decompose it as the concatenation of single-item lists.

There is also only one way to write a list `[x_0, x_1, ..., x_{n-1}]` as an element prepended to a list: `x_0 : [x_1, ..., x_{n-1}]`. The constituents of this decomposition are usually called as the `head` and the `tail` of the list: `head [x_0, x_1, ..., x_{n-1}] = x_0` and `tail [x_0, x_1, ..., x_{n-1}] = [x_1, ..., x_{n-1}]`. There is one subtlety though: how to define `head` and `tail` for the empty list, but we will get back to this question later.

The `cons` and `head/tail` functions are (almost) inverse to each other:

* for an element `x` and a list `xs`, we have `head(cons(x, xs)) = x` and `tail(cons(x, xs)) = xs`,
* for a non-empty list `xs`, we have `cons(head(xs), tail(xs)) = xs`.

### Mathematical digress

In mathematical terms, lists `[t]` of a given type `t` form a (non-commutative, cancellative) monoid with concatenation `++: [t] × [t] -> [t]` as the binary operation, and the empty list as the unit element.

Lists satisfy the following universal property: for a type `t` and a function `f: t -> m` to a monoid (i.e. a type `m` equipped with a binary operation `×: m × m -> m` and unit element `1` of type `m`), there exists a unique monoid homomorphism `f': [t] -> m` from the monoid of lists of type `t` to `m` such that `f` can be written as the composition of `unit: t -> [t]` and `f': [t] -> m`. Indeed, we define `f'` on a non-empty list `[x_0, x_1, ..., x_{n-1}]` as `f(x_0) × f(x_1) × ... × f(x_{n-1})`, and on the empty list `[]` as `1`, the unit element of `m`. The equality `f = f' . unit`˛follows from the definition of `unit` and `f'`, since `f'(unit(x)) = f'([x]) = f(x)`. Checking that `f'` is a monoid homomorphism requires two things: the unit element `[]` of `[t]` is sent to the unit element `1` of `m`, which is true by definition of `f'`, and that `f'(xs ++ ys) = f'(xs) × f'(ys)`, which can be verified using the definitions.

**TODO:** add graph of factorization

## TODO: Implementation as cons lists

from implementation POV, we will be dealing with *cons* or *singly linked* lists.

implementation of concatenation: runtime depends on the first list, associativity: the result is the same, however there are runtime implications

definition of lists: (empty, cons) or (empty, one-element, concat), equivalence?

list comprehension

## Basic operations and their implementation

**TODO:** add examples

### Length

We define the **length** of a list as the number of items the list contains. By definition, the empty list has 0 length. Length is additive with respect to concatenation, i.e. the length `length(xs ++ ys)` is the sum `length(xs) + length(ys)` of the individual lists.

In other words, the function `length: [t] -> Int (or Nat)` from the lists of type `t` to integers (even natural numbers suffice) is a monoid homomorphism, if we consider integers (natural numbers) as a monoid under addition.

Another POV for `length` is to use the universal property of lists: take the constant-1 function `const_1: t -> Int` that assigns the value `1` to all elements of type `t`, then the induced function `const_1': [t] -> Int (or Nat)` is precisely `length`.

### Map

Suppose that we have a function `f: t -> s` and a list `xs = [x_0, x_1, ..., x_{n-1}]` of type `t`. Mapping the function `f` over the list `xs` means that we apply the function on every item of the list, and collect the results in a new list of type `s`: `map(f, xs) = [f(x_0), f(x_1), ..., f(x_{n-1})]`. When mapping over the empty list, by definition we get the empty list as the result: `map f [] = []`.

`map` can be considered as a function with type `(t -> s) × [t] -> [s]`. In particular, if we are fixing a function `f: t -> s`, `map(f, -)` is a function `[t] -> [s]`.

From the definition of concatenation and `map`, we see that `map` distributes over concatenation: `map(f, xs ++ ys) = map(f, xs) ++ map(f, ys)`. Note that the concatenation on the left hand side is the concatenation of lists of type `t`, while on the right hand side, it is the concatenation of lists of type `s`. In mathematical words, we get that `map(f, -)` is a monoid homomorphism from the monoid of lists of type `t` to the monoid of lists of type `s`.

Looking at the first argument of `map`, we might ask how `map` and function composition relate to each other. Let `f: t -> s` and `g: s -> r` be two functions, then the function `map(g . f, -)` is just the composition `map(g, _) . map(f, _)`, so the result will be the same if we map over the composite function, or we compose the two mappings.

We can construct `map(f, -)` from the universal property, too: let `f: t -> s` be a function, and compose this with the `unit_s: s -> [s]` function of `s`. We end up with a function `unit_s . f` from `t` to the monoid `[s]`, so we can use the universal property of lists of type `t` and factorize this as `(unit_s . f)' . unit_t`: the induced function `(unit_s . f)'` is then precisely `map(f, -)`.

**TODO:** graph of commutative square `t -f> s -unit> [s]` and `t -unit> [t] -g> [s]`.

### Filter

Suppose that we have a predicate `p: t -> Bool` and a list `xs = [x_0, x_1, ..., x_{n-1}]` of type `t`. Filtering the list `xs` with the predicate `p` means that we return those items of the list that satisfy the predicate: `filter(p, xs) = [x_i | p(x_i) = True, i = 0..(n-1)]`. In the special case of the empty list, we get the empty list as the result.

The `filter` function has type `(t -> Bool) × [t] -> [t]`, and similarly as with `map`, if we fix a predicate `p: t -> Bool`, `filter(p, -)` is a function `[t] -> [t]`.

To continue with the similarities with `map`, `filter` also distributes over list concatenation: `filter(p, xs ++ ys) = filter(p, xs) ++ filter(p, ys)`. We can say that `filter(p, -)` is a monoid endomorphism of the monoid `[t]`, i.e. a `monoid homomorphism` from `[t]` to itself.

Given two predicates `p` and `q` on `t`, the order in which we filter with them does not matter: `filter(p, -) . filter(q, -) = filter(q, -) . filter(p, -)`, we ge those elements that satisfy both predicates.

Filtering with a predicate `p` twice is the same as filtering with it once: `filter(p, -) . filter(p, -) = filter(p, -)`.

Let `f: t -> s` be a function, and `p: s -> Bool` be a predicate. The composite `p . f: t -> Bool` is a predicate on `t`. If we map over `f` and filter with the predicate `p`, then we get the same result as if we filter with the `p . f` and map over `f`: `filter(p, -) . map(f, -) = map(f, -) . filter(p . f, -)`.  

**TODO:** is there a conceptual way to get filter?

### TODO: Positional operations

Add, get, update, delete n-th element of a list

## TODO: Reduction operators

easiest case is for an associative unital binary operator, than no unit element, then non-associative operator

reduction for associative unital binary operator ¤: reduce ¤ [a_0, a_1, ..., a_n] = a_1 ¤ a_2 ¤ ... ¤ a_n. Associativity is needed for well-definedness. reduce ¤ (l1 + l2) = (reduce ¤ l1) ¤ (reduce ¤ l2). How to define for empty list? From l = l + empty and if we want reduce to distribute over the empty list, then it should be the (left/right) unit element of ¤. Type of reduce: (fn(t, t) -> t) × list(t) -> t. Reduce ¤ is a monoid homomorphism from list(t) to t (t is a monoid with ¤).

examples: sum = reduce +, product = reduce ×, flatten = reduce +, all/any p (p is a predicate) = (reduce and/or) . (map p), min/max = reduce lesser/greater. however greater/lesser does not have a unit element.

### Mathematical digress: semigroups to monoids

A semigroup is a set equipped with an associative binary operation. A monoid is a semigroup with a (left and right) identity element. By forgetting the identity element, every monoid becomes a semigroup. How can we associate a monoid to a semigroup?

Let (S, ×) be a semigroup, and S' = S + {*} be the set S with a new element denoted by \*. We extend the binary operation of S to S' by defining s ×' t = s × t, if s, t are from S, and s × \* = \* × s = s for every element s of S, \* × \* = \*. With this new binary operation, S' becomes a monoid whose identity element is \*. The inclusion S -> S' is a semigroup homomorphism, i.e. it commutes with the binary operations.

Note that if S is a monoid, then the original identity element 1 loses its identity(ness), and \* will be the new identity element, since 1 ×' \* is 1, not \*.

Examples:

* Consider the set of integers {..., -2, -1, 0, 1, 2, ...} with the binary operation `greater` that chooses the greater number between two numbers. This is an associative binary operation on the integers, however it has no identity element: this identity element `x` would have to be smaller than any other integer because of the identity for the identity element `n = greater(n, x)` for every integer `n`, meaning that `n >= x` for all integer. The associated monoid is the set of integers with an element `-inf` such that `greater(n, -inf) = greater(-inf, n) = n`.
* Let S be an arbitrary set, and define the binary operation `first` as `first(s, t) = s`. This is an associative binary operation without an identity element (if S has at least two elements). The associated monoid is `Maybe(S)` with identity element `None` and binary operation `first(Some s, Some t) = Some s`, `first(Some s, None) = first(None, Some s) = Some s`, and `first(None, None) = None`, which is just `nvl/coalesce`.

## Resources

Bird - An Introduction to the Theory of Lists. In: Broy, M. (eds) Logic of Programming and Calculi of Discrete Design. NATO ASI Series, vol 36. Springer, Berlin, Heidelberg. 1987. <https://doi.org/10.1007/978-3-642-87374-4_1>
