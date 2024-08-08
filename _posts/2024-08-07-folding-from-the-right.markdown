---
layout:     post
title:      Folding from the Right
date:       Thu Aug  7 09:26:03 PM CEST 2024
categories: Programming
---

# Introduction

Functions of the
[fold](https://en.wikipedia.org/wiki/fold_(higher-order_function))
familiy (often named `fold`, `foldl`, `foldr`, `reduce`, `inject`,
...) have some very general applications. They can add numbers, they
can reverse lists, they can build strings, they can do almost
everything. The internet is full of detailed explanations of how these
functions work in your language of choice. I will not even try to
explain it here.

For collections whose items are ordered in some way (e.g. for lists or
arrays), there are two different kinds of fold functions. *Left folds*
can be thought of as traversing a collection from the the lowest to
the highest item (i.e. from the left to the right). *Right folds* can
be thought of as traversing a collection from the highest to the
lowest item (i.e. from the right to the left).

In most programming languages I have seen, right folds seem to be the
little brother of left folds if they exists at all.

* In Clojure, there is no right fold although the general concept of
  folding is very present.

* In Elixir, there is a `List.foldr` function. The
  [documentation](https://hexdocs.pm/elixir/1.12/List.html#foldr/3)
  states: "Folds (reduces) the given list from the right with a
  function. Requires an accumulator.".

* In Elm, there is a `List.foldr` function. The
  [documentation](https://package.elm-lang.org/packages/elm/core/latest/List#foldr)
  states: "Reduce list from the right.".

* In Erlang, there is a `lists:foldr` function. The
  [documentation](https://www.erlang.org/doc/man/lists.html#foldr-3)
  states: "Like foldl/3, but the list is traversed from right to
  left.".

* In Haskell, there is a `foldr` function. The
  [documentation](https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:foldr)
  states "Right-associative fold of a structure, lazy in the
  accumulator.".

* In JavaScript, there is a `reduceRight` method.  The
  [documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/reduceRight)
  states "The `reduceRight()` method applies a function against an
  accumulator and each value of the array (from right-to-left) to
  reduce it to a single value.".

* In Ruby, there is no equivalent to `foldr`. There is only an
  `inject` method for Array objects that traverses the array from the
  left.

It should not be too difficult to come up with more examples. Pick
your favourite programming language and see what its documentation has
to say about left and right folds.

For quite some time, my general impression was that left folds should
be good enough for almost all use cases. What sparked my interest in
the seemingly exotic topic of right folds is a piece of
[documentation](https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:foldl)
of `foldl` in Haskell: "Left-associative fold of a structure, lazy in
the accumulator. This is rarely what you want, but can work well for
structures with efficient right-to-left sequencing and an operator
that is lazy in its left argument."

Interesting. But what does it mean? And why does the Haskell
documentation say that `foldl` is not what I want while all other
programming languages I encountered seem to agree that left folds are
"preferred over" right folds (if right folds exists at all). I must be
missing something.

The programming languages listed above are quite different (although
they all implement the concept of "folding".) I am going to use
examples from these programming languages (mainly from Haskell and
JavaScript) to talk about certain concepts without explaining all of
the details. I believe that it is not too difficult to understand the
examples presented below as long as the idea of folding is understood
in any one programming language.

# The Order of Arguments of the Reducing Function

For the purpose of the current article, we will name the function of
two arguments passed into the different folding functions the
/reducing function/. The reducing function takes two arguments: the
accumulator (accumulating intermediate results) and a value of the
collection being folded.

Different fold implementations have different requirments for the
order of arguments of the reducing function. In some implementations,
the accumulator is passed in as the first argument, in other
implementations it is passed in as the second argument. This can
become particularly confusing when the arguments of the reducing
functions are not explicitly specified. When writing something like

```js
const add = (a, b) => a + b;
const sum = [1, 2, 3].reduce(add, 0);
```

it becomes very hard to track which argument passed into the `add`
function is the accumulator and which one is an item of the
collection. (And in this case, it does not really matter.)

Therefore, we will always write reducing functions in a very explicit
way and we will always try to make it very clear which of the two
arguments of the reducing function is the accumulator and which is an
item of the collection being reduced.

Here is an example of what happens if the order of the arguments in
the reducing function is ignored.

In Haskell, the type signature of `foldl` is

```haskell
(b -> a -> b) -> b -> t a -> b
```

In Elm, the type signature of `List.foldl` is

```elm
(a -> b -> b) -> b -> List a -> b
```

In both cases, the reducing function is the first argument passed into
`foldl` or `List.foldl`. Since the second argument is the initial
value of the accumulator, we see that in the Haskell case, the
reducing function takes the accumulator as its first argument while in
the Elm case, the reducing function takes the accumulator as its
second argument. In many cases, that does not matter. But in some
cases, it does.

For example, in Haskell

```haskell
foldl (-) 1 [0]
```
evaluates to `1 - 0 = 1` while in Elm,

```elm
List.foldl (-) 1 [0]
```
evaluates to `0 - 1 = -1`.

Subtraction is the obvious example used to demonstrate these
differences because, in general, (𝑎 - 𝑏) - 𝑐 is not the same as 𝑎 -
(𝑏 - 𝑐).

# Associativity

Addition of natural numbers is associative. That means that for any
three natural numbers 𝑎, 𝑏, 𝑐,

<div style="text-align: center; margin-bottom: 16px;">
(𝑎 + 𝑏) + 𝑐 = 𝑎 + (𝑏 + 𝑐)
</div>

We have probably seen that in school (without paying too much
attention). Similarly, multiplication of real numbers or composition
of (mathematical) functions of one variable is associative. In fact,
associativity is such a basic concept in mathematics that it is a
required property in the definition of a lot of algebraic structures
(starting from the humble semigroup).

Since addition is not the only mathematical operation on elements of
some general set (such as the natural numbers), we often use a more
general symbol such as ⊕ to express the law of associativity

<div style="text-align: center; margin-bottom: 16px;">
(𝑎 ⊕ 𝑏) ⊕ 𝑐 = 𝑎 ⊕ (𝑏 ⊕ 𝑐)
</div>

for a more general operation.

# Left Folds, Right Folds, and Associativity

To get a better understanding of how left and right folds relate to
associativity, let's first look at an example of using `reduce` in
JavaScript.

```js
const sum = [1, 2, 3].reduce((acc, val) => acc + val, 0);
console.log(sum);
```

In this example, we use `reduce` to compute the sum of `1`, `2`, and
`3`. We do that by telling `reduce` to use the reducing function
`(acc, val) => acc + val` to sum intermediate results, starting with
an initial value of `0`. Since the reducing function takes the
accumulator (i.e. the intermediate results) as its first argument,
this means that we actually compute

```js
const sum = (((0 + 1) + 2) + 3)
console.log(sum);
```

For the sake of completeness, Let's look at the same basic example in
Haskell.  But before we do that, let's have a look at the type
signatures of `foldl` and `foldr` and see whether we can derive
something from these type signatures..

```haskell
foldl :: (b -> a -> b) -> b -> t a -> b
foldr :: (a -> b -> b) -> b -> t a -> b
```

The first argument passed into `foldl` and `foldr` is the reducing
function.

The second argument is the initial value of the accumulator. This is a
value of type `b`, the same type that `foldl` and `foldr` return in
the end.

And the third argument is the list (or something else) to be folded.

We have seen that `foldl` and `foldr` return a value of type `b` which
is also the type of the initial value of the accumulator. If we
compare the type signatures of the reducing functions, we see that

* in the `foldl` case, the accumulator is the first argument passed
  into the reducing function, and

* in the `foldr` case, the accumulator is the second argument passed
  into the reducing function.

This is an important difference that we have to keep in mind.

As promised above, here is the basic example of `foldl` in Haskell

```js
foldl (\acc val -> acc + val) 0 [1, 2, 3]
```

As derived from the type signature of `foldl`, the accumulator is the
first argument passed into the reducing function. As in the JavaScript
case, we actually compute `(((0 + 1) + 2) + 3)`

This is what (I believe) the Haskell documentation means when it says
that `foldl` is left associative. We start evaluating a sequence of
operations (or a sequence of binary function calls) from the left of
the collection.

On the other hand, the Haskell documentation also states that `foldr`
is right associative. If *left associative* means that we group
operations as

<div style="text-align: center; margin-bottom: 16px;">
(𝑎 ⊕ 𝑏) ⊕ 𝑐
</div>

then *right associative* means that we group operations as

<div style="text-align: center; margin-bottom: 16px;">
𝑎 ⊕ (𝑏 ⊕ 𝑐)
</div>

We can see more easily what JavaScript and Haskell actually compute by
operating on strings that represent the actual computations. Lets
start with `reduce` in JavaScript.

```js
const sum = ["1", "2", "3"]
  .reduce((acc, val) => "(" + acc + " + " + val + ")", "0");
```

The same thing in Haskell can be done as follows:

```haskell
foldl (\acc val -> "(" ++ acc ++ " + " ++ val ++ ")") "0" ["1", "2", "3"]
```

In both cases, we see exactly what we expected: the string `"(((0 + 1) + 2) + 3)"`.

# Folding from the Right and Right Associativity

It may now seem obvious that the Haskell people and the rest of the
world essentially talk about the same thing. The Haskell people talk
about right associativity, and the rest of the world talks about
reducing a list from the right. But is this really true? What does it
really mean to reduce a list from the right?

Let's see what happens if redo some of the computations above with
JavaScript's `reduceRight` and Haskell's `foldr`. We also use the more
general `⊕` symbol instead of `+` to make it a bit clearer that the
operations we are considering a more general than simple addition.

Here is the JavaScript case. Note that the reducing function in
`reduceRight` still takes the accumulator as its first argument.

```js
const res = ["1", "2", "3"]
  .reduceRight((acc, val) => "(" + val + " ⊕ " + acc + ")", "0");
```

And here is the Haskell case. Note that the reducing function in
`foldr` takes the accumulator as its second argument.

```haskell
foldr (\val acc -> "(" ++ val ++ " ⊕ " ++ acc ++ ")") "0" ["1", "2", "3"]
```

We get identical results in the JavaScript and the Haskell cases:
`"(1 ⊕ (2 ⊕ (3 ⊕ 0)))"`.

Let's validate (using the simple example from above) that
`reduceRight` really is the same as using `reduce` on the reversed
list. Here is the JavaScript code snippet.

```js
const res = ["1", "2", "3"]
  .reverse()
  .reduce((acc, val) => "(" + val + " ⊕ " + acc + ")", "0");
```

<!-- For the sake of completeness, here are some examples of right folds in other -->
<!-- programming languages. -->

<!-- In Elixir, we get: -->

<!-- ```elixir -->
<!-- List.foldr(["1", "2", "3"], "0", fn val, acc -> "(" <> val <> " ⊕ " <> acc <> ")" end) # evaluates to "(1 ⊕ (2 ⊕ (3 ⊕ 0)))" -->
<!-- ``` -->

<!-- In Erlang, we get the same results that we get in Elixir (which is not -->
<!-- surprising given the relationship between Erlang and Elixir). -->

<!-- ```erlang -->
<!-- lists:foldr(fun(Val, Acc) -> "(" ++ Val ++ " + " ++ Acc ++ ")" end, "0", ["1", "2", "3"]). % evaluates to "(1 + (2 + (3 + 0)))" -->
<!-- ``` -->

<!-- And Elm is no exception. -->

<!-- ```elm -->
<!-- List.foldr(\val acc -> "(" ++ val ++ " ⊕ " ++ acc ++ ")") "0" ["1", "2", "3"] -- evaluates to "(1 ⊕ (2 ⊕ (3 ⊕ 0)))" -->
<!-- ``` -->

When we look at the result `"(1 ⊕ (2 ⊕ (3 ⊕ 0)))"`, we see that
operations are grouped from the right to the left meaning that right
folds are right associative.

# Implmentation of `foldl` And `foldr`

In many functional programming languages, lists are implemented as
linked lists. `foldl` works nicely with these linked lists. We take
the accumulator, the first item of the list, compute the new value of
the accumulator, and do the same operation with the rest of the list
recursively until the list is empty.

But what about `foldr`? When (e.g.) the Erlang documentation states
that "the list is traversed from right to left", is this really true?
Traversing a linked list from right to left is an expensive
operation. Is Erlang actually doing this?  Or is this merely a
description of the operation that is easier to understand by the
reader?

The [Haskell Wiki](https://wiki.haskell.org/Fold) shows the following
recursive definitions of `foldl` and `foldr`:

```haskell
foldl f acc [] = acc
foldl f acc (val:rest) = foldl f (f acc val) rest
```

The implementation of `foldr` looks a little different.

```haskell
foldr f acc [] = acc
foldr f acc (val:rest) = f val (foldr f acc rest)
```

We notice immediately, that these definitions do not involve any
reversal of the list to be folded. No list list traversed from right
to left. But what exactly is the difference between the `foldl` and
`foldr` implementations?  In the the `foldl` case, we compute new
values of the accumulator (by applying the reducing function) as soon
as possible with every reduction step.  In the `foldr` case, we defer
application of the reduction function to the end of the
recursion. Before we can apply the reducing function, we have to
compute the `foldr f acc rest` expression.

Let's try to find out how the programming languages we mentioned at
the beginning of this article implement `foldr`.

* In Clojure, there is no `foldr`.

* Elixir [reuses the Erlang implementation](https://github.com/elixir-lang/elixir/blob/fb729784e5504f499b0179c4f154e73ea2a6f216/lib/elixir/lib/list.ex#L266]).

* The [Elm implementation](https://github.com/elm/core/blob/master/src/List.elm) is quite interesting. Elm implements `List.foldr` recursively but reverses the given list if it becomes too large.

* The [Erlang
  implementation](https://github.com/erlang/otp/blob/master/lib/stdlib/src/lists.erl)
  is very similar to the Haskell implementation shown above. This
  means in particular, that no list is traversed from right to left.

* In JavaScript, `reduceRight` operates on arrays and not on linked
  lists such that the implementation can be done in a non-recursive
  way. Accessing the last item in an array is not an expensive
  operation.

* In Ruby, there is no `foldr`.

In other words, there is a nice recursive definition of `foldr` that
does not require any lists to be reversed. For large lists, this
becomes a problem since `foldr` is not tail recursive and intermediate
results need to be kept until the end of the list has been reached.

# Accumulating Lists

We have already seen that `foldr` is said to reduce the given list
from the right but that does not mean that the list is reversed or
that it is traversed to reach the last entry of the list. There are
recursive implementations of `foldr` that work without expensive list
operations.

Let's assume that we want to implement a function `incl` that takes a
list of integers and returns another list of integers where each entry
is incremented. And let's also assume for the sake of the argument
that we are not going to use `map`. An obvious implementation in
Haskell may look as follows.

```haskell
incl xs = foldl (\acc x -> (x + 1):acc) [] xs
```
We start with the empty list as initial value of the accumulator. Then we take
each number in the current list, increment it, an prepend it to the
accumulator. When we run this with `[1, 2, 3]` as input, we get the result
`[4, 3, 2]` (which is in reverse order). Of course, we can fix this be
reversing the result of ~incl~ and in Haskell that can be implemented very
concisely.

```haskell
incl' = reverse.incl
```

We could also solve this by appending to the accumulator.

```haskell
incl'' xs = foldl (\acc x -> acc ++ [x + 1]) [] xs
```

But now we have to traverse the accumulator whenever we process a new number
(which can also become computationally expensive.) This little problem goes
away if we used ~foldr~ instead of ~foldl~.

```haskell
incr xs = foldr (\x acc -> (x + 1):acc) [] xs
```

`incr` looks nearly identical to `incl`. The only difference is that
the order of the arguments in the reducing function are reversed. This
returns the desired `[2, 3, 4]` result without reversing the result or
traversing intermediate values of the accumulator.

# The Choice of the Folding Function in Haskell

At the beginning of the current article, we cited a piece of
documentation for `foldl` in Haskell: "Left-associative fold of a
structure, lazy in the accumulator. This is rarely what you want, but
can work well for structures with efficient right-to-left sequencing
and an operator that is lazy in its left argument."

Understanding this piece of documentation in its entirety is beyond
the scope of the current article and beyond my current understanding
of Haskell.  Nevertheless, we have learned a few things about folding
from the right and maybe that helps us to get a better understanding
of the "this is rarely what you want" part.

We have to keep in mind that `foldl` and `foldr` are semantically
different.  In many cases, the results produced by `foldl` and `foldr`
are identical but in some cases they differ. And if we need to fold
from the left in order to produce the correct result, then `foldr`
cannot help.

We have already seen one advantage of `foldr`. When a new list is
constructed as a result of folding, the entries in this list are
naturally in correct order. When folding from the left, the resulting
list is in reverse order (unless the resulting list is reordered
explicitly in one way or another).  (We would probably use `map`
instead of `foldr` anyway.)

Another interesting aspect of `foldr` is that it can behave well with infinite
lists. We have already seen the definition of `incr`

```haskell
incr xs = foldr (\x acc -> (x + 1):acc) [] xs
```

`incr` takes a list and adds `1` to each item in the list. For
example, `incr [1, 2, 3]` evaluates to `[2, 3, 4]`. But what happens
when we apply `incr` to an infinite list?

Obviously, we cannot compute `incr [1..]`. But `take 2 (incr [1..])`
returns `[2, 3]`. The reason for this astonishing capability is that
Haskell is lazy. Nothing is computed unless it is really needed. To
get an idea how this helps `foldr`, we look at the recursive
definition of ~foldr~

```haskell
foldr f acc [] = acc
foldr f acc (val:rest) = f val (foldr f acc rest)
```

that we have seen above. If we apply this definition recursively, we
get something like

```haskell
take 2 (incr [1..]) =
take 2 (foldr (\x acc -> (x + 1):acc) [] [1..]) =
take 2 ((\x acc -> (x + 1):acc) 1 (foldr (\x acc -> (x + 1):acc) [] [2..])) =
take 2 (2:(foldr (\x acc -> (x + 1):acc) [] [2..])) =
take 2 (2:(3:(foldr (\x acc -> (x + 1):acc) [] [3..]))) =
take 2 ([2, 3] ++ (foldr (\x acc -> (x + 1):acc) [] [3..]))
```

From this we see that the resulting list starts with `[2, 3]` and
whatever the remaining `foldr` expression may compute will not
influence the first two list items. This means that `take 2 (incr
[1..])` returns `[2, 3]`. The `foldr` expression is not executed. It
is an interesting observation that the initial value of the
accumulator does not influence the result at all.

Now, lets see what this looks like when we are folding from the
left. We use the following definition of `foldl`.

```haskell
foldl f acc [] = acc
foldl f acc (val:rest) = foldl f (f acc val) rest
```

It does not make too much sense to look at

```haskell
incl xs = foldl (\acc x -> (x + 1):acc) [] xs
```

  or

```haskell
incl' = reverse.incl
```

We have already seen that `incl` returns the desired result in
reversed order. Therefore, there is no hope that `take 2 (incl [1..])`
or `take 2 (incl' [1..])` returns `[2, 3]`. We consider

```haskell
incl'' xs = foldl (\acc x -> acc ++ [x + 1]) [] xs
```

since we know that `incl''` at least produces results in the expected
order.

```haskell
take 2 (incl'' [1..]) =
take 2 (foldl (\acc x -> acc ++ [x + 1]) [] [1..]) =
take 2 (foldl (\acc x -> acc ++ [x + 1]) ([] ++ [2]) [2..]) =
take 2 (foldl (\acc x -> acc ++ [x + 1]) ([] ++ [2] ++ [3]) [3..]) =
take 2 (foldl (\acc x -> acc ++ [x + 1]) [2, 3] [3..])
```

As in the `foldr` case, we see the `[2, 3]` sublist emerging. But this
time, it is not at the beginning of the result that is being
produced. It is an argument of some recursive function call where
`take 2` cannot get hold of it. Although the example above is very
specific, this is a general phenomenon. When using `foldl`,
intemediate results are always passed to the reducing function.

In some cases, `foldr` can even reduce infinite lists without using
something like `take 2`. For example, we can define a function `andr`
as follows:

```haskell
andr xs = foldr (&&) True xs
```

`andr` takes a list of boolean values as its argument. For a finite
input, `andr` returns `False` if and only if any of the items in the
list is `False`.

Interestingly, `andr ([True, False, True] ++ (repeat False))` also
returns `False` which means that Haskell is not evaluating the
infinite list. The reason for this behaviour is again that Haskell is
lazy and that the `(&&)` function does not need to evaluate its second
argument if the first one is `False`. As a consequence, this means
that for infinite lists, `andr` either returns `False` or it runs
forever. (Which somewhat limits its practical usability.)

As we have seen in the `foldr` case, being lazy can have some
advantages. In some cases, this allows `foldr` to return the result of
an fold early. The ability to process infinite lists is just one
specical case of this generally desirable behaviour.

At the beginning of this article, we have seen that one of the big
differences of `foldr` and `foldl` is the order in which expressions
are computed.

For example,

```
foldr (+) 0 [1, 2, 3, 4]
```

is computed as

```
1 + (2 + (3 + (4 + 0)))
```

while

```
foldl (+) 0 [1, 2, 3, 4]
```

is computed as

```
(((0 + 1) + 2) + 3) + 4
```

We see that `foldr` cannot compute intermediate results. The first
time `foldr` can actually simplify the constructed expression is when
the given list has been traversed completely. Only then can `foldr`
replace `4 + 0` by `4`. And then it can replace `3 + 4` by `7`. And so
on. Haskell needs to keep track of all of these computations. This
means that `foldr` needs linear space in terms of the length of the
list being folded.

On the other hand, `foldl` can easily compute intermediate results.
As soon as the first list item is processed, `0 + 1` can be replaced
by `1`. As soon as the second list item is processed, `1 + 2` can be
replaced by `3`. And so on. This means that `foldl` *should* use
constant space. But now, Haskell's lazyness makes things more
complicated.  Haskell will *not* compute `0 + 1` as soon as the first
list item is processed. After all, Haskell is lazy and it does not
know that `0 + 1` will actually be needed. This means that even in the
`foldl` case, Haskell will need to take care of a lot of intermediate
computation which also needs linear space in terms of the list being
folded.

When we are now comparing `foldl` and `foldr`, we see that both need
linear space but `foldr` has a few nice additional properties.

Neither `foldl` nor `foldr` can process very large lists. For example, running

```
foldl (+) 0 [1..100000000]
```

or

```
foldr (+) 0 [1..100000000]
```

both lead to a stack overflow error. Haskell's solution to this
problem is to provide a `foldl'` function defined in the
`Data.Foldable` module that strictly evaluates the reducing
function. And, indeed,

```
import Data.Foldable
foldl' (+) 0 [1..100000000]
```

quickly returns `5000000050000000`.

When the Haskell documentation states that `foldl` is rarely what I
want, what it really means is that I should use either `foldr`
(because I need to reduce from the right or I want to take advantage
of lazyness) or `foldl'` (because it is strict and computes results in
constant space). The main difference between Haskell and the rest of
the programming languages above is that Haskell is lazy.  And this
influences the behaviour of folding functions.

Interestingly, the `Data.Foldable` module also defines a `foldr'`
function.  And the corresponding
[documentation](https://hackage.haskell.org/package/base-4.16.2.0/docs/Data-Foldable.html)
says: This is rarely what you want.
