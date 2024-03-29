---
layout:     post
title:      The Order of Arguments in Left Folds
date:       Sat Mar 09 12:00:00 PM CET 2024
categories: Programming
---
# Preface

Over the years, I have developed a general interest in programming
languages. I wanted to have a place where I could look up the order of
arguments of folds without browsing the documentation of the
respective language over and over again.

I thought about this little article for quite some time. I always
postponed it because I did not really see its value for most software
developers who may only use one or two programming languages.

What finally made me change my mind was the way folds can be written
in the Gleam programming language. Gleam is the first language I
encountered where the arguments of folds can be provided using named
arguments. That is interesting.

In hindsight, it' is a simple idea. It is so simple that everybody can
write a wrapper that accepts named arguments and then calls the fold
function of the respective language with the arguments in the right
order (unless the language does not support named arguments, of
course).

Why aren't we writing these wrappers? I guess one of the reasons is
that we would have to remember the argument names. Which may be even
harder than remembering the argument order.

# Introduction

Functions of the
[fold](https://en.wikipedia.org/wiki/fold_(higher-order_function))
familiy (often named *fold*, *foldl*, *foldr*, *reduce*, *inject*,
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

In this article, we will focus on the order of arguments of left folds
because sometimes there are subtle differences between the order of
left and right folds even within a single programming language. And
left folds seem to be more common than right folds.

Usually, there is at least one variant of a left fold that takes three
arguments.

  - a collection of items to be "folded" (or "reduced")
  - a combining function of two arguments that computes the
    accumulator for the next iteration, and
  - an initial value of the accumulator.

The combining function always takes two arguments: the current
accumulator value and an item of the collection being folded.

The order of arguments of left folds and of the combining function
differ from programming language to programming language. Sometimes,
this makes things hard to understand. As we will see, there is not too
much consensus of how the arguments should be passed in.

Different programming languages follow different conventions and
requirements for naming identifiers. Therefore, we introduce variable
names that we will keep (more or less) unchanged in all examples to
follow. "More or less" in this context means that Ruby's
`current_accumulator` will be named `currentAccumulator` in
JavaScript, `CurrentAccumulator` in Erlang, and `current-accumulator`
in Clojure. Here

  - `collection` is the collection to be folded over,
  - `initial_accumulator` is the initial value of the accumulator,
  - `current_accumulator` is the current value of the accumulator
    passed into the combining function, and
  - `current_item` is the current item of the collection passed into
    the combining function.

For each programming language we will show how to reverse a list (or
an array) of the integers 1 to 5 using fold. When reversing a list (or
an array), the order of arguments in the combining function is
relevant, so these examples help make sure that we get the order or
arguments right.

# Clojure

The standard fold function in Clojure is called *reduce*. There are
lots of examples in
[https://clojuredocs.org/clojure.core/reduce](https://clojuredocs.org/clojure.core/reduce).

```clojure
(defn rev []
  (let [collection '(1 2 3 4 5)
        initial-accumulator []
        combining-function (fn [current-accumulator current-item]
                             (cons current-item current-accumulator))]
    (reduce
     combining-function
     initial-accumulator
     collection)))

(println (rev)) ; prints (5 4 3 2 1)
```

# Elixir

The standard fold function in Elixir is called *Enum.reduce*.
[https://hexdocs.pm/elixir/Enum.html#reduce/3](https://hexdocs.pm/elixir/Enum.html#reduce/3)
provides a description of the `Enum.reduce` function (along with all
the other functions in the `Enum` module).

```elixir
def rev do
  collection = [1, 2, 3, 4, 5]
  initial_accumulator = []

  combining_function = fn current_item, current_accumulator ->
    [current_item | current_accumulator]
  end

  Enum.reduce(
    collection,
    initial_accumulator,
    combining_function
  )
end

Reduce.rev() # prints [5, 4, 3, 2, 1]
```

# Gleam

The standard fold function in Gleam is actually called *fold*. (Great name.)
[https://hexdocs.pm/gleam_stdlib/gleam/list.html#fold](https://hexdocs.pm/gleam_stdlib/gleam/list.html#fold)
provides a short description of fold in the context of Gleam's list
module.

```gleam
import gleam/list
import gleam/io

pub fn rev1() {
  let collection = [1, 2, 3, 4, 5]
  let initial_accumulator = []
  let combining_function = fn(current_accumulator, current_item) {
    [current_item, ..current_accumulator]
  }

  list.fold(collection, initial_accumulator, combining_function)
}

io.debug(rev1()) // prints [5, 4, 3, 2, 1]
```

This looks familiar. However, Gleam's `fold` function can also be
called using named arguments.

```gleam
import gleam/list
import gleam/io

pub fn rev2() {
  let collection = [1, 2, 3, 4, 5]
  let initial_accumulator = []
  let combining_function = fn(current_accumulator, current_item) {
    [current_item, ..current_accumulator]
  }

  list.fold(
    over: collection,
    from: initial_accumulator,
    with: combining_function,
  )
}

io.debug(rev2()) // prints [5, 4, 3, 2, 1]
```

With this way of calling `fold`, the order of arguments is irrelevant
and the keys clearly describe the purpose of the respective argument.
(It is, however, still required to remember the order of arguments of
the combining function.)

# JavaScript

The standard fold method in JavaScript is called *reduce*. JavaScript
implements `reduce` as a method on arrays and not as a function. The
implementation is very flexible, details can be found in
[https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/Reduce](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/Reduce).

```js
const rev1 = () => {
  const collection = [1, 2, 3, 4, 5];
  const initialAccumulator = [];
  const combiningFunction = (currentAccumulator, currentItem) => [
    currentItem,
    ...currentAccumulator,
  ];

  return collection.reduce(combiningFunction, initialAccumulator);
};

console.log(rev1()); // prints [ 5, 4, 3, 2, 1 ]
```

JavaScript does not support named parameters. However, a very similar
behaviour can be achieved by using object literals and destructuring.
This can be used to write a wrapper around `reduce` that nearly looks
like the Gleam implementation of `fold` using named parameters..

```js
const reduceWithNamedArgs = ({ over, from, withFn }) => {
  return over.reduce(withFn, from);
};

const rev2 = () => {
  const collection = [1, 2, 3, 4, 5];
  const initialAccumulator = [];
  const combiningFunction = (currentAccumulator, currentItem) => [
    currentItem,
    ...currentAccumulator,
  ];

  return reduceWithNamedArgs({
    over: collection,
    from: initialAccumulator,
    withFn: combiningFunction,
  });
};

console.log(rev2());  // prints [ 5, 4, 3, 2, 1 ]
```

# Summary

In JavaScript, fold is not implemented as a function but as a method
on array objects. In this case, it is not possible to talk about the
order of the three arguments passed into fold. It is still possible to
talk about the order in which collection, combining function, and
initial value of the accumulator appear in code. Which means that for
JavaScript and Ruby, the collection is considered the first argument.

There is a variant of fold in the Gleam standard library that accepts
named argument for the collection, the initial accumulator, and the
combining function. This version does not really fit into the table
below.

**fold**

|            | first              | middle              | last                |
|:-----------|:-------------------|:--------------------|:--------------------|
| Clojure    | combining function | initial accumulator | collection          |
| Elixir     | collection         | initial accumulator | combining function  |
| Gleam      | collection         | initial accumulator | combining function  |
| JavaScript | collection         | combining function  | initial accumulator |


**Combining Function**

|            | first               | last                |
|:-----------|:--------------------|:--------------------|
| Clojure    | current accumulator | current item        |
| Elixir     | current item        | current accumulator |
| Gleam      | current accumulator | current item        |
| JavaScript | current accumulator | current item        |


The order of arguments in the combining function seems to be arbitrary
(which probably reflects that there is no "natural" order of
arguments).
