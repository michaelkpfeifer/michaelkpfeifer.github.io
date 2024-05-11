---
layout:     post
title:      The Order of Arguments in Left Folds
date:       Sat Mar 09 12:00:00 PM CET 2024
categories: Programming
---
# Preface

Over the years, I have developed a general interest in programming
languages. I wanted to have a place where I could look up the order of
arguments of fold without browsing the documentation of the
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
harder than remembering the argument order. Nevertheless, a nice idea.

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
lots of examples in the [ClojureDocs
page](https://clojuredocs.org/clojure.core/reduce).

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
The standard fold function in Elixir is called *Enum.reduce*. It is
[documented](https://hexdocs.pm/elixir/Enum.html#reduce/3) in the
context of the Elixr Enum module.

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

# Elm

The standard fold function in Elm is called *List.foldl*. It is
documented in the Elm [List
modulule](https://package.elm-lang.org/packages/elm/core/latest/List).

```elm
module Fold exposing (rev)


rev : List Int
rev =
    let
        collection =
            [ 1, 2, 3, 4, 5 ]

        initialAccumulator =
            []

        combiningFunction =
            \currentItem currentAccumulator -> currentItem :: currentAccumulator
    in
    List.foldl combiningFunction initialAccumulator collection


Fold.rev // returns [5, 4, 3, 2, 1]
```

# Emacs Lisp

Emacs Lisp comes with multiple functions (e.g. *seq-reduce* or
*cl-reduce*) that play the rold of fold. This section looks at the
*seq-reduce* function whose documentation is available within Emacs
itself.

```elisp
(require 'seq)
(defun rev ()
  (let ((collection '(1 2 3 4 5))
        (initial-accumulator ())
        (combining-function (lambda (current-accumulator current-item)
                              (cons current-item current-accumulator))))
    (seq-reduce
     combining-function
     collection
     initial-accumulator)))

(print (rev)) ; prints (5 4 3 2 1)
```

# Erlang

The standard fold function in Erlang is called *foldl*. It is
documented in the Erlang [lists
module](https://erlang.org/doc/man/lists.html).

```erlang
-module(fold).

-export([run/0]).

rev() ->
    Collection = [1, 2, 3, 4, 5],
    InitialAccumulator = [],
    CombiningFunction =
        fun(CurrentItem, CurrentAccumulator) ->
                [ CurrentItem | CurrentAccumulator ]
        end,

    lists:foldl(
      CombiningFunction,
      InitialAccumulator,
      Collection
     ).

run() ->
    io:format("~p~n", [rev()]).
```
# Haskell

The standard left fold function in Haskell is (not surprisingly)
called *foldl*. Haskell has a lot more to say about folding than the
rest of the programming languages considered so far. The [Reducing
lists
(folds)](https://hackage.haskell.org/package/base-4.19.1.0/docs/Data-List.html#g:3)
section of the Data.List module only shows the tip of the iceberg.

```haskell
rev :: [Int]
rev =
    let
        collection = [1, 2, 3, 4, 5]
        initialAccumulator = []
        combiningFunction =
          \currentAccumulator currentItem -> currentItem : currentAccumulator
    in
    foldl combiningFunction initialAccumulator collection

main :: IO ()
main = do
    putStrLn $ show rev -- prints [5,4,3,2,1]
```

# F#

The section on [Fold and Scan
Operations](https://learn.microsoft.com/en-us/dotnet/fsharp/language-reference/lists#fold-and-scan-operations)
of the F# documentation on lists explains how to use the *fold*
function. Microsoft provides a lot of documentation. The details may
be somewhat hard to find.

```fsharp
let rev =
    let collection = [1; 2; 3; 4; 5]
    let initialAccumulator = []
    let combiningFunction currentAccumulator currentItem =
        currentItem :: currentAccumulator

    List.fold combiningFunction initialAccumulator collection

printfn "%A" rev // prints [5; 4; 3; 2; 1]
```

# Gleam

The standard fold function in Gleam is actually called *fold*. (Great
name.) It is
[documented](https://hexdocs.pm/gleam_stdlib/gleam/list.html#fold) in
the context of the Gleam list module.

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

This looks familiar. However, Gleam's fold function can also be
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

With this way of calling fold, the order of arguments is irrelevant
and the keys clearly describe the purpose of the respective argument.
(It is, however, still required to remember the order of arguments of
the combining function.)

# JavaScript

The standard fold method in JavaScript is called *reduce*. JavaScript
implements reduce as a method on arrays and not as a function. The
implementation is very flexible,
[details](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/Reduce)
can be found on the Mozilla Developer Network


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
This can be used to write a wrapper around reduce that nearly looks
like the Gleam implementation of fold using named parameters..

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

# Ruby

The standard fold method in Ruby is called *inject* and *reduce* is an
alias. Ruby implements inject as a method on enumerable objects.  It
is
[documented](https://ruby-doc.org/3.3.1/Enumerable.html#method-i-inject)
as a part of the Enumerable module.

```ruby
def rev
  collection = [1, 2, 3, 4, 5]
  initial_accumulator = []
  combining_function = lambda do |current_accumulator, current_item|
    [current_item] + current_accumulator
  end

  collection.inject(initial_accumulator, &combining_function)
end

printf("%s\n", rev) # prints [5, 4, 3, 2, 1]
```

# Summary

In JavaScript and Ruby, fold is not implemented as a function but as a
method on array (like) objects. In these cases, it is not possible to
talk about the order of the three arguments passed into fold. It is
still possible to talk about the order in which collection, combining
function, and initial value of the accumulator appear in code. Which
means that for JavaScript and Ruby, the collection is considered the
first argument.

There is a variant of fold in the Gleam standard library that accepts
named arguments for the collection, the initial accumulator, and the
combining function. This version does not really fit into the table
below.

**fold**

|            | first              | middle              | last                |
|:-----------|:-------------------|:--------------------|:--------------------|
| Clojure    | combining function | initial accumulator | collection          |
| Elixir     | collection         | initial accumulator | combining function  |
| Elm        | combining function | initial accumulator | collection          |
| Emacs Lisp | combining function | collection          | initial accumulator |
| Erlang     | combining function | initial accumulator | collection          |
| F#         | combining function | initial accumulator | collection          |
| Gleam      | collection         | initial accumulator | combining function  |
| Haskell    | combining function | initial accumulator | collection          |
| JavaScript | collection         | combining function  | initial accumulator |
| Ruby       | collection         | initial accumulator | combining function  |

Clojure, Elm, Erlang, F#, and Haskell follow their functional
roots. Their fold functions take the combining function as the first
argument. Although Elixir and Gleam are also functional languages,
they moved the combining function to the end of the argument list.

**Combining Function**

|            | first               | last                |
|:-----------|:--------------------|:--------------------|
| Clojure    | current accumulator | current item        |
| Elixir     | current item        | current accumulator |
| Elm        | current item        | current accumulator |
| Emacs Lisp | current accumulator | current item        |
| Erlang     | current item        | current accumulator |
| F#         | current accumulator | current item        |
| Gleam      | current accumulator | current item        |
| Haskell    | current accumulator | current item        |
| JavaScript | current accumulator | current item        |
| Ruby       | current accumulator | current item        |

Apparently, it is more common to pass in the current accumulator as
the first argument of the combining function. However, I cannot see
any reason why this should be the preferred way of doing things.

**Conclusion**

There is not much of a conclusion. The only recognizable pattern is
that most functional languages take the combining function as first
argument, the initial value as second argument, and the collection as
last argument. But then, even in those functional languages, the order
of arguments in the combining function differs.
