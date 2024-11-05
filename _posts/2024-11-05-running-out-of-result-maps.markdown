---
layout:     post
title:      Running Out of Result Maps
date:       Mon Nov  4 07:26:27 PM CET 2024
categories: Programming
---

# The Problem

The [Elm Maybe
module](https://package.elm-lang.org/packages/elm/core/latest/Maybe)
only defines `Maybe.map`, `Maybe.map2`, ..., `Maybe.map5`. There is no
`Maybe.map6`. Similarly, the [Elm Result
module](https://package.elm-lang.org/packages/elm/core/latest/Result)
defines `Result.map`, `Result.map2`, ..., `Result.map5`, and there is
no `Result.map6`.

In [Running Out of
Maps](https://thoughtbot.com/blog/running-out-of-maps), Joël
Quenneville gives a simple example why functions such as `Maybe.map6`
or `Result.map6` are useful. He starts with a `User` record that is
defined as follows:

```elm
type alias User =
  { name : String
  , age : Int
  , address : String
  , email : String
  , role : Role
  }

type Role = Admin | Regular
```

Defining the `User` record automatically defines a `User` record
constructor function that returns a `User` record when passing in
values corresponding to the different fields of the `User` record.
For example,

```elm
User "Alice" 42 "41 Winter Street" "alice@example.com" Regular
```

  evaluates to

```elm
{ address = "41 Winter Street"
, age = 42
, email = "alice@example.com"
, name = "Alice"
, role = Regular
}
```

Since records with six or more fields are quite common, (record
constructor) functions with six or more arguments are quite common as
well.

`Maybe.map5` can be used to create a `User` record wrapped in `Just`
if all the required data is present or `Nothing` if some piece of data
is missing.

```elm
Maybe.map5 User
  (Just "Alice")
  (Just 42)
  (Just "41 Winter Street")
  (Just "alice@example.com")
  (Just Regular)
```

evaluates to

```elm
Just
  { address = "41 Winter Street"
  , age = 42
  , email = "alice@example.com"
  , name = "Alice"
  , role = Regular
  }
```

while

```elm
Maybe.map5 User
  Nothing
  (Just 42)
  Nothing
  (Just "alice@example.com")
  (Just Regular)
```

  evaluates to

```elm
Nothing
```

Similarly,

```elm
Result.map5 User
  (Ok "Alice")
  (Ok 42)
  (Ok "41 Winter Street")
  (Ok "alice@example.com")
  (Ok Regular)
```

  evaluates to

```elm
Ok
  { address = "41 Winter Street"
  , age = 42
  , email = "alice@example.com"
  , name = "Alice"
  , role = Regular
  }
```

  while

```elm
Result.map5 User
  (Err "Name not found")
  (Ok 42)
  (Err "Address not found")
  (Ok "alice@example.com")
  (Ok Regular)
```

  evaluates to

```elm
Err ("Name not found")
```

Note that the result is `Err ("Name not found")` and not `Err
("Address not found")`. `Result.map5` returns the first `Err` value it
encounters.

# The Solution for the Maybe Case

If we extend the `User` record (e.g. by adding a field for storing
language preferences), we will need something to play the role of the
missing `Maybe.map6` and `Result.map6` functions.

In [Running Out of
Maps](https://thoughtbot.com/blog/running-out-of-maps), Joël
Quenneville solves this problem for the `Maybe` case. He defines

```elm
andMap : Maybe a -> Maybe (a -> b) -> Maybe b
andMap =
    Maybe.map2 (|>)
```

and then shows that for any function `fn` of arity 6

```
Maybe.map6 fn x1 x2 x3 x4 x5 x6
```

is the same as

```elm
Just fn
    |> andMap x1
    |> andMap x2
    |> andMap x3
    |> andMap x4
    |> andMap x5
    |> andMap x6
```

The `andMap` pipeline needs a bit more code but it is still easy read
and write.

Before we look at the Result case, let's try to get a better
understanding of what `andMap` does. The first argument passed into
`andMap` is either `Nothing` or `Just v` where `v` is some value of
type `a`.The second argument passed into `andMap` is either `Nothing`
or `Just fn` where `fn` is a function that takes an `a` and returns a
`b`. Let's see what `andMap` does in detail.

```elm
andMap (Just v) (Just fn)
    == Maybe.map2 (|>) (Just v) (Just fn)
    == Just ((|>) v fn)
    == Just (fn v)

andMap Nothing (Just fn) == Maybe.map2 (|>) Nothing (Just fn) == Nothing

andMap (Just v) Nothing == Maybe.map2 (|>) (Just v) Nothing == Nothing

andMap Nothing Nothing == Maybe.map2 (|>) Nothing Nothing == Nothing
```

If any of the arguments passed in is `Nothing`, then `andMap`
immediately returns `Nothing` because `Maybe.map2` returns `Nothing`
if any of the arguments passed in is `Nothing`.

If both the function and the value "are present", `andMap` unwraps
both arguments, applies the function to the value, and wraps the
result in `Just`.

Elm allows the construction of types that seem to lack an obvious
interpretration. In the type signature of `andMap` we see a `Maybe
(a-> b)` type. What does it mean to work with functions that may not
be present? This is hard to understand for people coming from a more
object oriented or imperative background. Nevertheless, It is easy to
write down concrete examples for such types. According to he Elm REPL,
the type of `Just (\n -> n + 1)` is `Maybe (number -> number)`.  The
Elm REPL has not problem with these type because it does not care
about their meaning and because it treats functions just like it
treats any other value.

# A First Look at the Result Case

Following [Running Out of
Maps](https://thoughtbot.com/blog/running-out-of-maps), we define a
function

```elm
andMapR_ : Result x a -> Result x (a -> b) -> Result x b
andMapR_ =
    Result.map2 (|>)
```

The capital `R` in `andMapR_` indicates that `andMapR_` is defined for
the result case. We will later define `andMapR` (without the
underscore) in a different way.

We can now use the Elm REPL to see that

```elm
Ok User
    |> andMapR_ (Err "Name not found")
    |> andMapR_ (Ok 42)
    |> andMapR_ (Err "Address not found")
    |> andMapR_ (Ok "alice@example.com")
    |> andMapR_ (Ok Regular)
```

evaluates to

```elm
Err ("Address not found")
```

This is somewhat unexpected since

```elm
Result.map5 User
    (Err "Name not found")
    (Ok 42)
    (Err "Address not found")
    (Ok "alice@example.com")
    (Ok Regular)
```

  evaluates to

```elm
Err ("Name not found")
```

Simply copying the the construction of `andMap` from [Running Out of
Maps](https://thoughtbot.com/blog/running-out-of-maps) does not work
for `Result` values. The `andMapR_` pipeline returns `Err ("Address
not found")` instead of the expected `Err ("Name not found")`. Maybe
we can fix this by defining another function that plays the role of
`andMap` when dealing with `Result` values.

# The Result.map2 Case

Let's look at the simplest `Result` case. We can verify (e.g. using
the Elm REPL) that

```elm
andMapR_ : Result x a -> Result x (a -> b) -> Result x b
andMapR_ =
    Result.map2 (|>)
```

is not working as expected.

```elm
Result.map2 fn2 (Err e1) (Err e2)
```

is not the same as

```elm
Ok fn2
    |> andMapR_ (Err e1)
    |> andMapR_ (Err e2)
```

The `Result.map2` call returns `Err e1` while the `andMapR_` pipeline
returns `Err e2`. But why is this?

To get a better understanding of where things break, we need a better
understanding of what `andMapR` does. Similar to the `andMap` case for
`Maybe`, we have the following identities:

```elm
andMapR_ (Ok v) (Ok fn)
    == Result.map2 (|>) (Ok v) (Ok fn)
    == Ok ((|>) v fn)
    == Ok (fn v)

andMapR_ (Err e) (Ok fn) == Result.map2 (|>) (Err e) (Ok fn) == Err e

andMapR_ (Ok v) (Err e) == Result.map2 (|>) (Ok v) (Err e) == Err e

andMapR_ (Err e1) (Err e2) == Result.map2 (|>) (Err e1) (Err e2) == Err e1
```

If any of the values passed in is an `Err` value, then `andMap_`
immediately returns some `Err` value because `Result.map2` returns an
`Err` value. The last case shown above is the interesting one. If both
arguments passed in are `Err` values, then `andMapR_` returns the the
first one.

If both the function and the value are `Ok` values, then `andMapR_`
unwraps both arguments, applies the function to the value, and wraps
the result in `Ok`.

We have seen that In the `andMap` case, we have

```elm
andMap Nothing Nothing == Maybe.map2 (|>) Nothing Nothing == Nothing
```

while in the `andMapR_` case,  we have

```elm
andMapR_ (Err e1) (Err e2) == Result.map2 (|>) (Err e1) (Err e2) == Err e1
```

In the `andMap` case, we simply return `Nothing`. There is no first
and second `Nothing`. All `Nothing`s are equal. In the `andMapR_`
case, order matters. `andMapR_` returns the first `Err` value it
encounters.

Now let's look at what happens with in an `andMapR_` pipeline if we pass in
two `Err` values. We start by rewriting the pipeline in one line.

```elm
Ok fn |> andMapR_ (Err e1) |> andMapR_ (Err e2)
    == (Ok fn |> andMapR_ (Err e1)) |> andMapR_ (Err e2)  -- add parentheses
    == (andMapR_ (Err e1) (Ok fn)) |> andMapR_ (Err e2)   -- apply pipe operator
    == Err e1 |> andMapR_ (Err e2)                        -- see above_
    == andMapR_ (Err e2) (Err e1)                         -- apply pipe operator
    == Err e2                                             -- see above
```

It looks like the problem shows up in the last step where we apply
`andMapR_` to two `Err` values. `andMapR_` returns the first `Err`
value passed in. Let's construct something that returns the second
`Err` value instead.

The following implementation of `andMapR` behaves identically to
`andMapR_` except for the case where two `Err` values are passed
in. In this case, `andMapR` returns the second `Err` value instead of
the first one.

```elm
andMapR : Result x a -> Result x (a -> b) -> Result x b
andMapR ra rb =
    case ( ra, rb ) of
        ( Ok v, Ok fn ) ->
            Ok (fn v)

        ( Err e, Ok _ ) ->
            Err e

        ( Ok _, Err e ) ->
            Err e

        ( Err _, Err e ) ->
            Err e
```

Now we have to check that a pipeline of two `andMapR` calls is
actually the same as `Result.map2`. In the transformations below,
`fn2` always denotes a function of two arguments (with the matching
type signature).

Let's first get rid of the pipe operator so that it is easier to
determine the values of `andMapR` function application

```elm
Ok fn2 |> andMapR x1 |> andMapR x2
    == andMapR x1 (Ok fn2) |> andMapR x2            -- apply pipe operator
    == andMapR x2 (andMapR x1 (Ok fn2))             -- apply pipe operator
```

**Case 1: `x1 = Ok v1`, `x2 = Ok v2`**

```elm
andMapR x2 (andMapR x1 (Ok fn2))
    == andMapR (Ok v2) (andMapR (Ok v1) (Ok fn2))
    == andMapR (Ok v2) (Ok (fn2 v1))                -- definition of andMapR
    == Ok ((fn2 v1) v2)                             -- definition of andMapR
    == Ok (fn2 v1 v2)                               -- partial application
    == Result.map2 fn2 (Ok v1) (Ok v2)              -- definition of Result.map2
```

**Case 2: `x1 = Ok v1`, `x2 = Err e2`**

```elm
andMapR x2 (andMapR x1 (Ok fn2))
    == andMapR (Err e2) (andMapR (Ok v1) (Ok fn2))
    == andMapR (Err e2) (Ok (fn2 v1))               -- definition of andMapR
    == Err e2                                       -- definition of andMapR
    == Result.map2 fn2 (Ok v1) (Err e2)             -- definition of Result.map2
```

**Case 3: `x1 = Err e1`, `x2 = Ok v2`**

```elm
andMapR x2 (andMapR x1 (Ok fn2))
    == andMapR (Ok v2) (andMapR (Err e1) (Ok fn2))
    == andMapR (Ok v2) (Err e1)                     -- definition of andMapR
    == Err e1                                       -- definition of andMapR
    == Result.map2 fn2 (Err e1) (Ok v2)             -- definition of Result.map2
```

**Case 4: `x1 = Err e1`, `x2 = Err e2`**

```elm
andMapR x2 (andMapR x1 (Ok fn2))
    == andMapR (Err e2) (andMapR (Err e1) (Ok fn2))
    == andMapR (Err e2) (Err e1)                    -- definition of andMapR
    == Err e1                                       -- definition of andMapR
    == Result.map2 fn2 (Err e1) (Err e2)            -- definition of Result.map2
```

We see that with the new `andMapR` function, a pipeline of two
`andMapR` calls is identical to `Result.map2`. In particular, just
like `Result.map2`, the pipeline of two `andMapR` calls returns the
*first* `Err` it encounters.

# Generalization to More Arguments and Longer Pipelines

This section essentially repeats the computations of the previous
section in an attempt to prove the general case using induction. It
does not provide any new insights. This article is not a peer-reviewed
academic paper. If you find any error or typo, please let me know.

Subscripts are hard to write in markdown code blocks. Therefore, we
are going to change notation a bit. We will be writing (e.g.) `f2` as
`f<2>` where the integer in angle brackets plays the role of a
subscript. Using this notation, we have proven in the last section
that

```elm
Ok fn<2>
    |> andMapR x<1>
    |> andMapR x<2>
```

is the same as

```elm
Result.map2 fn<2> x<1> x<2>
```

Let's assume that for any integer `k`, we have

```elm
Ok fn<k>
    |> andMapR x<1>
    |> andMapR x<2>
    ...
    |> andMapR x<k>
```

is the same as

```elm
Result.map<k> fn<k> x<1> x<2> ... x<k>
```

By applying the first `k` `andMapR` pipeline steps, we see that

```elm
Ok fn<k+1>
    |> andMapR x<1>
    |> andMapR x<2>
    ...
    |> andMapR x<k>
    |> andMapR x<k+1>
```

is the same as

```elm
(Result.map<k> fn<k+1> x<1> x<2> ... x<k>) |> andMapR x<k+1>             (1)
```
It remains to show that the latter expression is the same as

```elm
Result.map<k+1> fn<k+1> x<1> x<2> ... x<k> x<k+1>
```

Note that in expression `(1)` we are passing the function `fn<k+1>` as
the first argument to `Result.map<k>` and we are also providing only
`k` arguments `x<1>`, `x<2>`, ..., `x<k>`. This means that expression
`(1)` actually returns a function of one argument wrapped in a
`Result`. This is exactly what the `andMapR` function expects as its
second argument.

Let's start by applying the pipeline operator in expression `(1)`.  We
see that `(1)` is the same as

```elm
andMapR x<k+1> (Result.map<k> fn<k+1> x<1> x<2> ... x<k>)
```

Again, we consider four cases.

**Case 1: `x<k+1> = Ok v1`, `(Result.map<k> fn<k+1> x<1> x<2> ... x<k>) = Ok fn`**

```elm
andMapR x<k+1> (Result.map<k> fn<k+1> x<1> x<2> ... x<k>)
    == andMapR (Ok v1) (Ok fn)
    == Ok (fn v1)
    == Ok ((fn<k+1> x<1> x<2> ... x<k>) x<k+1>)
    == Ok (fn<k+1> x<1> x<2> ... x<k> x<k+1>)
    == Result.map<k+1> fn<k+1> x<1> x<2> ... x<k> x<k+1>
```

**Case 2: `x<k+1> = Ok v1`, `(Result.map<k> fn<k+1> x<1> x<2> ... x<k>) = Err e`**

```elm
andMapR x<k+1> (Result.map<k> fn<k+1> x<1> x<2> ... x<k>)
    == Err e
    == Result.map<k+1> fn<k+1> x<1> x<2> ... x<k> x<k+1>
```

Since, by construction, `Err e` is the first error passed passed into

```elm
Result.map<k> fn<k+1> x<1> x<2> ... x<k>
```

it is also the first error passed into

```elm
Result.map<k+1> fn<k+1> x<1> x<2> ... x<k> x<k+1>
```

**Case 3: `x<k+1> = Err e`, `(Result.map<k> fn<k+1> x<1> x<2> ... x<k>) = Ok fn`**

```elm
andMapR x<k+1> (Result.map<k> fn<k+1> x<1> x<2> ... x<k>)
    == Err e
    == Result.map<k+1> fn<k+1> x<1> x<2> ... x<k> x<k+1>
```

Again, we see that `x<k+1> = Err e` is the first error passed into

```elm
Result.map<k+1> fn<k+1> x<1> x<2> ... x<k> x<k+1>
```

because `x<1>`, `x<2>`, ..., `x<k>` are all `Ok` values by construction.


**Case 4: `x<k+1> = Err e1`, `(Result.map<k> fn<k+1> x<1> x<2> ... x<k>) = Err e2`**

```elm
andMapR x<k+1> (Result.map<k> fn<k+1> x<1> x<2> ... x<k>)
    == Err e2
    == Result.map<k+1> fn<k+1> x<1> x<2> ... x<k> x<k+1>
```

Here, `Err e2` is the first error value passed into

```elm
Result.map<k> fn<k+1> x<1> x<2> ... x<k>
```

and we show that it is also the first `Err` value passed into

```elm
Result.map<k+1> fn<k+1> x<1> x<2> ... x<k> x<k+1>
```

# The Implementation of andMap in Result.Extra

Interestingly, the
[Result.Extra](https://github.com/elm-community/result-extra/blob/master/src/Result/Extra.elm)
Elm module also contains an implementation of `andMap` that looks as follows:

```elm
andMap : Result e a -> Result e (a -> b) -> Result e b
andMap ra rb =
    case ( ra, rb ) of
        ( _, Err x ) ->
            Err x

        ( o, Ok fn ) ->
            Result.map fn o
```

This is different from the implementation of `andMapR` above (mainly
because it uses `Result.map`) but thinking through all the possible
cases of `ra` and `rb` shows `Result.Extra.andMap` does the same as
`andMapR`.
