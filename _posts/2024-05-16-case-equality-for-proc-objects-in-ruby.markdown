---
layout:     post
title:      Case Equality for Proc Objects in Ruby
date:       Thu May 16 10:48:12 PM CEST 2024
categories: Programming
---

# The Basics

Let `p` be a Ruby proc object and let `obj` be an arbitrary object. In
an expression such as

```ruby
p === obj
```

the `===` method "invokes the block with `obj` as the proc's parameter
like `#call`. This allows a proc object to be the target of a `when`
clause in a case statement". (I copied this description from the [the
documentation of `===` as a method on Proc
objects](https://ruby-doc.org/core-3.0.0/Proc.html#method-i-3D-3D-3D).

Let's start by considering the kind of the `case` statements that are
usually used to motivate the behaviour described above.

```ruby
case 3
when even? then puts 'even'
when odd? then puts 'odd'
else puts 'impossible'
end
```

But how does this work in practice? How do we construct `even?` and
`odd?`. How do we use the `even?` and `odd?` methods on integers that
Ruby already provides?

To understand how Ruby processes the different options in the case
expression, we consider the following equivalent expression:

```ruby
if even? === 3
  puts 'even'
elsif odd? === 3
  puts 'odd'
else
  puts 'impossible'
end
```

The most important aspect of this transformation is that clauses in
`case` expressions are compared using the `===` operator. The
documentation cited at the beginning of the current article means that
this is equivalent to

```ruby
if even?.call(3)
  puts 'even'
elsif odd?.call(3)
  puts 'odd'
else
  puts 'impossible'
end
```

# Procs, Lamdas, and Methods

What does this imply for `even?` and `odd?`? Whatever `even?` and
`odd?` are, they must have a `call` method. And we know that proc
objects have a `call` method.

Ruby knows two slightly different kinds of proc objects (namely
lambdas and procs) that have slightly different propertries (that are
not relevant for the current discussion).

Each of the two kinds can be defined in (at least) two different ways.

```ruby
l1 = lambda { |n| n + 1 }
l2 = -> n { n + 1 }
p1 = Proc.new { |n|  n + 1 }
p2 = proc { |n| n + 1 }
```

`l1` and `l2` define the same lambda and `p1` and `p2` define the same
(non-lambda) proc.

Once a proc object is defined, it can be called in (at least) four different
ways.

```ruby
p = -> n { n + 1 }

p.call(1)
p.(1)
p[1]
p === 1
```

Back to the initial example. How exactly do we construct `even?` and
`odd?`? The following is a first example.

```ruby
def even?
  ->(n) { n.even? }
end

def odd?
  ->(n) { n.odd? }
end
```

Here, `even?` and `odd?` are methods that do not take any
arguments. Instead, they return a lambda that takes a single
argument. This is an important distinction. In this definition,
`even?` and `odd?` are not proc objects.

We could try to simplify this a bit by directly assigning proc objects
to local variables (instead of returning them from method calls).

```ruby
even? = Proc.new { |n| n.even? }
odd? = Proc.new { |n| n.odd? }
```

Unfortunately, this does not work. Ruby *does* allow method names to
end with a question mark but it does *not* allow variable names to end
with a question mark.

We could still replace the lambdas returned by `even?` and `odd?` by
procs.

```ruby
def even?
  Proc.new { |n| n.even? }
end

def odd?
  Proc.new { |n| n.odd? }
end
```

This does not help too much. In the context of the current
discussion. Methods returning procs are not too different from methods
returning lambdas and the effort for writing procs is the same as the
effort for writing lambdas.

# Reusing Existing Methods

In order to use an existing method with `case` and `when` as described
above, it needs to be wrapped in something like a lambda or a
proc. And this needs to be done for each of the branches of the `case`
expression. This quickly becomes somewhat painful if the number of
branches grows.

```ruby
zero = -> (n) { n % 4 ==  0 }
one = -> (n) { n % 4 ==  1 }
two = -> (n) { n % 4 ==  2 }
three = -> (n) { n % 4 ==  3 }

case 3
when zero then puts 'divisile'
when one then puts 'residue 1'
when two then puts 'residue 2'
when three then puts 'residue 3'
else puts 'impossible'
end
```

The example above demonstrates that it is not always necessary to use
`def` to define methods that return lamdas or procs. We can also use
(e.g.) local varriables. In the first examples, this was only
necessary because that questionmark in `even?` and `odd?` looks so
good.

Of course, the last example is somewhat contrived. It can be written as

```ruby
case 3 % 4
when 0 then puts 'divisile'
when 1 then puts 'residue 1'
when 2 then puts 'residue 2'
when 3 then puts 'residue 3'
else puts 'impossible'
end
```

The lambdas that are assigned to the `zero`, `one`, `two`, `three`
variables, are created explicitly for each possible residue. Since
Ruby is a very flexible language, there are ways to get rid of this
explicitness.

```ruby
[[:zero, 0], [:one, 1], [:two, 2], [:three, 3]].each do |(name, residue)|
  define_method(name) { -> (n) { n % 4 == residue } }
end

case 3
when zero then puts 'divisile'
when one then puts 'residue 1'
when two then puts 'residue 2'
when three then puts 'residue 3'
else puts 'impossible'
end
```

# Readability

An expression such as

```ruby
case 3
when even? then puts 'even'
when odd? then puts 'odd'
else puts 'impossible'
end
```

is very readable. There is no doubt about this. Now, let's add the methods that
return anonymous functions.

```ruby
def even?
  ->(n) { n.even? }
end

def odd?
  ->(n) { n.odd? }
end

case 3
when even? then puts 'even'
when odd? then puts 'odd'
else puts 'impossible'
end
```

Is this still readable? Since the `case` expression did not change,
the answer should still be "yes".

The price to be paid for the readability of the `case` expression is
the unreadability of the definition of the lambda expressions. There
may be special cases where using case equality for proc is a good
idea, but in general, it seems to sacrifices simplicity for a some
sort of elegance. This becomes even more apparent when looking at
another example from above.

```ruby
[[:zero, 0], [:one, 1], [:two, 2], [:three, 3]].each do |(name, residue)|
  define_method(name) { -> (n) { n % 4 == residue } }
end

case 3
when zero then puts 'divisile'
when one then puts 'residue 1'
when two then puts 'residue 2'
when three then puts 'residue 3'
else puts 'impossible'
end
```

There is a big discrepancy in perceived "complexity" between the apparent
simplicity of the `case` expression and the definition of the lambdas used in
the `case` expression.

# Changeability

Let's have a look at another example. Suppose that `is_prime` is some
method that returns `true` if the argument passed in is a prime number
and `false` otherwise. (Note that the definition of `is_prime` below
is not correct for arbitrary input.)

```ruby
def is_prime?(n)
  [11, 13, 17, 19].include?(n)
end

def small?
  -> (n) { n < 10 }
end

def prime?
  -> (n) { is_prime?(n) }
end

case 13
when small? then puts 'small'
when prime? then puts 'prime'
else puts 'composite'
end
```

The reason why the `case` expression in this example is easy to read
is that we *believe* we understand what it does. And Ruby is built in
a way that our believe matches implementation.  Now suppose we want to
extend the `case` expression so that it returns "negative" for
negtative input. Easy. Just write

```ruby
case 13
when negative? then puts 'negative'
when small? then puts 'small'
when prime? then puts 'prime'
else puts 'composite'
end
```

The only part that is missing is that `negative?` thing that (as we
remember) must be or return a proc object that accepts a `call` method
with one integer argument.
