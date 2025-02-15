---
layout:     post
title:      On the Expression Problem in Functional Programming
date:       Sat Feb 15 03:46:24 PM CET 2025
categories: Programming
---

# The Expression Problem in Object Oriented Programming

A simple example often used for explaining the expression problem in
object oriented programming is the implementation of an abstract shape
class that has concrete sub classes dealing with individual shapes.

In Ruby, something like this could be implemented as follows.

```ruby
class Shape
  def initialize
    raise NotImplementedError
  end
end

class Square < Shape
  def initialize(side)
    @side = side
  end

  def area
    @side * @side
  end
end

class Circle < Shape
  def initialize(radius)
    @radius = radius
  end

  def area
    Math::PI * @radius * @radius
  end
end
```

Adding a new shape is simple. It is sufficient to add the `Rectangle`
class and its `area` method.

```ruby
class Rectangle < Shape
  def initialize(a, b)
    @a = a
    @b = b
  end

  def area
    @a * @b
  end
end
```

If we now want to add a `circumfernce` method for the shapes, we have
defined so far, we have to open every class.

```ruby
class Shape
  def initialize
    raise NotImplementedError
  end
end

class Square < Shape
  def initialize(side)
    @side = side
  end

  def area
    @side * @side
  end

  def circumference
    4 * @side
  end
end

class Circle < Shape
  def initialize(radius)
    @radius = radius
  end

  def area
    Math::PI * @radius * @radius
  end

  def circumference
    2 * Math::PI * @radius
  end
end

class Rectangle < Shape
  def initialize(a, b)
    @a = a
    @b = b
  end

  def area
    @a * @b
  end

  def circumference
    2 * (@a + @b)
  end
end
```

In particular, the `area` and `circumference` methods are defined in
three different places. And every shape that is added makes things
worse. This is known as the [expression
problem](https://en.wikipedia.org/wiki/Expression_problem)

# An Implementation in a Functional Programming Language

In a functional programming language such as Elm, we could model
shapes as a custom type. As in the object oriented case, we begin with
the compuation of the area of squares and circles.

```elm
type Shape
    = Square Float
    | Circle Float


area : Shape -> Float
area shape =
    case shape of
        Square side ->
            side * side

        Circle radius ->
            pi * radius * radius
```


We use pattern matching on the type of the shape in the `area` function
to provide concrete implementations for the area of different
shapes. Instead of adding the reactangle shape first (as we did in the
object oriented case), let's first add the `circumference` function.

```elm
type Shape
    = Square Float
    | Circle Float


area : Shape -> Float
area shape =
    case shape of
        Square side ->
            side * side

        Circle radius ->
            pi * radius * radius


circumference : Shape -> Float
circumference shape =
    case shape of
        Square side ->
            4 * side

        Circle radius ->
            2.0 * pi * radius
```

And now let's add rectangles as the last step.

```elm
type Shape
    = Square Float
    | Circle Float
    | Rectangle Float Float


area : Shape -> Float
area shape =
    case shape of
        Square side ->
            side * side

        Circle radius ->
            pi * radius * radius

        Rectangle a b ->
            a * b


circumference : Shape -> Float
circumference shape =
    case shape of
        Square side ->
            4 * side

        Circle radius ->
            2.0 * pi * radius

        Rectangle a b ->
            2 * (a + b)
```

Adding a new shape requires to add a new case to the implementation of
each function.

# Object Oriented vs. Functional

In object oriented programming, adding a new shape is simple. There
is only one place that needs to be touched: the new class. But
adding a new operation is hard because it must be added to each of
the exisitng classes.

In functional programming, adding a new function is simple. There is
only one place that needs to be touched: the new function. But adding
a new shape is hard because the new shape must be added to each of the
existing functions.

In other words: functional programming solves the object oriented
expression problem at the expense of creating a functional version of
it.

# The Visitor Pattern

The visitor pattern is a solution for the expression problem in object
oriented programming languages. We start by defining an `accept`
method in the base class.

```ruby
class Shape
  def initialize
    raise NotImplementedError
  end

  def accept(visitor)
    raise NotImplementedError
  end
end
```

Strictly speaking, having `initialize` and `accept` in the base class
is not necessary. We only add `initialize` and `accept` to make it a
bit clearer that these methods must be implemented in the child
classes.

In the next step, we implement `accept` for each individual shape (and
we get rid of the `area` and `circumference` methods that we had
already implemented.) We also add accessors for the attributes of the
respective objects since this will make it a bit easier to work with
them.

```ruby
class Square < Shape
  attr_reader :side

  def initialize(side)
    @side = side
  end

  def accept(visitor)
    visitor.visitSquare(self)
  end
end

class Circle < Shape
  attr_reader :radius

  def initialize(radius)
    @radius = radius
  end

  def accept(visitor)
    visitor.visitCircle(self)
  end
end
```

Of course, this does not work yet. We haven't defined any visitors
yet.  We start with the area visitor so that we can later show what to
do when we introduce the computation of circumferences.

```ruby
class ShapeAreaVisitor
  def visitSquare(square)
    square.side * square.side
  end

  def visitCircle(circle)
    Math::PI * circle.radius * circle.radius
  end
end
```

Now we have written a certain amount of code that is much harder to
understand than the initial simple class based implementation. Let's
see how it works. Let's say we want to compute the area of a square.

```ruby
square = Square.new(5)
shape_area_visitor = ShapeAreaVisitor.new
square.accept(shape_area_visitor)
```

Here is an attempt to describe the idea. The `ShapeAreaVisitor` class
collects all the implementations for computing areas of shapes. To
compute the area of a square, we instantiate the `Square` object and
pass an instance of the `ShapeAreaVisitor` into the `accept` method of
the `Square` object. All the `accept` method does is select the
correct method of the `ShapeAreaVisior` instance.

Technically, it is not too difficult to see how the computation of the
area of a square works by following the code. It is much harder to get
an intuition about the idea behind the visitor pattern.

Let's add a method for computing the circumference of shapes.

```ruby
class ShapeCircumferenceVisitor
  def visitSquare(square)
    4 * square.side
  end

  def visitCircle(circle)
    2 * Math::PI * circle.radius
  end
end
```

There is a single place where we have all the circumference
computations and we did not have to touch any of the `Square`, or
`Circle` classes.

We can now use the `ShapeCircumferenceVisitor` as follows.

```ruby
square = Square.new(5)
shape_circumference_visitor = ShapeCircumferenceVisitor.new
square.accept(shape_circumference_visitor)
```

Let's see what happens when we add a new shape. First, we have to
create the new `Rectangle` class giving it an `accept` method similar
to the ones we have seen before.

```ruby
class Rectangle < Shape
  attr_reader :a
  attr_reader :b

  def initialize(a, b)
    @a = a
    @b = b
  end

  def accept(visitor)
    visitor.visitRectangle(self)
  end
end
```

But now we also need to define the `visitRectangle` method in the
`ShapeAreaVisitor` and in the `ShapeCircumferenceVisitor` classes.

```ruby
class ShapeAreaVisitor
  def visitSquare(square)
    square.side * square.side
  end

  def visitCircle(circle)
    Math::PI * circle.radius * circle.radius
  end

  def visitRectangle(rectangle)
    rectangle.a * rectangle.b
  end
end

class ShapeCircumferenceVisitor
  def visitSquare(square)
    4 * square.side
  end

  def visitCircle(circle)
    2 * Math::PI * circle.radius
  end

  def visitRectangle(rectangle)
    2 * (rectangle.a + rectangle.b)
  end
end
```

We see that adding a shape is not easy anymore. We have to add the
class representing the new shape and then we have to go through all of
the visitor classes we have defined and add the corresponding method
for rectangles.

Using the visitor pattern, our implementation behaves as it would
behave in a functional programming language. Adding methods is easy,
adding classes is hard.

# The Expression Problem in Functional Programming

The following is the second challenge in chapter 5 of Robert Nystrom's
[Crafting
Interpreters](https://craftinginterpreters.com/contents.html) book.

"The Visitor pattern lets you emulate the functional style in an
object-oriented language. Devise a complementary pattern for a
functional language. It should let you bundle all of the operations on
one type together and let you define new types easily."

I was barely able to understand the visitor pattern as a solution of
the expression problem in object oriented programming and I had no
idea how to even start solving this problem.

Luckily, the Git repository for Nystrom's book also contains the
solution. If you have been reading this far, you probably don't mind
me showing you the solution as we will try to make sense of it anyway.

"One way is to create a record or tuple containing a function pointer
for each operation. In order to allow defining new types and passing
them to existing code, these functions need to encapsulate the type
entirely -- the existing code isn't aware of it, so it can't type
check. You can do that by having the functions be closures that all
close over the same shared object, 'this', basically."

Unfortunately, I have no idea what this means.

# Type Classes and Protocols

Some functional programming languages provide language support to
allow the definition of functions in different places. For example, in
Haskell, there are type classes, and in Clojure and Elixir, there are
protocols. These are extensions of the respecitve language itself. In
the context of the current article, this is not what I am interested
in. The visitor pattern we showed above works in any object oriented
programming language that has a few basic properties.

# The Solution of the Expression Problem in Functional Programming

Here is again Nystrom's proposed solution. "One way is to create a
record or tuple containing a function pointer for each operation. In
order to allow defining new types and passing them to existing code,
these functions need to encapsulate the type entirely -- the existing
code isn't aware of it, so it can't type check. You can do that by
having the functions be closures that all close over the same shared
object, 'this', basically."

And here is an Elm implementation of what I came up with after
thinking about the problem for quite some time and after numerous
failed attempts. I am not sure that this is the solution that Robert
Nystrom had in mind, but it includes a record of functions and
closures.

We start with a type alias.

```elm
type alias ShapeOperations =
    { area : () -> Float
    }
```

We use this type alias to implement the computation of the area of
squares and circles.

```elm
square : Float -> ShapeOperations
square side =
    { area = \() -> side * side
    }


circle : Float -> ShapeOperations
circle radius =
    { area = \() -> pi * radius * radius
    }
```

The important trick we apply here is that the value returned by the
`square` and `circle` functions closes over the arguments passed in.

This looks somewhat abstract. Here is an expression that returns the
area of a square with a side length of 2: `(square 2).area ()`.

`(square 2)` returns a record. `(square 2).area` selects the function
with the `area` key and `(square 2).area ()` finally applies the
function. Note that in this case `()` is not an empty argument list
but Elm's single representation of the unit type (which is also
denoted `()`).

The reason why a function that does not take any argument is able to
is return the correct value is that it closes over the `side` argument
that we passed in when calling `(square 2)`.

As in the introduction of the visitor pattern in the object oriented
case, we have introduced a lot of code that is somewhat hard to
understand without providing too much obvious benefit.

Let's see what happens when we add rectangles.

```elm
type alias ShapeOperations =
    { area : () -> Float
    }


square : Float -> ShapeOperations
square side =
    { area = \() -> side * side
    }


circle : Float -> ShapeOperations
circle radius =
    { area = \() -> pi * radius * radius
    }


rectangle : Float -> Float -> ShapeOperations
rectangle a b =
    { area = \() -> a * b
    }
```

We simply added the `rectangle` function. We did not touch the
`square` or `circle` functions. Adding the rectangle was easy just
like in object oriented languages.

Finally, let's add the compuation of the circumference of squares,
circles, and rectangles.

```elm
type alias ShapeOperations =
    { area : () -> Float
    , circumference : () -> Float
    }


square : Float -> ShapeOperations
square side =
    { area = \() -> side * side
    , circumference = \() -> 4 * side
    }


circle : Float -> ShapeOperations
circle radius =
    { area = \() -> pi * radius * radius
    , circumference = \() -> 2 * pi * radius
    }


rectangle : Float -> Float -> ShapeOperations
rectangle a b =
    { area = \() -> a * b
    , circumference = \() -> 2 * (a + b)
    }
```

The important thing to note is that we had to add the computation of
the circumference in `square`, `circle`, and `rectangle`. In other
words, adding a function is now hard.

# Conclusion

In the object oriented case, the visitor pattern makes it easy to add
new methods (while making it harder to add new classes).

In the functional case, the kind of inverse visitor pattern introduced
above makes it easy to add new types (while making it harder to add
new functions).

Both cases require a certain amount of overhead and make code harder
to understand. Whether it is worth to introduce this overhead depends
on the use case.
