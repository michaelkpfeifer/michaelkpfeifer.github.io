---
layout:     post
title:      Variations on Pattern Matching in Elixir
date:       Wed Dec 27 03:22:39 PM CET 2023
categories: Elixir
---
One of the more common ways of pattern matching on lists is to
separate the first item of the list from the rest of the items.  In
Elixir, this is done using a vertical bar symbol called the *cons
operator*.

```elixir
list = [1, 2, 3]
[head | tail] = list
```

In this little example, the variable `head` will be bound to `1` and
the variable `tail` will be bound to the list `[2, 3]`.

The cons operator can also be used to build a larger list by adding
items to the front of the given list.

```elixir
head = 1
tail = [2, 3]
list = [head | tail]
```

In this example, `head` is added to the beginning of `tail`. In the
end, the variable `list` is bound to the list `[1, 2, 3]`.

Pattern matching as shown in the first example and list construction
as shown in the second example are inverse operations of each
other. Many languages have ways to take lists or arrays apart and put
them back together. The Elixir and Erlang implementations are
particularly beautiful because pattern matching and list construction
*look* identical.

In Elixir, the methods described above can be easily extended to
matching the first two (or more) list items.

```elixir
[e1, e2 | tail] = [1, 2, 3]
```

Now, `e1` will be bound to `1`, `e2` will be bound to `2`, and `tail`
will be bound to the list `[3]` with a single remaining item..

This way of pattern matching can be used recursively to walk through a
list as shown in the following little function that computes the sum
of consecutive list items. For the list `[1, 2, 3, 4, 5]`, we expect
the result to be `[3, 7]`.  We ignore the last item in the given list
because there is no other number we could add.

```elixir
defmodule Chunk do
  def add_consecutive([e1, e2 | rest]) do
    [e1 + e2 | add_consecutive(rest)]
  end

  def add_consecutive([_]) do
    []
  end

  def add_consecutive([]) do
    []
  end
end

Chunk.add_consecutive([1, 2, 3, 4, 5])
```

In each recursive step, we consume the first two items of the given
list, naming them `e1` and `e2`. The `rest` variable is bound to the
given list without the first two items.  We add `e1` and `e2` and run
`add_consecutive/1` passing in the remaining list items. To stop the
recursion, we return an empty list if `add_consecutive/1` is called
with a list consisting of a single item or with an empty list.

In the next example, we consume only one list item in each recursive
step. We still compute the sum of consecutive items, but this time the
list items overlap. For the list `[1, 2, 3, 4, 5]`, we expect the
result to be `[3, 5, 7, 9]`.

```elixir
defmodule Chunk do
  def add_overlapping([e1, e2 | rest]) do
    [e1 + e2 | add_overlapping([e2 | rest])]
  end

  def add_overlapping([_]) do
    []
  end

  def add_overlapping([]) do
    []
  end
end

Chunk.add_overlapping([1, 2, 3, 4, 5])
```

Again, we take the first two items of the given list naming them `e1`
and `e2` in each recusive step.

The difference between the implementations of `add_consecutive/1` and
`add_overlapping/1` is that in `add_overlapping/1`, we put `e2` back
into the list we are processing. Instead of

```elixir
  [e1 + e2 | add_consecutive(rest)]
```

we write

```elixir
  [e1 + e2 | add_overlapping([e2 | rest])]
```

In hindsight, the implementation of `add_overlapping/1` looks more or
less obvious. However, (at least) two ideas must be present before
being able to write `add_overlapping/1` in the way shown above:

  - we can use pattern matching to take more than one item from the
    head of a given list and

  - we can put items back into the list before running the next
    recursive step.

The two little problems solved above using `add_consecutive/1` and
`add_overlapping/1` can be split in two smaller problems. The first
problem is to create a list of consecutive or overlapping pairs. And
the second problem is to add the numbers in the pairs. For both
problems, Elixir provides solutions out of the box and the solutions
of our little problems become a matter of function composition.

```elixir
defmodule Chunk do
  def sum_consecutive(list) do
    Enum.chunk_every(list, 2, 2, :discard)
    |> Enum.map(fn([fst, lst]) -> fst + lst end)
  end
end

Chunk.sum_consecutive([1, 2, 3, 4, 5])
```

```elixir
defmodule Chunk do
  def sum_overlapping(list) do
    Enum.chunk_every(list, 2, 1, :discard)
    |> Enum.map(fn([fst, lst]) -> fst + lst end)
  end
end

Chunk.sum_overlapping([1, 2, 3, 4, 5])
```

The [documentation of the Elixir Enum
module](https://hexdocs.pm/elixir/Enum.html) explains how exactly
`Enum.chunk_every/4` and `Enum.map/2` operate on lists.
