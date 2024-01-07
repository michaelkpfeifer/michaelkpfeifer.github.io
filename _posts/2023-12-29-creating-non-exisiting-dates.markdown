---
layout:     post
title:      Creating Non-Existing Dates
date:       Sat Dec 30 09:55:11 PM CET 2023
categories: Programming
---
# Introduction

Dealing with dates and times in programming is often thought to be
simple. It isn't. Time zones are already difficult enough. But there
are daylight savings time, leap years and leap seconds. Things can get
very complicated.

Some programming languages make their users happy (or unhappy) by
allowing them to schedule meetings on 2021-02-31 08:00 UTC. Other
programming languages complain when they are told to create dates from
invalid input.

The purpose of this little article is to get an overview of how
different programming languages behave when they are asked to create
some data structure representing the date 2021-02-31 or the time
2021-02-31 08:00 UTC. (I don't want to deal with time zones.)

I am not an expert in most of the programming languages i will be
talking about.  If you find an error, or if I missed some interesting
aspect, or if I oversimplified and created a wrong impression, please
let me know.

This is not a rating of programming programming languages. It is not
my intention to say that one language handles dates and times better
than some other language.

When I started to work on this article, I believed that it should not
take too long to have a quick look at the documentation of each
language and write down a few examples. I believed that in each
language there would be only one way for dealing with dates and times
and that this way should be easy to find. Not true. The more I read,
the more I wondered whether what I wrote down was actually an
idiomatic way of dealing with dates and times in the respective
languages.

The examples below are merely examples. They show *one* way for
creating dates and times.  But this may not even be an idiomatic way.

# Elixir

Elixir has a notion of dates, times, and datetimes (i.e. time stamps).
Dates can be created using `Date.new/3`, times can be created using
`Time.new/3` and time stamps can be created using `DateTime.new/4`.

{% highlight elixir %}
# Elixir Date.new/3 with valid input
date = Date.new(2021, 1, 15)

# "IO.inspect(date)" prints {:ok, ~D[2021-01-15]}
{% endhighlight %}

{% highlight elixir %}
# Elixir Date.new/3 with invalid input
date = Date.new(2021, 2, 30)

# "IO.inspect(date)" prints {:error, :invalid_date}
{% endhighlight %}

Creating a time stamp requires a date and a time.

{% highlight elixir %}
# Elixir DateTime.new/4 with valid input
{:ok, date} = Date.new(2021, 1, 15)
{:ok, time} = Time.new(8, 0, 0)
time_stamp = DateTime.new(date, time)

# "IO.inspect(time_stamp)" prints {:ok, ~U[2021-01-15 08:00:00Z]}
{% endhighlight %}

When no time zone is specified in the `DateTime.new/4` function,
Elixir assumes UTC.

There is no way to write down the code that would lead to an invalid
time stamp because we would need to pass in an invalid date. And, as
we have seen, we cannot create invalid dates.

# Erlang

Erlang is an interesting language with respect to date and time
manipulation. Dates and times are not constructed using some method or
function. They are simply data structures.

A date value is represented by a tuple consisting of year, month, and
date. The [documentation of the calendar
module](https://erlang.org/doc/man/calendar.html) states that "the
date tuple must denote a valid date".

A time value is represented by a tuple consisting ot hour, minute, and
second.

A time stamp is represented by a tuple consisting of a date and a
time. There is no room for a time zone in this data structure. Such a
time stamp represents a time in a timezone that is implicit to the
application.

Creating valid and invalid dates is exceptionally simple.

{% highlight erlang %}
%% Erlang valid date as tuple
Date = {2021, 1, 15}.

%% "io:format("~p~n", [Date])." prints {2021,1,15}
{% endhighlight %}

{% highlight erlang %}
%% Erlang invalid date as tuple
Date = {2021, 2, 30}.

%% "io:format("~p~n", [Date])." prints {2021,2,30}
{% endhighlight %}

Erlang provides the `calendar:valid_date/1` and
`calendar:valid_date/3` functions to validate that a given date is
actually valid.

Let's see what happens if we attempt to do artihmetic with invalid
dates. We will try to add one day to 2021-1-15 and to 2021-2-30.

{% highlight erlang %}
%% Erlang valid date as tuple
Date = {2021, 1, 15}.
DatePlus1 =
  calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + 1).

%% "io:format("~p~n", [DatePlus1])." prints {2021,1,16}
{% endhighlight %}

{% highlight erlang %}
 %% Erlang invalid date as tuple
 Date = {2021, 2, 30}.
 try
   DatePlus1 =
     calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + 1)
 catch
   error -> io:format("~p~n", error)
 end.

 %% prints
 %% ** exception error: no true branch found when evaluating an if expression
 %%      in function  calendar:date_to_gregorian_days/3 (calendar.erl, line 134)
{% endhighlight %}

There is nothing that distinguishes any tuple from a tuple
representing a date. `{2021, 2, 30}` is a perfectly fine tuple but it
is simply not a representation of a date.

Valid time stamps depend on valid dates. There is no need to try to
create a time stamp representing 2021-02-30 08:00. Of course, we can
easily write down a tuple of tuples such as

{% highlight erlang %}
{% raw %}{{2021, 2, 30}, {8, 0, 0}}{% endraw %}
{% endhighlight %}

and insist that this is a representation of an invalid
timestamp. Whether this is true is probably a philosophical
question. (And I would argue that this not a representation of a time
stamp at all.)

Erlang is a little bit unusual in this list of programming languages
since dates and time stamps are created by creating the corresoponding
representation without the help of any constructor functions.

# Haskell

Everything I have learned about dates and times in Haskell is
summarized in the [Haskell Time Library
Tutorial](https://two-wrongs.com/haskell-time-library-tutorial).

Haskell comes with the `Data.Time` module which provides the
`fromGregorian` function. `fromGregorian` takes three arguments: the
year, the month, and the day. Months are counted from 1.
`fromGregorian` returns a value of type `Day`.

{% highlight haskell %}
-- Haskell fromGregorian with valid input
import Data.Time
date = fromGregorian 2021 1 15

-- "print date" prints "2021-01-15"
{% endhighlight %}

{% highlight haskell %}
-- Haskell fromGregorian with invalid input
import Data.Time
date = fromGregorian 2021 2 30

-- "print date" prints "2021-02-28"
{% endhighlight %}

Really? Just return the last day of the month? This is surprising
enough to dig a little bit deeper. I was expecting Haskell to fight
against invalid data. And, indeed, there is a `fromGregorianValid`
function that works similarly to `fromGregorian` but returns a `Maybe
Day` value.

{% highlight haskell %}
-- Haskell fromGregorianValid with invalid input
import Data.Time
maybeDate = fromGregorianValid 2021 2 30

-- "print maybeDate" prints Nothing
{% endhighlight %}

UTC time stamps in Haskell are represented by values of the `UTCTime`
type.  And `UTCTime` values are built from a `Day` value and an offset
that stores how many seconds of the given day have passed.

Creating `UTCTime` values is easy when using `fromGregorian` since
`fromGregorian` returns a Day value.

{% highlight haskell %}
-- Haskell fromGregorian with invalid input
import Data.Time
utcTime = UTCTime (fromGregorian 2021 2 30) (8 * 60 * 60)

-- "print utcTime" prints "2021-02-28 08:00:00 UTC"
{% endhighlight %}

Things become somewhat more complicated when using
`fromGregorianValid` to construct a Day value because
`fromGregorianValid` returns a Maybe Day value.

Note that the `:{` and `:}` tokens in the code fragments below do not
really belong to the code. The are used to allow line breaks in GHCi,
the interactive interface to the Glasgow Haskell compiler.

{% highlight haskell %}
-- Haskell fromGregorianValid with valid input
import Data.Time
:{
maybeUtcTime = case (fromGregorianValid 2021 1 15) of
                 Nothing -> Nothing
                 Just day -> Just $ UTCTime day (8 * 60 * 60)
:}

-- "print maybeUtcTime" prints "Just 2021-01-15 08:00:00 UTC"
{% endhighlight %}

{% highlight haskell %}
-- Haskell fromGregorianValid with invalid input
import Data.Time
:{
maybeUtcTime = case (fromGregorianValid 2021 2 30) of
                 Nothing -> Nothing
                 Just day -> Just $ UTCTime day (8 * 60 * 60)
:}

-- "print maybeUtcTime" prints Nothing
{% endhighlight %}






# Java

The [Java SE 8 Date and
Time](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
documentation seems to be a useful introduction to date and time
handling in Java.

Java ships with the `LocalDate` class whose purpose is to represent
dates without time zones.  Exactly what we want for the first test.

Dates can be created using the `LocalDate.of` method, passing in year,
month, and day. Months are counted from 1, so 1 corresponds to January
and 12 corresponds to December.

{% highlight java %}
// Java Localdate.of with valid input
import java.time.LocalDate;

class SomeDate {
    public static void main(String[] args) {
        LocalDate someDate = LocalDate.of(2021, 1, 15);
        System.out.println(someDate);
    }
}

// prints "2021-01-15"
{% endhighlight %}

{% highlight java %}
// Java Localdate.of with invalid input
import java.time.LocalDate;

class SomeDate {
    public static void main(String[] args) {
        try {
            LocalDate someDate = LocalDate.of(2021, 2, 30);
            System.out.println(someDate);
        }
        catch(Exception e) {
            System.out.println(e);
        }
    }
}

// raises a "java.time.DateTimeException: Invalid date 'FEBRUARY 30'" exception
{% endhighlight %}

Instances of the `LocalDateTime` class represent time stamps in local
time. They do not have any notion of a time zone (so they are not too
useful when trying to create an object that represents 2021-02-31
08:00 UTC. Fortunately, there is the `ZonedDateTime` class that adds
time time zones to `LocalDateTime`.

{% highlight java %}
// Java LocaldateTime.of with valid input
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.time.ZoneId;

class SomeTime {
    public static void main(String[] args) {
        LocalDateTime someLocalTime = LocalDateTime.of(2021, 1, 15, 8, 0, 0);
        ZonedDateTime someZonedTime =
            ZonedDateTime.of(someLocalTime, ZoneId.of("UTC"));
        System.out.println(someZonedTime);
    }
}

// prints "2021-01-15T08:00Z[UTC]"
{% endhighlight %}

{% highlight java %}
// Java LocaldateTime.of with invalid input
import java.time.LocalDateTime;
import java.time.ZonedDateTime;
import java.time.ZoneId;

class SomeTime {
    public static void main(String[] args) {
        try {
            LocalDateTime someLocalTime = LocalDateTime.of(2021, 2, 30, 8, 0, 0);
            ZonedDateTime someZonedTime =
                ZonedDateTime.of(someLocalTime, ZoneId.of("UTC"));
            System.out.println(someZonedTime);
        } catch(Exception e) {
            System.out.println(e);
        }
    }
}

// raises a "java.time.DateTimeException: Invalid date 'FEBRUARY 30'" exception
{% endhighlight %}

In some sense, Java does exactly what is expected from the big
enterprise language. It simply refuses to create invalid dates and
expects the user to handle the resulting errors.

# JavaScript

JavaScript does not distinguish between dates and times.  The
JavaScript `Date` object represents a single moment in
time. Internally, it stores milliseconds since 1 January 1970 UTC.

`Date` objects can be created by passing year, month, and day into the
`Date` constructor. Months are counted from 0, so 0 represents January
and 11 represents December.

{% highlight js %}
// new Date with valid input
const date = new Date(2021, 0, 15);

// "console.log(date.toDateString());" prints "Fri Jan 15 2021"
{% endhighlight %}

{% highlight js %}
// new Date with invalid input
const date = new Date(2021, 1, 30);

// "console.log(date.toDateString());" prints "Tue Mar 02 2021"
{% endhighlight %}

Since JavaScript does not distinguish between dates and times, it
would be very surprising to see a different behavior when passing in
hour, minutes, and seconds in addition to year, month, and day.

Note that the examples below use `Date.UTC()` method to compute the
number of milliseconds since January 1, 1970, 00:00:00 UTC.  This
number is then then be passed into the `Date` constructor.

{% highlight js %}
// new Date with valid input
const date = new Date(Date.UTC(2021, 0, 15, 8, 0, 0));

// "console.log(date.toISOString());" prints 2021-01-15T08:00:00.000Z
{% endhighlight %}

{% highlight js %}
// new Date with invalid input
const date = new Date(Date.UTC(2021, 1, 30, 8, 0, 0));

// "console.log(date.toisostring());" prints 2021-03-02t08:00:00.000z
{% endhighlight %}

Apparently, JavaScript wants to spare its users from negative
experiences in the browser.

# Ruby

Ruby provides a `Date`, a `DateTime`, and a `Time` class.

`Date` objects store simple dates without seconds or time zones.

`Time` objects store time stamps.

And `DateTime` objects are deprecated (compare [class
DateTime](https://docs.ruby-lang.org/en/master/DateTime.html)), so we
will look at `Date` and `Time`.

The constructor of the `Date` class takes three arguments: year,
month, date. Months are indexed from 1.

{% highlight ruby %}
# Date.new with valid input
require 'date'
date = Date.new(2021, 1, 15)

# "puts date" prints "2021-01-15"
{% endhighlight %}

{% highlight ruby %}
# Date.new with invalid input
require 'date'
date = begin
  Date.new(2021, 2, 30)
rescue => e
  e
end

# "puts date" prints "#<Date::Error: invalid date>"
{% endhighlight %}

And now, UTC timestamps. The `Time.utc` class method takes the six
expected arguments in the expected order and returns time a stamp in
the UTC time zone.

{% highlight ruby %}
# Time.utc with valid input
require 'date'
timestamp = Time.utc(2021, 1, 15, 8, 0, 0)

# "puts timestamp" prints "2021-01-15 08:00:00 UTC"
{% endhighlight %}

{% highlight ruby %}
# Time.utc with invalid input
require 'date'
timestamp = Time.utc(2021, 2, 30, 8, 0, 0)

# "puts timestamp" prints "2021-03-02 08:00:00 UTC"
{% endhighlight %}

`Date.new` raises an exception where `Time.utc` happily creates some
time stamp in the following month.

# Summary

The following table is a collection of the results.

|-------------------------------+-----------------------------------------------+
| Programming language and task | Outcome when creating invalid date or time    |
|-------------------------------+-----------------------------------------------+
| Elixir Date                   | returns error                                 |
| Elixir UTC Time               | impossible to create                          |
| Erlang Date                   | unusable syntactically correct representation |
| Erlang UTC Time               | unusable syntactically correct representation |
| Haskell Date                  | provides two implementations                  |
| Haskell UTC Time              | depends on construction date values           |
| Java Date                     | raises exception                              |
| Java UTC Time                 | raises exception                              |
| JavaScript Date               | rolls over to some valid date                 |
| JavaScript UTC Time           | rolls over to some valid time stamp           |
| Ruby Date                     | raises exception                              |
| Ruby UTC Time                 | rolls over to some valid time stamp           |
|-------------------------------+-----------------------------------------------+

# Conclusion

All the programming languages mentioned above agree that it is a good
idea to be able to have some data structure representing dates and
times and that there should be some functions or methods that can do
artihmetic operations on dates and times. Nevertheless, there is a lot
of diversity in the different implementations and APIs.

All of the programming languages listed above differ in one aspect or
another. There is no obvious common API. There are no universal
truths.
