# GeneralizedChartParsing

[![Build Status](https://travis-ci.org/dharasim/GeneralizedChartParsing.jl.svg?branch=master)](https://travis-ci.org/dharasim/GeneralizedChartParsing.jl)

[![Coverage Status](https://coveralls.io/repos/dharasim/GeneralizedChartParsing.jl/badge.svg?branch=master&service=github)](https://coveralls.io/github/dharasim/GeneralizedChartParsing.jl?branch=master)

[![codecov.io](http://codecov.io/github/dharasim/GeneralizedChartParsing.jl/coverage.svg?branch=master)](http://codecov.io/github/dharasim/GeneralizedChartParsing.jl?branch=master)

This package implements a [semiring parser](https://dash.harvard.edu/bitstream/handle/1/24829603/tr-07-98.pdf?sequence=1) for *Abstract Context-Free Grammars*
It is still in development and thus currently limited to grammars in [Chomsky normal form](https://en.wikipedia.org/wiki/Chomsky_normal_form).

Install the package by typing
```
add https://github.com/dharasim/GeneralizedChartParsing.jl
```

in the Julia package manager prompt.

## Calculating the number of binary trees

To give a simple first demonstration of how to use this library,
we calculate the number of binary trees with `n` leafs
using a simple context-free grammar:

```julia
julia> using GeneralizedChartParsing

julia> g = grammar_from_string("S --> S S | a");

julia> n = 10

julia> scores = parse(g, fill("a", n))
Dict{String,NamedTuple{(:count,),Tuple{Int64}}} with 1 entry:
  "S" => (count = 4862,)

julia> scores["S"].count
4862
```

The first line loads the package. The second line creates a grammar object from
the string `"S --> S S | a"`. This grammar contains two rules, the binary rule
`S --> S S` and the terminal rule `S --> a`. The function `grammar_from_string`
accepts also multiline strings. The first left-hand sind of the first rule in
the string is taken as the start symbol of the grammar.

The function `parse` takes a grammar and a sequence of terminal symbols and
returns a dictionary mapping start symbols of the grammar (here we have only
one start symbol) to the number of parse trees that generate the input sequence.
We can extract this number and get the answer that there are 4862 different
binary trees.

In general, the dictionary returned by the `parse` function maps the start symbols
of the grammar to their semiring values using the score functions of
the grammar. If a sequence is not parsable, zero is returned.

```julia
julia> scores = parse(g, fill("b", n))
Dict{String,NamedTuple{(:count,),Tuple{Int64}}} with 1 entry:
  "S" => (count = 0,)
```

```julia

```



## The canonical `a^n b^n` example
