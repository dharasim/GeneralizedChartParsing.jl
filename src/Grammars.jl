# from standard library
using Distributed
using SpecialFunctions: digamma
using LinearAlgebra:    normalize, norm

using LogProbs
# using FastClosures
# using DataFrames
using Distributions: Dirichlet
using Setfield

# imports
import Base: zero, one, +, *
import Base: IteratorSize, eltype, iterate
import Base: parse, rand, merge, convert, show

# # exports
# export NamedFunction
# export Grammar, grammar_from_string, train_grammar
# export add_score, set_scores
# export add_random_prob_score, add_expected_count_score, add_forest_score
# export count_score, enum_forest_score, forest_score
# export tree_struct, tree_structs, expected_counts_dataframe

# include("utils.jl")
include("LazyDicts.jl")
# include("newtrees.jl")
# include("Grammars.jl")
include("scores.jl")
include("ScoredForests.jl")
# include("grammar_from_string.jl")
# include("variational_bayes.jl")

# using .Trees
using .LazyDicts
using .ScoredForests

######################
### NamedFunctions ###
######################

struct NamedFunction{F} <: Function
    name :: String
    f    :: F
end

show(io::IO, nf::NamedFunction) = print(io, nf.name)
(nf::NamedFunction)(args...)  = (nf.f)(args...)

##########################
### Tabulate Functions ###
##########################

tabulate(f, xs)         = [f(x) for x in xs]
tabulate(f, xs, ys)     = [f(x, y) for x in xs, y in ys]
tabulate(f, xs, ys, zs) = [f(x, y, z) for x in xs, y in ys, z in zs]

##################################
### Equivalence Classification ###
##################################

function equivalence_classes(f, n::Int)
    classes   = zeros(Int, n)
    n_classes = 0 # number of distinct classes

    for i in 1:n
        classified = false
        for j in 1:i-1
            if f(i) == f(j)
                classes[i] = classes[j]
                classified = true
                break
            end
        end
        if !classified
            n_classes += 1
            classes[i] = n_classes
        end
    end

    classes
end

################
### Grammars ###
################

struct Grammar{S}
    categories         :: Vector
    terminals          :: Vector
    rules              :: Vector
    unary_completions  :: Vector{Vector{Tuple{Int,Int}}}
    binary_completions :: Matrix{Vector{Tuple{Int,Int}}}
    scores             :: Matrix{S}
    classes            :: Vector{Int}
end

function Grammar(categories, terminals, rules, score, classify_nt=identity)
    Grammar(categories, terminals, rules, tabulate(score, categories, rules), classify_nt)
end

function Grammar(categories, terminals, rules, scores::Matrix, classify_nt=identity)
    cats_and_terms = [categories; terminals]

    # category ID
    cid(category) = findfirst(isequal(category), cats_and_terms) :: Int

    unary_completions  = [Tuple{Int,Int}[] for c in cats_and_terms]
    binary_completions = [Tuple{Int,Int}[] for c1 in cats_and_terms, c2 in cats_and_terms]

    for (c, category) in enumerate(categories)
        for (r, rule) in enumerate(rules)
            rhs = rule(category)
            if rhs === nothing
                continue
            elseif rhs isa Tuple
                @assert length(rhs) == 2
                push!(binary_completions[cid(rhs[1]), cid(rhs[2])], (c, r))
            else
                push!(unary_completions[cid(rhs)], (c, r))
            end
        end
    end

    classes = equivalence_classes(length(categories)) do i
        classify_nt(categories[i])
    end

    Grammar(
        collect(categories), collect(terminals), collect(rules),
        unary_completions, binary_completions, scores, classes
    )
end

function transform_scores(f, grammar::Grammar)
    @set grammar.scores = f(grammar.scores)
end

cid(grammar::Grammar, category) =
    findfirst(isequal(category), [grammar.categories; grammar.terminals]) :: Int

completions(grammar::Grammar, c::Int) = grammar.unary_completions[c]
completions(grammar::Grammar, c1::Int, c2::Int) = grammar.binary_completions[c1, c2]
scores(grammar::Grammar) = grammar.scores
score(grammar::Grammar, c::Int, r::Int) = scores(grammar)[c, r]

function Base.parse(grammar::Grammar{S}, sequence) where S
    insert!(cell, c, s) = if haskey(cell, c)
        cell[c] += s
    else
        cell[c]  = s
    end

    initial_chart = Dict(
        (i, i) => Dict(cid(grammar, x) => one(S))
        for (i, x) in enumerate(sequence)
    )

    for i in eachindex(sequence)
        cell = initial_chart[i, i]
        for k in keys(cell)
            for (c, r) in completions(grammar, k)
                insert!(cell, c, score(grammar, c, r))
            end
        end
    end

    chart = LazyDict(initial_chart) do i, j
        cell = valtype(initial_chart)()
        for k in i : j-1
            for (c1, s1) in chart[i, k]
                for (c2, s2) in chart[k+1, j]
                    for (c, r) in completions(grammar, c1, c2)
                        s = score(grammar, c, r) * s1 * s2
                        insert!(cell, c, s)
                        for (uc, ur) in completions(grammar, c)
                            insert!(cell, uc, score(grammar, uc, ur) * s)
                        end
                    end
                end
            end
        end
        cell
    end
end

double(x) = (x, x)
countup(x) = x <= 9 ? (x, x+1) : nothing
terminate(x) = string(x)
g = Grammar(1:10, map(string, 1:10), [double, countup, terminate], count_score)

parse(g, split("1 1 2 3 2"))[1,5][1]

h = transform_scores(forest_scores ∘ random_prob_scores, g)

using TikzQTrees
tree_struct(h.categories, h.terminals, h.rules, rand(parse(h, split("1 2 3 1 1"))[1,5][1])) |> TikzQTree







g = let r1(x)      = ("S", "S"),
        r2(x)      = "a",
        r3(x)      = "b",
        categories = ["S"],
        terminals  = ["a", "b"],
        rules      = [r1, r2, r3],
        counts     = tabulate(count_score, categories, rules),
        probs      = random_prob_scores(counts),
        scores     = zip( (priorcount=counts, prob=probs, forest=forest_scores(probs)) )
    Grammar(categories, terminals, rules, scores)
end

using Test, Random
dataset = let terminals = [fill("a", 3); fill("b", 7)]
    [terminals[randperm(10)] for i in 1:100]
end

loglikelihoods = Float64[]

include("variational_bayes.jl")
g, p = train_grammar(g, dataset)

for i in 1:5
    global g, p = train_grammar(g, dataset)
    push!(loglikelihoods, log(p))
end

true_parameters = [9 / 19, 3 / 19, 7 / 19]
inferred_parameters = vec(map(s->float(s.prob), g.scores))

@test norm(true_parameters - inferred_parameters) < 0.01
