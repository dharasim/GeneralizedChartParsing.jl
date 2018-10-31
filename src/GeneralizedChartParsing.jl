module GeneralizedChartParsing

# from standard library
using Distributed
using SpecialFunctions: digamma
using LinearAlgebra: normalize, norm

# from other packages
using LogProbs
using Distributions: Dirichlet
using Setfield
using FastClosures

# imports
import Base: zero, one, +, *
import Base: IteratorSize, eltype, iterate
import Base: parse, rand, merge, convert, show

# exports
export NamedFunction, tabulate, rule_from_dict, make_rule
export Grammar, train_grammar
export count_score, random_prob_scores, forest_scores
export tree_struct

include("utils.jl")
include("LazyDicts.jl")
include("Grammars.jl")
include("scores.jl")
include("ScoredForests.jl")
include("variational_bayes.jl")

using .LazyDicts
using .ScoredForests

end # module
