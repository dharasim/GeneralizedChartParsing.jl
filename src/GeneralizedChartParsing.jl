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
import Base: zero, one, +, *, iszero, isone, isless
import Base: IteratorSize, eltype, iterate
import Base: parse, rand, merge, convert, show

# exports
export NamedFunction, tabulate, rule_from_dict, make_rule
export Grammar, scores, count_score
export random_prob_scores, random_prob_score, forest_scores, forest_score
export Viterbi, greedy_best_tree
export tree_struct, tabulate

include("utils.jl")
include("LazyDicts.jl")
include("Grammars.jl")
include("scores.jl")
include("ScoredForests.jl")
# include("variational_bayes.jl")

using .LazyDicts
using .ScoredForests

end # module
