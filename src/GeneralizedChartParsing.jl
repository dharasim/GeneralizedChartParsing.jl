__precompile__()

module GeneralizedChartParsing

# from standard library
using Distributed
using SpecialFunctions: digamma
using SparseArrays:     SparseMatrixCSC, sparse, spzeros, findnz
using LinearAlgebra:    normalize

# other packages
using LogProbs
using FastClosures
using DataFrames
using Distributions: Dirichlet

# imports
import Base: zero, one, +, *
import Base: IteratorSize, eltype, iterate
import Base: parse, rand, merge, convert, show

# exports
export NamedFunction
export categorical_sample
export Grammar, grammar_from_string
export add_score, set_scores
export add_random_prob_score, add_expected_count_score, add_forest_score
export count_score, enum_forest_score, forest_score
export tree_struct, tree_structs, expected_counts_dataframe

include("utils.jl")
include("LazyDicts.jl")
include("newtrees.jl")
include("Grammars.jl")
include("scores.jl")
include("grammar_from_string.jl")
include("variational_bayes.jl")

using .Trees
using .LazyDicts

end # module
