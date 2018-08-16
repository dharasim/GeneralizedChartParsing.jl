__precompile__()

module GeneralizedChartParsing

	using LogProbs
	using FastClosures
	using DataFrames
	using SparseArrays: SparseMatrixCSC, sparse, spzeros, findnz
	using Distributions: Dirichlet

	import Base: zero, one, +, *
	import Base: IteratorSize, eltype, iterate
	import Base: parse, rand, merge, convert

	export Grammar, grammar_from_string
	export add_score, add_random_prob_score, add_expected_count_score
	export count_score, enum_forest_score, forest_score
	export tree_struct, tree_structs, expected_counts_dataframe

	include("utils.jl")
	include("LazyDicts.jl")
	include("newtrees.jl")
	include("Grammars.jl")
	include("scores.jl")
	include("grammar_from_string.jl")

	using .Trees
	using .LazyDicts

end # module
