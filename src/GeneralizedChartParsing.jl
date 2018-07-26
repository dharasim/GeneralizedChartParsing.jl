module GeneralizedChartParsing

include("newtrees.jl")
include("DynamicDicts.jl")
include("lift_to_tuples.jl")

using AccessMacro, LogProbs
using GeneralizedChartParsing.Trees, GeneralizedChartParsing.DynamicDicts
using Distributions: Dirichlet

import Base: +, *, show, ==, hash, rand, insert!, parse
import Base: start, next, done, length, eltype

export
    Rule, name, isapplicable,
    CNFGrammar, # start_categories, binary_rules, terminal_rules,
    prob, score, Categorical,
	RuleApplication, ParseTreeDistribution, tree_struct,
	mapvalues, mapvalues!,
	catalan_number, number_binary_trees,
	categorical_sample, DirCat, logscore, sample

#################
### Utilities ###
#################

"""
    mapvalues(f, dict)

map a function to the values of a dictionary
"""
function mapvalues(f, d::Dict)
    Dict(k => f(v) for (k,v) in d)
end


"""
    mapvalues(f, dict)

map a function to the values of a dictionary in place
"""
function mapvalues!(f, d::Dict)
    for k in keys(d)
        d[k] = f(d[k])
    end
    d
end

"""
    catalan_number(n)

calculate the n-th catalan number
"""
catalan_number(n) = div(binomial(2n, n), n+1)

"""
    number_binary_trees(n)

calculate the number of binary trees with n leafs
"""
number_binary_trees(n) = catalan_number(n - 1)

"""
    categorical_sample(tokens, weights)

sample one token according to the given weights
"""
function categorical_sample(tokens, weights)
    x = rand() * sum(weights)
    cum_weights = zero(eltype(weights))
    for (t, w) in zip(tokens, weights)
        cum_weights += w
        if cum_weights >= x
            return t
        end
    end
    error("categorical sample failed with length(tokens)=$(length(tokens)), weights=$weights")
end

categorical_sample(d::Dict) = categorical_sample(keys(d), values(d))
categorical_sample(v::Vector) = categorical_sample(1:length(v), v)

"""
    insert!(op, d, k, v)

insert value `v` with key `k` into the dictionary `d`, if `k` already exists
then `op(d[k], v)` is used instead of `v`
"""
function Base.insert!(op::Function, d::Dict{K, V}, k::K, v::V) where {K, V}
    d[k] = haskey(d, k) ? op(d[k], v) : v
    d
end

function Base.insert!(op::Function, d::Dict{K, Vector{V}}, k::K, v::V) where {K, V}
    d[k] = haskey(d, k) ? op(d[k], v) : [v]
    d
end

function Base.insert!(op::Function, d::Dict, k_v)
    k, v = k_v
    insert!(op, d, k, v)
end

###################
### Categorical ###
###################

mutable struct Categorical{T}
	probs :: Dict{T, LogProb}

	function Categorical(params::Dict{T, _}) where {T, _}
		s = sum(values(params))
		new{T}(mapvalues(v->LogProb(v/s), params))
	end
end

sample(c::Categorical) = categorical_sample(probs)
logscore(c::Categorical, x) = c.probs[x]

#############################
### Dirichlet Categorical ###
#############################

mutable struct DirCat{T, C}
    counts :: Dict{T, C}
end

DirCat(support, priors) = DirCat(Dict(x => p for (x,p) in zip(support, priors)))
support(dc::DirCat) = keys(dc.counts)

function sample(dc::DirCat)
    weights = [gammarand(c, 1) for c in values(dc.counts)]
    categorical_sample(keys(dc.counts), weights)
end

function logscore(dc::DirCat, obs)
    LogProb(lbeta(sum(values(dc.counts)), 1) - lbeta(dc.counts[obs], 1))
end

function add_obs!(dc::DirCat, obs)
    dc.counts[obs] += 1
end

function rm_obs!(dc::DirCat, obs)
    dc.counts[obs] -= 1
end

##################
### Rule class ###
##################

mutable struct RunningCounter
	n :: Int
end

RunningCounter() = RunningCounter(0)
count!(c::RunningCounter) = c.n += 1

const RULE_COUNTER = RunningCounter()

struct Rule{N, A, B}
	name :: String
	id   :: Int
	dict :: Dict{A, NTuple{N, B}}
end

==(r1::Rule, r2::Rule) = name(r1) == name(r2)
hash(r::Rule)          = hash(r.name)
name(r::Rule)          = r.name
id(r::Rule) 		   = r.id
show(io::IO, r::Rule)  = print(io, "Rule($(r.name))")

Rule(f::Function, name, lhss) =
	Rule(name, count!(RULE_COUNTER), Dict(lhs => f(lhs) for lhs in lhss))

lhss(r::Rule) = keys(r.dict)
isapplicable(r::Rule, lhs) = haskey(r.dict, lhs)
(r::Rule)(lhs) = r.dict[lhs]

##########################
### ScoredForest class ###
##########################

mutable struct RuleApplication{C, S}
	category :: C
    ruleid   :: Int
    children :: Vector{Vector{RuleApplication{C, S}}}
    score    :: S
end

function RuleApplication(category::C, ruleid, score::S) where {C, S}
    RuleApplication(category, ruleid, Vector{Vector{RuleApplication{C, S}}}(), score)
end

ScoredForest{C, S} = Vector{RuleApplication{C, S}}

+(f::ScoredForest, g::ScoredForest) = append!(f, g)
+(f::ScoredForest, ra::RuleApplication) = push!(f, ra)

function *(f1::ScoredForest, f2::ScoredForest)
    for ra1 in f1
		ra1 * f2
    end
    f1
end

function *(ra::RuleApplication, f::ScoredForest)
	ra.score *= sum(ra_.score for ra_ in f)
	push!(ra.children, f)
	ra
end

#####################
### Grammar class ###
#####################

@access mutable struct CNFGrammar{C, T, R, D, F}
    start_categories :: Vector{C}
    binary_rules     :: Dict{Tuple{C, C}, Vector{Tuple{C, Int}}}
    terminal_rules   :: Dict{Tuple{T}, Vector{Tuple{C, Int}}}
	id_to_rule           :: Dict{Int, R}
    rule_dist            :: D
	dependent_components :: F
end

function CNFGrammar(
		start_categories,
		binary_rules	     ::Vector{Rule{2, C, C}},
		terminal_rules	     ::Vector{Rule{1, C, T}},
		dependent_components;
		initial_probs = :uniform
	) where {C, T}

	function invert_rules(rules::Vector{Rule{N, A, B}}) where {N, A, B}
        foldl(
    	    (d, r) -> begin
    	        for (lhs, rhs) in r.dict
    	            insert!((rs, r) -> append!(rs, r), d, rhs, [(lhs, id(r))])
    	        end
    	        d
    	    end,
    	    Dict{NTuple{N, B}, Vector{Tuple{A, Int}}}(),
    	    rules)
    end

	id_to_rule = Dict(id(r) => r for r in [binary_rules; terminal_rules])

	rule_dist = Dict(
		dependent_components(c) =>
			let applicable_rules = filter(r -> isapplicable(r, c), collect(values(id_to_rule)))
				Categorical(
					if initial_probs == :uniform
						Dict(id(r) => 1 for r in applicable_rules)
					elseif initial_probs == :random
						probs = rand(Dirichlet(length(applicable_rules), 1))
						Dict(id(r) => p for (r, p) in zip(applicable_rules, probs))
					end
				)
			end
		for c in union(map(lhss, values(id_to_rule))...)
	)

	CNFGrammar(
		start_categories,
		invert_rules(binary_rules),
		invert_rules(terminal_rules),
		id_to_rule,
		rule_dist,
		dependent_components
	)
end

logscore(g::CNFGrammar, c, rid) =
	logscore(g.rule_dist[g.dependent_components(c)], rid)

score(g::CNFGrammar, c, rid) =
	[RuleApplication(c, rid, logscore(g, c, rid))], 1

###################################
### ParseTreeDistribution class ###
###################################

struct ParseTreeDistribution{C, S}
    f :: ScoredForest{C, S}
    n :: Int # number of internal nodes of the tree
end

"""
    ParseTreeDistribution(grammar, terminals)

parse the terminals according to the grammar and compactly represent
the distribution over all parse trees of the sequence

This type can be used through the iterator interface, iterating over tuples of
categories and rules of a random tree in left-most order. The implementation
of `legnth` only works for grammars in Chomsky normal form.
"""
function ParseTreeDistribution(grammar::CNFGrammar, terminals)
	p = parse(grammar, terminals)
	@assert !isempty(p)
    ParseTreeDistribution(sum(values(p))[1], 2*length(terminals)-1)
end

"collect a random tree from a parse tree distribution"
rand(d::ParseTreeDistribution) = collect(d)

start(d::ParseTreeDistribution) = [categorical_sample(d.f, getfield.(d.f, :score))]
done(d::ParseTreeDistribution, state) = isempty(state)
length(d::ParseTreeDistribution) = d.n
eltype(::Type{ParseTreeDistribution{C, S}}) where {C, S} = Tuple{C, Int}

function next(d::ParseTreeDistribution, state)
    ra, ras = state[1], state[2:end]
    new_ras = map(ra.children) do f
        categorical_sample(f, getfield.(f, :score))
    end
    (ra.category, ra.ruleid), prepend!(ras, new_ras)
end

"""
    tree_struct(grammar, categories_and_ruleids)

convert a parse tree represented by a list of tuples of categories and rules to
a tree data structure representation
"""
function tree_struct(grammar, categories_and_ruleids)
    function apply_rules!(nodes, rules)
        if isempty(rules)
            nothing
        else
            r, rs = rules[1], rules[2:end]
            n, ns = nodes[1], nodes[2:end]
            if isapplicable(r, n.data)
                for c in r(n.data)
                    add_child!(n, c)
                end
                apply_rules!([children(n); ns], rs)
            else
                apply_rules!(ns, rules)
            end
        end
        nothing
    end

	category = categories_and_ruleids[1][1]
	root     = Tree(category, Any)

	ruleids  = getindex.(categories_and_ruleids, 2)
	rules    = getindex.(grammar.id_to_rule, ruleids)

    apply_rules!([root], rules)
    root
end

####################
### parse method ###
####################

"""
    parse(grammar, terminals)

parse the sequence of terminals according to the grammar, return a dictionary
that maps head categories to their semiring values

The metod first creates the diagonal of the chart as a dictionary,
then updates this dictionary dynammicaly using a `DynamicDict`,
applying dynamic programming under the hood automatically.
"""
function Base.parse(grammar::CNFGrammar, terminals)
    initial_chart = Dict(
        (i,i) => Dict(
            lhs => score(grammar, lhs, r)
            for (lhs, r) in terminal_rules(grammar)[(terminals[i],)]
        )
        for i in eachindex(terminals)
    )

    # should work in v0.7
    # chart = DynamicDict(initial_chart) do i, j
    #     foldl(
    #         (d, k_v) -> insert!(+, d, k_v),
    #         valtype(initial_chart)(),
    #         @comphack eltype(valtype(initial_chart)) [ lhs => (score(grammar, lhs, r) * s1) * s2
    #             for k in i : j-1
    #             for (rhs1, s1) in chart[i,   k]
    #             for (rhs2, s2) in chart[k+1, j]
    #             if haskey(binary_rules(grammar), (rhs1, rhs2))
    #             for (lhs, r) in binary_rules(grammar)[rhs1, rhs2] ] )
    # end

    chart = DynamicDict(initial_chart) do i, j
        d = valtype(initial_chart)() # TODO: rename d to cell
        for k in i : j-1
            for (rhs1, s1) in chart[i, k]
                for (rhs2, s2) in chart[k+1, j]
                    if haskey(binary_rules(grammar), (rhs1, rhs2))
                        for (lhs, r) in binary_rules(grammar)[rhs1, rhs2]
                            pair = lhs => (score(grammar, lhs, r) * s1) * s2
                            insert!(+, d, pair)
                        end
                    end
                end
            end
        end
        d
    end

    filter(chart[1, length(terminals)]) do k, v
        k in start_categories(grammar)
    end
end

end # module
