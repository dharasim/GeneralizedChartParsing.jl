params = ["T1" * foldr(*, [",T$i" for i in 2:n], init="") for n in 1:20]

for ps in params
    eval(Meta.parse("zero(::Type{Tuple{$ps}}) where {$ps} = map(zero, ($ps,))"))
    eval(Meta.parse("one( ::Type{Tuple{$ps}}) where {$ps} = map(one,  ($ps,))"))

    eval(Meta.parse(
        "zero(::Type{NamedTuple{names, Tuple{$ps}}}) where {names, $ps} = NamedTuple{names}(map(zero, ($ps,)))"
    ))
    eval(Meta.parse(
        "one( ::Type{NamedTuple{names, Tuple{$ps}}}) where {names, $ps} = NamedTuple{names}(map(one,  ($ps,)))"
    ))
end

+(a::T, b::T) where T <: Union{Tuple, NamedTuple} = map(+, a, b)
*(a::T, b::T) where T <: Union{Tuple, NamedTuple} = map(*, a, b)

###################
### Count Score ###
###################

count_score(category, rule) = rule(category) === nothing ? 0 : 1

#######################
### Transform Score ###
#######################

# f :: Category × Rule × OldScore -> NewScore
function transform_score(f, grammar::Grammar)
	transform_completions(comps) = map(comps) do comp
		c, r, s = comp.cid, comp.rid, comp.score
		Completion(c, r, f(grammar.categories[c], grammar.rules[r], s))
	end

	Grammar(
		grammar.categories,
		grammar.rules,
		map(transform_completions, grammar.unary_completions),
		map(transform_completions, grammar.binary_completions)
	)
end

#############################
### Expected Counts Score ###
#############################

# function expected_count_score(g::Grammar, base_score_name::Symbol)
#     @closure function get_expected_counts(catidx, ruleidx)
#         dcompidx = g.catidx2dcompidx[catidx]
#         p        = getfield(score(g, catidx, ruleidx), base_score_name)
#         m, n     = length(g.depcomps), length(g.all_rules)
#         c        = sparse([dcompidx], [ruleidx], p, m, n)
#         ExpectedCounts(p, c)
#     end
# end
#
# function add_expected_count_score(
#         g::Grammar, base_score_name::Symbol, score_name::Symbol=:exp_counts
#     )
#     add_score(g, score_name, expected_count_score(g, base_score_name))
# end
#
# struct ExpectedCounts{m, n, S, A <: AbstractMatrix{S}}
#     inside_score    :: S
#     expected_counts :: A
#
#     function ExpectedCounts(inside_score::S, expected_counts::A) where
#             {S, A <: AbstractMatrix{S}}
#         m, n = size(expected_counts)
#         new{m, n, S, A}(inside_score, expected_counts)
#     end
# end
#
# function expected_counts_dataframe(
#         g::Grammar, ec::ExpectedCounts{m, n, S, SparseMatrixCSC{S, Int}}
#     ) where {m ,n, S}
#
#     M = ec.expected_counts / ec.inside_score
#     dcompidxs, ruleidxs, counts = findnz(M)
#     DataFrame(
#         dcomp = g.depcomps[dcompidxs],
#         rule  = g.all_rules[ruleidxs],
#         count = counts
#     )
# end
#
# function zero(::Type{ExpectedCounts{m, n, S, Matrix{S}}}) where {m ,n, S}
#     ExpectedCounts(zero(S), zeros(S, m, n))
# end
#
# function one(::Type{ExpectedCounts{m, n, S, Matrix{S}}}) where {m ,n, S}
#     ExpectedCounts(one(S), zeros(S, m, n))
# end
#
# function zero(::Type{ExpectedCounts{m, n, S, SparseMatrixCSC{S, Int}}}) where {m ,n, S}
#     ExpectedCounts(zero(S), spzeros(S, m, n))
# end
#
# function one(::Type{ExpectedCounts{m, n, S, SparseMatrixCSC{S, Int}}}) where {m ,n, S}
#     ExpectedCounts(one(S), spzeros(S, m, n))
# end
#
# function +(c::ExpectedCounts, d::ExpectedCounts)
#     ExpectedCounts(c.inside_score    + d.inside_score,
#                    c.expected_counts + d.expected_counts)
# end
#
# function *(c::ExpectedCounts, d::ExpectedCounts)
#     p, q = c.inside_score, d.inside_score
#     ExpectedCounts(p * q, p * d.expected_counts + q * c.expected_counts)
# end

#########################
### Random Prob Score ###
#########################

function zero_robust_dirichlet_sample(a::AbstractVector)
    non_zero_idxs = findall(x -> x > 0, a)
    probs = rand(Dirichlet(a[non_zero_idxs]))
    b = fill(zero(LogProb), length(a))
    b[non_zero_idxs] = probs
    b
end

function sample_dirichlet_rows(M::AbstractMatrix)
    mapslices(zero_robust_dirichlet_sample, M, dims=2)
end

function random_prob_score(g::Grammar, base_score_name::Symbol)
    params = collect(
        getfield(score(g, catidx, ruleidx), base_score_name)
        for catidx  in findallin(unique(g.depcomp, g.categories), g.categories),
            ruleidx in eachindex(g.all_rules)
    )

    probs = sample_dirichlet_rows(params)

    @closure (catidx, ruleidx) -> probs[g.catidx2dcompidx[catidx], ruleidx]
end

function add_random_prob_score(
        g::Grammar, base_score_name::Symbol, score_name::Symbol=:prob
    )
    add_score(g, score_name, random_prob_score(g, base_score_name))
end

##########################
### Enumerated Forests ###
##########################

enum_forest_score(catidx, ruleidx) = EnumForest([[(catidx, ruleidx)]])

struct EnumForest
    trees :: Vector{Vector{Tuple{Int, Int}}}
end

zero(::Type{EnumForest}) = EnumForest(Vector{Tuple{Int, Int}}[])
one(::Type{EnumForest})  = EnumForest([Tuple{Int, Int}[]])

function +(f::EnumForest, g::EnumForest)
    EnumForest( [f.trees; g.trees] )
end
function *(f::EnumForest, g::EnumForest)
    EnumForest( [[tf; tg] for tf in f.trees for tg in g.trees] )
end

######################
### Scored Forests ###
######################

function add_forest_score(
        g::Grammar, base_score_name::Symbol, score_name::Symbol=:forest)
    add_score(g, score_name, forest_score(getfield(g.scores, base_score_name)))
end

forest_score(score) = function (catidx, ruleidx)
    [RuleApp(catidx, ruleidx, score(catidx, ruleidx))]
end

struct RuleApp{S}
    catidx   :: Int
    ruleidx  :: Int
    children :: Vector{Vector{RuleApp{S}}}
    score    :: S
end

function RuleApp(catidx, ruleidx, score::S) where S
    RuleApp(catidx, ruleidx, Vector{Vector{RuleApp{S}}}(), score)
end

ScoredForest{S} = Vector{RuleApp{S}}
score(f::ScoredForest) = sum(ra.score for ra in f)

zero(::Type{ScoredForest{S}}) where S = RuleApp{S}[]

+(f::ScoredForest, g::ScoredForest) = [f; g]
+(f::ScoredForest, ra::RuleApp) = [f; [ra]]

function *(f1::ScoredForest, f2::ScoredForest)
    [ra * f2 for ra in f1]
end

function *(ra::RuleApp, f::ScoredForest)
    RuleApp(
        ra.catidx,
        ra.ruleidx,
        [ra.children; [f]],
        ra.score * score(f)
    )
end

#############################
### Random Tree Iterators ###
#############################

struct RandomParseTreeIterator{S}
    f :: ScoredForest{S}
end

rand(f::ScoredForest) = RandomParseTreeIterator(f)

IteratorSize(::Type{RandomParseTreeIterator{S}}) where S = Base.SizeUnknown()
eltype(::Type{RandomParseTreeIterator{S}}) where S = Tuple{Int, Int}

function iterate(
        iter::RandomParseTreeIterator,
        state = [categorical_sample(iter.f, getfield.(iter.f, :score))]
    )
    if isempty(state)
        nothing
    else
        ra, ras = state[1], state[2:end]
        new_ras = map(ra.children) do f
            categorical_sample(f, getfield.(f, :score))
        end
        (ra.catidx, ra.ruleidx), prepend!(ras, new_ras)
    end
end

"""
    tree_struct(grammar, categories_and_ruleids)

convert a parse tree represented by a list of tuples of categories and rules to
a tree data structure representation
"""
function tree_struct(grammar, catidxs_and_ruleidxs)
    function apply_rules!(nodes, rules)
        if isempty(rules)
            nothing
        else
            r, rs = rules[1], rules[2:end]
            n, ns = nodes[1], nodes[2:end]
            if n.value in grammar.categories && isapplicable(r, n.value)
                for c in r(n.value)
                    add_child!(n, c)
                end
                apply_rules!([children(n); ns], rs)
            else
                apply_rules!(ns, rules)
            end
        end
        nothing
    end

	category = grammar.categories[catidxs_and_ruleidxs[1][1]]
	root     = Tree(category, Any)

	ruleidxs = getindex.(catidxs_and_ruleidxs, 2)
	rules    = grammar.all_rules[ruleidxs]

    apply_rules!([root], rules)
    root
end

function tree_structs(g::Grammar, f::EnumForest)
    tree_struct.(Ref(g), f.trees)
end
