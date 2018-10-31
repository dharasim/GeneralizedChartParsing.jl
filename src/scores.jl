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

function Base.Iterators.zip(t::NamedTuple{names, T}) where {names, T}
	map(NamedTuple{names}, map(tuple, t...))
end

###################
### Count Score ###
###################

count_score(category, rule) = rule(category) === nothing ? 0 : 1

#########################
### Random Prob Score ###
#########################

function random_prob_scores(pseudocounts::AbstractMatrix)
	function zero_robust_dirichlet_sample(a::AbstractVector)
		non_zero_idxs = findall(x -> x > 0, a)
		probs = rand(Dirichlet(a[non_zero_idxs]))
		b = fill(zero(LogProb), length(a))
		b[non_zero_idxs] = probs
		b
	end

	mapslices(zero_robust_dirichlet_sample, pseudocounts, dims=2)
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
