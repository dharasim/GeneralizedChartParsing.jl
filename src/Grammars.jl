#######################
### Named Functions ###
#######################

struct NamedFunction{F} <: Function
    name :: String
    f    :: F
end

show(io::IO, nf::NamedFunction) = print(io, nf.name)
(nf::NamedFunction)(x)  = (nf.f)(x)

#####################
### Grammar Class ###
#####################

isapplicable(r, c) = r(c) !== nothing

Completion = Tuple{Int, Int} # category indices and rule indices

mutable struct Grammar{C, D, F, S, T}
    categories       :: Vector{C}
    start_categories :: Vector{C}
    depcomp          :: F
    depcomps         :: Vector{D}
    catidx2dcompidx  :: Vector{Int}
    binary_rules     :: Vector{Function}
    terminal_rules   :: Vector{Function}
    all_rules        :: Vector{Function}
    binary_dict      :: Dict{Tuple{Int, Int}, Vector{Completion}}
    terminal_dict    :: Dict{T, Vector{Completion}}
    scores           :: S
end

function Grammar(
        categories       :: Vector,
        start_categories :: Vector,
        binary_rules_    :: Vector,
        terminal_rules_  :: Vector,
        depcomp,
        scores_          :: NamedTuple = (count = count_score,)
    )

    terminal_rules = convert(Vector{Function}, terminal_rules_)
    binary_rules   = convert(Vector{Function}, binary_rules_)

    depcomps  = unique(map(depcomp, categories))
    all_rules = vcat(binary_rules, terminal_rules)

    function find_elem_in(x, xs)
        i = findfirst(y->x==y, xs)
        @assert i > 0 "$x not in $xs"
        i
    end

    catidx   = @closure c  -> find_elem_in(c,  categories)
    dcompidx = @closure dc -> find_elem_in(dc, depcomps)
    ruleidx  = @closure r  -> find_elem_in(r,  all_rules)

    catidx2dcompidx = map(dcompidx ∘ depcomp, categories)

    binary_rhss = collect(
        (map(catidx, r(c)), (catidx(c), ruleidx(r)))
        for r in binary_rules for c in categories if isapplicable(r, c)
    )

    terminal_rhss = collect(
        (r(c), (catidx(c), ruleidx(r)))
        for r in terminal_rules for c in categories if isapplicable(r, c)
    )

    function multidict_from_list(list::AbstractVector{Tuple{K, V}}) where {K, V}
        foldr(list, init = Dict{K, Vector{V}}()) do (k, v), d
            haskey(d, k) ? push!(d[k], v) : d[k] = [v]
            d
        end
    end

    binary_dict   = multidict_from_list(binary_rhss)
    terminal_dict = multidict_from_list(terminal_rhss)

    one_dcomp_per_category = unique(depcomp, categories)
    score_matrix(s) = [s(c, r) for c in one_dcomp_per_category, r in all_rules]
    score_matrices  = map(score_matrix, scores_)

    scores = map(score_matrices) do M
        @closure (catidx, ruleidx) -> M[catidx2dcompidx[catidx], ruleidx]
    end

    Grammar(
        categories,
        start_categories,
        depcomp,
        depcomps,
        catidx2dcompidx,
        binary_rules,
        terminal_rules,
        all_rules,
        binary_dict,
        terminal_dict,
        scores
    )
end

score(g::Grammar, catidx, ruleidx) = map(eval_at(catidx, ruleidx), g.scores)
mergeable(g::Grammar, rhs)         = haskey(g.binary_dict, rhs)
merge(g::Grammar, rhs)             = g.binary_dict[rhs]

function categorize(g::Grammar, terminal)
    if haskey(g.terminal_dict, terminal)
        g.terminal_dict[terminal]
    else
        Vector{Completion}()
    end
end

##################
### The Parser ###
##################

"""
    parse(grammar, terminals)

parse the sequence of terminals according to the grammar, return a dictionary
that maps head categories to their semiring values

The metod first creates the diagonal of the chart as a dictionary,
then updates this dictionary dynammicaly using a `LazyDict`,
applying dynamic programming under the hood automatically.
"""
function parse(g::Grammar, terminals)
    initial_chart = Dict(
        (i, i) => Dict(
            lhs => score(g, lhs, r)
            for (lhs, r) in categorize(g, t)
        )
        for (i, t) in enumerate(terminals)
    )

    chart = LazyDict(initial_chart) do i, j
        cell = valtype(initial_chart)()
        for k in i : j-1
            for (rhs1, s1) in chart[i, k]
                for (rhs2, s2) in chart[k+1, j]
                    if mergeable(g, (rhs1, rhs2))
                        for (lhs, r) in merge(g, (rhs1, rhs2))
                            if haskey(cell, lhs)
                                cell[lhs] += score(g, lhs, r) * s1 * s2
                            else
                                cell[lhs] =  score(g, lhs, r) * s1 * s2
                            end
                        end
                    end
                end
            end
        end
        cell
    end

    heads = chart[1, length(terminals)]
    Dict(
        startcat => haskey(heads, catidx) ? heads[catidx] : zero(valtype(heads))
        for (catidx, startcat) in enumerate(g.start_categories)
    )
end
