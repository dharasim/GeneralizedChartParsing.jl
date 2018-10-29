#######################
### Named Functions ###
#######################

struct NamedFunction{F} <: Function
    name :: String
    f    :: F
end

show(io::IO, nf::NamedFunction) = print(io, nf.name)
(nf::NamedFunction)(x)  = (nf.f)(x)

################
### Grammars ###
################

struct Completion{S}
    cid   :: Int
    rid   :: Int
    score :: S
end

struct Grammar{C,R,S}
    categories         :: Vector{C}
    rules              :: Vector{R}
    unary_completions  :: Vector{Vector{Completion{S}}}
    binary_completions :: Matrix{Vector{Completion{S}}}
end

function Grammar(categories, rules, score)
    S = typeof(score(first(categories), first(rules)))
    n = length(categories)
    unary_completions  = [Vector{Completion{S}}() for c in categories]
    binary_completions = [Vector{Completion{S}}() for c1 in categories, c2 in categories]

    "category ID"
    cid(category) = findfirst(isequal(category), categories) :: Int

    for (c, category) in enumerate(categories)
        for (r, rule) in enumerate(rules)
            rhs = rule(category)
            if rhs === nothing
                continue
            elseif rhs isa Tuple
                @assert length(rhs) == 2
                push!(
                    binary_completions[cid(rhs[1]), cid(rhs[2])],
                    Completion(c, r, score(category, rule)))
            else
                push!(
                    unary_completions[cid(rhs)],
                    Completion(c, r, score(category, rule)))
            end
        end
    end
    Grammar(collect(categories), collect(rules), unary_completions, binary_completions)
end

scoretype(grammar::Grammar{C,R,S}) where {C,R,S} = S
cid(grammar::Grammar, category) = findfirst(isequal(category), grammar.categories)
completions(grammar::Grammar, c::Int) = grammar.unary_completions[c]
completions(grammar::Grammar, c1::Int, c2::Int) = grammar.binary_completions[c1, c2]

function Base.parse(grammar::Grammar, terminals)
    insert!(cell, c, s) = if haskey(cell, c)
        cell[c] += s
    else
        cell[c]  = s
    end

    initial_chart = Dict(
        (i, i) => Dict(cid(grammar, t) => one(scoretype(grammar)))
        for (i, t) in enumerate(terminals)
    )

    for i in eachindex(terminals)
        cell = initial_chart[i, i]
        for c in keys(cell)
            for comp in completions(grammar, c)
                insert!(cell, comp.cid, comp.score)
            end
        end
    end

    chart = LazyDict(initial_chart) do i, j
        cell = valtype(initial_chart)()
        for k in i : j-1
            for (c1, s1) in chart[i, k]
                for (c2, s2) in chart[k+1, j]
                    for bcomp in completions(grammar, c1, c2)
                        c = bcomp.cid
                        s = bcomp.score * s1 * s2
                        insert!(cell, c, s)
                        for ucomp in completions(grammar, c)
                            insert!(cell, ucomp.cid, ucomp.score * s)
                        end
                    end
                end
            end
        end
        cell
    end
end

count_score(category, rule) = rule(category) === nothing ? 0 : 1
double(x)        = (x, x)
countup(x)       = x <= 9 ? (x, x+1) : nothing
g = Grammar(1:10, [double, countup], count_score)

f = (category, rule, s) -> "$category $rule $s"

transform_score(f, g)

function sample_prob_score(category, rules)
    function (rule, pseudocount)
