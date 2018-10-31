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

cid(grammar::Grammar, category) =
    findfirst(isequal(category), [grammar.categories; grammar.terminals]) :: Int

completions(grammar::Grammar, c::Int) = grammar.unary_completions[c]
completions(grammar::Grammar, c1::Int, c2::Int) = grammar.binary_completions[c1, c2]
scores(grammar::Grammar) = grammar.scores
score(grammar::Grammar, c::Int, r::Int) = scores(grammar)[c, r]

function parse(grammar::Grammar{S}, sequence) where S
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
