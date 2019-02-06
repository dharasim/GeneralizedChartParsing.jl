const Completion{S} = Tuple{Int, Int, S}

struct IntGrammar{S}
    unary_completions  :: Vector{Vector{Completion{S}}}
    binary_completions :: Matrix{Vector{Completion{S}}}
    classes            :: Vector{Int}
end

completions(g::IntGrammar, c::Int) = g.unary_completions[c]
completions(g::IntGrammar, c1::Int, c2::Int) = g.binary_completions[c1, c2]
scoretype(::Type{IntGrammar{S}}) where S = S
scoretype(g::G) where G = scoretype(G)

function parsechart(g::IntGrammar{S}, seq::Vector{Int}) where S
    add!(cell, c, s) = haskey(cell, c) ? (cell[c] += s) : (cell[c] = s)

    initial_chart = Dict(
        (i, i) => Dict(k => one(S))
        for (i, k) in enumerate(seq)
    )

    for (i, k) in enumerate(seq)
        for (c, r, s) in completions(g, k)
            add!(initial_chart[i, i], c, s)
        end
    end

    chart = LazyDict(initial_chart) do i, j
        cell = valtype(initial_chart)()
        for k in i : j-1
            for (c1, s1) in chart[i, k]
                for (c2, s2) in chart[k+1, j]
                    for (c, r, s) in completions(g, c1, c2)
                        s_ = s * s1 *s2
                        add!(cell, c, s_)

                        for (uc, ur, us) in completions(g, c) # unary completions
                            add!(cell, uc, us * s_)
                        end
                    end
                end
            end
        end
        cell
    end
end

function scores(g::IntGrammar)
    merge(+,
        Dict( (lhs, r) => s for comps in g.unary_completions for (lhs, r, s) in comps ),
        Dict( (lhs, r) => s for comps in g.binary_completions for (lhs, r, s) in comps )
    )
end

struct Grammar{S}
    nonterminals :: Vector
    terminals    :: Vector
    rules        :: Vector
    intgrammar   :: IntGrammar{S}
end

function Grammar(nonterminals, terminals, rules, score, classify=identity)
    disjunct(iter1, iter2) = length(union(iter1, iter2)) == length(iter1) + length(iter2)
    @assert disjunct(nonterminals, terminals)
    categories = [nonterminals; terminals]

    # category ID
    cid(category) = findfirst(isequal(category), categories) :: Int

    S = typeof(sum(score(nonterminals[1], r) for r in rules))

    unary_completions  = [Tuple{Int,Int,S}[] for c in categories]
    binary_completions = [Tuple{Int,Int,S}[] for c1 in categories, c2 in categories]

    for (c, lhs) in enumerate(nonterminals)
        for (r, rule) in enumerate(rules)
            rhs = rule(lhs)
            s   = score(lhs, rule)
            if rhs === nothing
                continue
            elseif rhs isa Tuple
                @assert length(rhs) == 2
                push!(binary_completions[cid(rhs[1]), cid(rhs[2])], (c, r, s))
            else
                push!(unary_completions[cid(rhs)], (c, r, s))
            end
        end
    end

    classes = equivalence_classes(length(nonterminals)) do i
        classify(nonterminals[i])
    end

    Grammar(
        collect(nonterminals), collect(terminals), collect(rules),
        IntGrammar(unary_completions, binary_completions, classes)
    )

    # scores = Dict( 
    #     (nt, r) => score(nonterminal, rule)
    #     for (nt, nonterminal) in enumerate(nonterminals) 
    #     for (r, rule) in enumerate(rules)
    #     if !iszero(score(nonterminal, rule))
    # )

    

    # let score    = @closure( (nt, r) -> haskey(scores, (nt, r)) ? scores[nt, r] : zero(valtype(scores)) ),
    #     classify = @closure( nt -> classes[nt] )

    #     Grammar(
    #         collect(nonterminals), collect(terminals), collect(rules),
    #         IntGrammar{typeof(score), typeof(classify), valtype(scores)}(
    #             unary_completions, binary_completions, score, classify
    #         )
    #     )
    # end
end

cid(g::Grammar, category) = findfirst(isequal(category), [g.nonterminals; g.terminals]) :: Int
parsechart(g::Grammar, seq) = parsechart(g.intgrammar, cid.(Ref(g), seq))
parse(g, seq) = parsechart(g, seq)[1, length(seq)][1]








# struct Grammar{S}
#     categories         :: Vector
#     terminals          :: Vector
#     rules              :: Vector
#     unary_completions  :: Vector{Vector{Tuple{Int,Int}}}
#     binary_completions :: Matrix{Vector{Tuple{Int,Int}}}
#     scores             :: Matrix{S}
#     classes            :: Vector{Int}
# end

# function Grammar(categories, terminals, rules, score, classify_nt=identity)
#     Grammar(categories, terminals, rules, tabulate(score, categories, rules), classify_nt)
# end

# function Grammar(categories, terminals, rules, scores::Matrix, classify_nt=identity)
#     cats_and_terms = [categories; terminals]

#     # category ID
#     cid(category) = findfirst(isequal(category), cats_and_terms) :: Int

#     unary_completions  = [Tuple{Int,Int}[] for c in cats_and_terms]
#     binary_completions = [Tuple{Int,Int}[] for c1 in cats_and_terms, c2 in cats_and_terms]

#     for (c, category) in enumerate(categories)
#         for (r, rule) in enumerate(rules)
#             rhs = rule(category)
#             if rhs === nothing
#                 continue
#             elseif rhs isa Tuple
#                 @assert length(rhs) == 2
#                 push!(binary_completions[cid(rhs[1]), cid(rhs[2])], (c, r))
#             else
#                 push!(unary_completions[cid(rhs)], (c, r))
#             end
#         end
#     end

#     classes = equivalence_classes(length(categories)) do i
#         classify_nt(categories[i])
#     end

#     Grammar(
#         collect(categories), collect(terminals), collect(rules),
#         unary_completions, binary_completions, scores, classes
#     )
# end

# cid(grammar::Grammar, category) =
#     findfirst(isequal(category), [grammar.categories; grammar.terminals]) :: Int

# completions(grammar::Grammar, c::Int) = grammar.unary_completions[c]
# completions(grammar::Grammar, c1::Int, c2::Int) = grammar.binary_completions[c1, c2]
# scores(grammar::Grammar) = grammar.scores
# score(grammar::Grammar, c::Int, r::Int) = scores(grammar)[c, r]

# function parse(grammar::Grammar{S}, sequence) where S
#     insert!(cell, c, s) = if haskey(cell, c)
#         cell[c] += s
#     else
#         cell[c]  = s
#     end

#     initial_chart = Dict(
#         (i, i) => Dict(cid(grammar, x) => one(S))
#         for (i, x) in enumerate(sequence)
#     )

#     for i in eachindex(sequence)
#         cell = initial_chart[i, i]
#         for k in keys(cell)
#             for (c, r) in completions(grammar, k)
#                 insert!(cell, c, score(grammar, c, r))
#             end
#         end
#     end

#     chart = LazyDict(initial_chart) do i, j
#         cell = valtype(initial_chart)()
#         for k in i : j-1
#             for (c1, s1) in chart[i, k]
#                 for (c2, s2) in chart[k+1, j]
#                     for (c, r) in completions(grammar, c1, c2)
#                         s = score(grammar, c, r) * s1 * s2
#                         insert!(cell, c, s)
#                         for (uc, ur) in completions(grammar, c)
#                             insert!(cell, uc, score(grammar, uc, ur) * s)
#                         end
#                     end
#                 end
#             end
#         end
#         cell
#     end
# end

# function scores(grammar::Grammar{S}, sequences; workers=workers()) where S
#     wpool = CachingPool(workers)
#     let g = grammar
#         pmap(wpool, sequences) do sequence
#             heads = parse(g, sequence)[1,length(sequence)]
#             get(heads, 1, zero(S))
#         end
#     end
# end   
