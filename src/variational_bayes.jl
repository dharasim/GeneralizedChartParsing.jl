"""
    estimate_rule_counts(grammar, terminals; n_samples = length(terminals)^2)

Parse `terminals` using `grammar` and calculate a Monte Carlo estimate of
of the expected rule usage in the parse tree for all dependent category
components and rules, return a tuple of the expected counts as a matrix and
the likelihood as a `LogProb`

# Assumptions
- the number of used rules is constant for each parse tree
- `grammar` has a score named `:prob` to calculate the likelihood
- `grammar` has a score named `:forest` from which trees can be sampled as
  iterable objects of tuples of category indices and rule indices
"""
function estimate_rule_counts(
        grammar :: Grammar, terminals; n_samples = length(terminals)^2
    )

    m, n   = length(grammar.categories), length(grammar.rules)
    counts = fill(0, m, n)

    score = try
        chart = parse(grammar, terminals)
        chart[1,length(terminals)][1]
    catch e
        println(e)
        println("not parsable: ", terminals)
        return fill(0.0, m, n), LogProb(1)
    end

    likelihood = score.prob

    for i in 1:n_samples
        tree = rand(score.forest)
        for (c, r) in tree
            counts[c, r] += 1
        end
    end

    counts / n_samples, likelihood
end

"""
    variational_probs(pseudocounts::Union{AbstractVector, AbstractMatrix})

Calculate the updated variational probabilities according to the `pseudocounts`.

Rowwise calculation if pseudocounts is a matric
"""
function variational_probs(pseudocounts::AbstractVector)
    s = sum(pseudocounts)
    normalize(map(a ->exp(digamma(a)), pseudocounts), 1)
end

function variational_probs(pseudocounts::AbstractMatrix)
    mapslices(variational_probs, pseudocounts, dims=2)
end

"""
    train_grammar(grammar, dataset)

Train `grammar` iterating through the `dataset` once.

The result is an updated grammar and the likelihood of the dataset according to
the initial grammar.

# Assumptions
- the number of used rules is constant for each parse tree
- `grammar` has a score named `:priorcount` that returns real numbers
- `grammar` has a score named `:prob` to calculate the likelihood
- `grammar` has a score named `:forest` from which trees can be sampled as
  iterable objects of tuples of category indices and rule indices
"""
function train_grammar(grammar::Grammar, dataset)
    # add counts, multiply likelihoods
    reduce((c1, p1), (c2, p2)) = (c1 + c2, p1 * p2)

    # calculate rule count estimates in parallel
    rule_counts, likelihood = @distributed reduce for terminals in dataset
        estimate_rule_counts(grammar, terminals)
    end

    # classify rule counts
    cls = unique(grammar.classes)
    cs  = eachindex(grammar.categories)
    rs  = eachindex(grammar.rules)
    classified_rule_counts = zeros(Int, length(cls), length(rs))
    for c in cs
        for r in rs
            classified_rule_counts[grammar.classes[c], r] += rule_counts[c, r]
        end
    end

    shared_rule_counts = tabulate(cs, rs) do c, r
        classified_rule_counts[grammar.classes[c], r]
    end

    priorcounts  = map(s->s.priorcount, grammar.scores)
    pseudocounts = shared_rule_counts + priorcounts

    probs  = map(LogProb, mapslices(row -> normalize(row, 1), pseudocounts, dims=2))
    vprobs = map(LogProb, variational_probs(pseudocounts))

    scores = zip( (priorcount=priorcounts, prob=probs, forest=forest_scores(vprobs)) )

    trained_grammar = @set grammar.scores = scores

    trained_grammar, likelihood
end
