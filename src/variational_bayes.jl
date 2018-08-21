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
        grammar::Grammar, terminals; n_samples=length(terminals)^2)

    m, n = length(grammar.depcomps), length(grammar.all_rules)

    p = try
        parse(grammar, terminals)
    catch e
        println(e)
        println("not parsable: ", terminals)
        return fill(0.0, m, n), LogProb(1)
    end

    likelihood           = sum(values(p)).prob
    parse_forest         = sum(values(p)).forest
    random_tree_iterator = rand(parse_forest)
    n_rules              = length(collect(random_tree_iterator))

    trees = fill((0,0), n_rules, n_samples)
    for j in 1:n_samples
        trees[:, j] = collect(random_tree_iterator)
    end

    counts = foldr(vec(trees), init = fill(0, m, n)) do (c, r), M
        M[g.catidx2dcompidx[c], r] += 1
        M
    end

    counts / n_samples, likelihood
end

"""
    variational_probs(pseudocounts::Union{AbstractVector, AbstractMatrix})

Calculate the updated variational probabilities according to the `pseudocounts`.

Rowwise calculation if pseudocounts is a matric
"""
function variational_probs(pseudocounts::AbstractVector)
    s = sum(rule_counts)
    normalize(map(a ->exp(digamma(a)), rule_counts), 1)
end

function variational_probs(rule_counts::AbstractMatrix)
    mapslices(variational_probs, rule_counts, dims=2)
end

"""
    train_grammar(grammar, dataset)

Train `grammar` iterating through the `dataset` once.

The result is an updated grammar and the likelihood of the dataset according to
the initial grammar.

# Assumptions
- the number of used rules is constant for each parse tree
- `grammar` has a score named `:prior` that returns real numbers
- `grammar` has a score named `:prob` to calculate the likelihood
- `grammar` has a score named `:forest` from which trees can be sampled as
  iterable objects of tuples of category indices and rule indices
"""
function train_grammar(g::Grammar, dataset)
    # add counts, multiply likelihoods
    reduce((c1, p1), (c2, p2)) = (c1 + c2, p1 * p2)

    rule_counts, likelihood = @distributed (reduce) for terminals in dataset
        estimate_rule_counts(g, terminals)
    end

    pseudocounts = rule_counts + tabulate_score(g, :prior)

    vprobs = variational_probs(pseudocounts)
    probs  = mapslices(row -> normalize(row, 1), pseudocounts, dims=2)

    vprob_score = @closure (catidx, ruleidx) ->
        LogProb(vprobs[g.catidx2dcompidx[catidx], ruleidx])

    prob_score  = @closure (catidx, ruleidx) ->
        LogProb(probs[g.catidx2dcompidx[catidx], ruleidx])

    g = set_scores(g, (prior = g.scores.prior, vprob = vprob_score, prob = prob_score))
    g = add_forest_score(g, :vprob)
    g, likelihood
end
