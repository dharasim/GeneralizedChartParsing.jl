using GeneralizedChartParsing, Test
using Random, TikzQTrees, Setfield, LinearAlgebra

double(x) = (x, x)
countup(x) = x <= 9 ? (x, x+1) : nothing
terminate(x) = string(x)
g = Grammar(1:10, map(string, 1:10), [double, countup, terminate], count_score)

@test parse(g, split("1 1 2 3 2"))[1,5][1] == 5

h = @set g.scores = g.scores |> random_prob_scores |> forest_scores

@test tree_struct(
        h.categories, h.terminals, h.rules,
        rand(parse(h, split("1 2 3 1"))[1,4][1])
    ) |>
    TikzQTree |>
    string == "[.1 [.1 [.1 1 ] [.2 [.2 2 ] [.3 3 ] ] ] [.1 1 ] ] "

g = let r1(x)      = ("S", "S"),
        r2(x)      = "a",
        r3(x)      = "b",
        categories = ["S"],
        terminals  = ["a", "b"],
        rules      = [r1, r2, r3],
        counts     = tabulate(count_score, categories, rules),
        probs      = random_prob_scores(counts),
        scores     = zip( (priorcount=counts, prob=probs, forest=forest_scores(probs)) )
    Grammar(categories, terminals, rules, scores)
end


dataset = let terminals = [fill("a", 3); fill("b", 7)]
    [terminals[randperm(10)] for i in 1:100]
end

loglikelihoods = Float64[]

g, p = train_grammar(g, dataset)

for i in 1:5
    global g, p = train_grammar(g, dataset)
    push!(loglikelihoods, log(p))
end

true_parameters = [9 / 19, 3 / 19, 7 / 19]
inferred_parameters = vec(map(s->float(s.prob), g.scores))

@test norm(true_parameters - inferred_parameters) < 0.01
