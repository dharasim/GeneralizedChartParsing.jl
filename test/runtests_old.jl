using GeneralizedChartParsing
using Test

using LinearAlgebra: norm
using Random:        randperm

###################
### Utils Tests ###
###################

@test GeneralizedChartParsing.findallin([1,3,5], [4,3,2,5,1]) == [5,2,4]

############################
### Named Function Tests ###
############################

f = NamedFunction("goo", x -> x + 2)
@test string(f) == "goo"
@test f(3) == 5

##############################
### Construct Test Grammar ###
##############################

categories       = collect(1:10)
start_categories = [1]

double(x)        = (x, x)
countup(x)       = x <= 9 ? (x, x+1) : nothing
terminate(x)     = string(x)

binary_rules     = [double, countup]
terminal_rules   = [terminate]
depcomp          = identity

#############
### Tests ###
#############

g = Grammar(categories, start_categories, binary_rules, terminal_rules, depcomp)
g = add_random_prob_score(g, :count)

terminals = split("1 2 3 4 1 2")
@test s = parse(g, terminals)[1].count == 2

g = add_score(g, :forest, forest_score(g.scores.prob))
s = parse(g, terminals)[1]
t = s.forest |> rand |> collect |> t -> tree_struct(g, t)
ts = ["[1[1[1[1]][2[2[2]][3[3[3]][4[4]]]]][1[1[1]][2[2]]]]",
      "[1[1[1[1[1]][2[2[2]][3[3[3]][4[4]]]]][1[1]]][2[2]]]"]
@test string(t) in ts

g = Grammar(categories, start_categories, binary_rules, terminal_rules, depcomp)
g = add_score(g, :enum_forest, enum_forest_score)
s = parse(g, terminals)[1]
@test string.(tree_structs(g, s.enum_forest)) == ts

s = parse(g, fill("1", 10))[1]
@test length(tree_structs(g, s.enum_forest)) == s.count == GeneralizedChartParsing.number_binary_trees(10)

g = Grammar(categories, start_categories, binary_rules, terminal_rules, depcomp)
g = add_random_prob_score(g, :count)
g = add_expected_count_score(g, :prob)

ec = parse(g, terminals)[1].exp_counts

@test expected_counts_dataframe(g, ec)[:rule] |> unique ==
    [double, countup, terminate]

#################################
### Grammar From String Tests ###
#################################

str = """
    S --> A A | a
    A --> A A | a
    """

g = grammar_from_string(str)
g = add_score(g, :forest, enum_forest_score)
g = add_random_prob_score(g, :count)
g = add_expected_count_score(g, :prob)

rule_strings = ["S --> A A", "S --> a", "A --> A A", "A --> a"]

@test Set(map(string, g.all_rules)) == Set(rule_strings)

p = parse(g, fill("a", 3))["S"]
@test map(string, tree_structs(g, p.forest)) ==
    ["[S[A[a]][A[A[a]][A[a]]]]", "[S[A[A[a]][A[a]]][A[a]]]"]

@test expected_counts_dataframe(g, p.exp_counts)[:rule] .|> string |> Set ==
    Set(["S --> A A", "A --> A A", "A --> a"])

str = """
    S --> A N
    N --> S B | b
    A --> a
    B --> b
    """

g = grammar_from_string(str)
g = add_score(g, :forest, enum_forest_score)

p = parse(g, split("a a b b"))["S"]

@test map(string, tree_structs(g, p.forest)) ==
    ["[S[A[a]][N[S[A[a]][N[b]]][B[b]]]]"]

###############################
### Variational Bayes Tests ###
###############################

g = grammar_from_string("S --> S S | a | b")
g = set_scores(g, (prior = g.scores.count,))
g = add_random_prob_score(g, :prior)
g = add_forest_score(g, :prob)

dataset = let terminals = [fill("a", 3); fill("b", 7)]
    [terminals[randperm(10)] for i in 1:100]
end

loglikelihoods = Float64[]

for i in 1:5
    global g, p = train_grammar(g, dataset)
    push!(loglikelihoods, log(p))
end

true_parameters =
    [9 / 19, 3 / 19, 7 / 19]
inferred_parameters =
    map(float, vec(GeneralizedChartParsing.tabulate_score(g, :prob)))

@test norm(true_parameters - inferred_parameters) < 0.01
