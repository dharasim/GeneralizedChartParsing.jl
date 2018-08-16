using Main.GeneralizedChartParsing
using Test

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
@test length(tree_structs(g, s.enum_forest)) == s.count == Main.GeneralizedChartParsing.number_binary_trees(10)

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

p = parse(g, fill("a", 3))["S"]
@test map(string, tree_structs(g, p.forest)) ==
    ["[S[A[a]][A[A[a]][A[a]]]]", "[S[A[A[a]][A[a]]][A[a]]]"]

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
