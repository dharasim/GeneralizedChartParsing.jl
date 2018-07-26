using GeneralizedChartParsing

r1 = Rule("double", [1]) do _
    (1, 1)
end
r2 = Rule("terminate", [1]) do _
    ('a',)
end

grammar = CNFGrammar([1], [r1], [r2], identity)
terminals = ['a', 'a', 'a', 'a']
parse(grammar, terminals)

countup = Rule("countup", collect(1:3)) do i
    (i, i + 1)
end

double = Rule("double", collect(1:4)) do i
    (i, i)
end

terminate = Rule("terminate", collect(1:4)) do i
    (string(i),)
end

grammar = CNFGrammar([1], [countup, double], [terminate], identity)
terminals = split("1 2 3 4 1 2")

dist = ParseTreeDistribution(grammar, terminals);

tree = rand(dist); println(tree); println(tree_struct(grammar, tree))
tree_struct(grammar, tree)

grammar.id_to_rule
