function make_rules_from_one_line(rule_string::AbstractString)
    lhs, rhs = map(strip, split(rule_string, "-->"))
    rhss = map(rhs->map(strip, rhs), map(split, map(strip, split(rhs, "|"))))

    rules = map(rhs->make_cfg_rule(lhs, rhs...), rhss)

    (binary_rules   = [r.rule for r in rules if !r.isterminal],
     terminal_rules = [r.rule for r in rules if  r.isterminal])
end

function make_rules(rule_string::AbstractString)
    rules = map(make_rules_from_one_line, split(strip(rule_string), "\n"))
    (binary_rules   = vcat(map(rs -> rs.binary_rules,   rules)...),
     terminal_rules = vcat(map(rs -> rs.terminal_rules, rules)...))
end

function make_cfg_rule(lhs, rhs1, rhs2)
    r = NamedFunction(
        "$lhs --> $rhs1 $rhs2",
        @closure x -> x == lhs ? (rhs1, rhs2) : nothing)

    (rule = r, isterminal = false)
end

function make_cfg_rule(lhs, terminal)
    r = NamedFunction(
        "$lhs --> $terminal",
        @closure x -> x == lhs ? terminal : nothing)
    (rule = r, isterminal = true)
end

function grammar_from_string(str::AbstractString)
    categories = map(s -> string(strip(split(s, "-->")[1])), split(strip(str), "\n"))
    start_categories = [categories[1]]

    binary_rules, terminal_rules = make_rules(str)

    Grammar(categories, start_categories, binary_rules, terminal_rules, identity)
end
