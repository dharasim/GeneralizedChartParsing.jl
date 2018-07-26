"lift a binary operation to an operation on tuples of length n"
macro lift_to_tuples(op, n)

    "comma spaced string of the input list elements)"
    css(strs) = string(strs[1], [", " * strs[i] for i in 2:length(strs)]...)

    typevars1  = css(["T1$i" for i in 1:n])
    typevars2  = css(["T2$i" for i in 1:n])
    args      = "t1::Tuple{$typevars1}, t2::Tuple{$typevars2}"
    new_tuple = css(
        [string("$op(", css(["t$i[$j]" for i in 1:2]), ")") for j in 1:n]
    )

    code_str = """
        function $op($args) where {$typevars1, $typevars2}
            $new_tuple
        end
    """

    parse(code_str)
end

import Base: +, *

for op in (+, *), n in 1:4
    eval( :( @lift_to_tuples($op, $n) ) )
end

# tests
# +((1,2,3), (1,1,1))
# *((1,2,3.0,4.0), (2,2,2.0,2))
