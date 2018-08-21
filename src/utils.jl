eval_at(args...) = f -> f(args...)
findallin(xs, ys) = map(x -> findfirst(y -> x == y, ys), xs)

"""
    catalan_number(n)

calculate the n-th catalan number
"""
catalan_number(n) = div(binomial(2n, n), n+1)

"""
    number_binary_trees(n)

calculate the number of binary trees with n leafs
"""
number_binary_trees(n) = catalan_number(n - 1)

"""
    categorical_sample(tokens, weights)

sample one token according to the given weights
"""
function categorical_sample(tokens, weights)
    T = eltype(weights)
    x = convert(T, rand()) * sum(weights)
    cum_weights = zero(T)
    for (t, w) in zip(tokens, weights)
        cum_weights += w
        if cum_weights >= x
            return t
        end
    end
    error("categorical sample failed with length(tokens)=$(length(tokens)), weights=$weights")
end

categorical_sample(d::Dict)   = categorical_sample(keys(d), values(d))
categorical_sample(v::Vector) = categorical_sample(1:length(v), v)
