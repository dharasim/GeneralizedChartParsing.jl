######################
### NamedFunctions ###
######################

struct NamedFunction{F} <: Function
    name :: String
    f    :: F
end

show(io::IO, nf::NamedFunction) = print(io, nf.name)
(nf::NamedFunction)(args...)  = (nf.f)(args...)

##########################
### Tabulate Functions ###
##########################

tabulate(f, xs)         = [f(x) for x in xs]
tabulate(f, xs, ys)     = [f(x, y) for x in xs, y in ys]
tabulate(f, xs, ys, zs) = [f(x, y, z) for x in xs, y in ys, z in zs]

##################################
### Equivalence Classification ###
##################################

function equivalence_classes(f, n::Int)
    classes   = zeros(Int, n)
    n_classes = 0 # number of distinct classes

    for i in 1:n
        classified = false
        for j in 1:i-1
            if f(i) == f(j)
                classes[i] = classes[j]
                classified = true
                break
            end
        end
        if !classified
            n_classes += 1
            classes[i] = n_classes
        end
    end

    classes
end

#######################
### Other Utilities ###
#######################

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
