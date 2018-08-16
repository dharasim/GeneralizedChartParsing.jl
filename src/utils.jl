"""
    mapvalues(f, dict)

map a function to the values of a dictionary
"""
function mapvalues(f, d::Dict)
    Dict(k => f(v) for (k,v) in d)
end


"""
    mapvalues(f, dict)

map a function to the values of a dictionary in place
"""
function mapvalues!(f, d::Dict)
    for k in keys(d)
        d[k] = f(d[k])
    end
    d
end

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
    x = rand() * sum(weights)
    cum_weights = zero(eltype(weights))
    for (t, w) in zip(tokens, weights)
        cum_weights += w
        if cum_weights >= x
            return t
        end
    end
    error("categorical sample failed with length(tokens)=$(length(tokens)), weights=$weights")
end

categorical_sample(d::Dict) = categorical_sample(keys(d), values(d))
categorical_sample(v::Vector) = categorical_sample(1:length(v), v)

###################
### Categorical ###
###################

mutable struct Categorical{T}
	probs :: Dict{T, LogProb}

	function Categorical(params::Dict{T, _}) where {T, _}
		s = sum(values(params))
		new{T}(mapvalues(v->LogProb(v/s), params))
	end
end

sample(c::Categorical) = categorical_sample(probs)
logscore(c::Categorical, x) = c.probs[x]

#############################>
### Dirichlet Categorical ###
#############################

mutable struct DirCat{T, C}
    counts :: Dict{T, C}
end

DirCat(support, priors) = DirCat(Dict(x => p for (x,p) in zip(support, priors)))
support(dc::DirCat) = keys(dc.counts)

function sample(dc::DirCat)
    weights = [gammarand(c, 1) for c in values(dc.counts)]
    categorical_sample(keys(dc.counts), weights)
end

function logscore(dc::DirCat, obs)
    LogProb(lbeta(sum(values(dc.counts)), 1) - lbeta(dc.counts[obs], 1))
end

function add_obs!(dc::DirCat, obs)
    dc.counts[obs] += 1
end

function rm_obs!(dc::DirCat, obs)
    dc.counts[obs] -= 1
end
