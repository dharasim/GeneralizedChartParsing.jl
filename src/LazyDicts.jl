module LazyDicts

import Base: show, getindex, setindex!, keys, values, keytype, valtype, map

export LazyDict

struct LazyDict{F, K, V, D <: AbstractDict{K, V}}
    f :: F
    d :: D
end

LazyDict(f, kv) = LazyDict(f, Dict(kv))
LazyDict(f, kv::Pair...) = LazyDict(f, Dict(kv...))
LazyDict(f, K::Type, V::Type) = LazyDict(f, Dict{K, V}())
LazyDict(f) = LazyDict(f, Any, Any)

function getindex(d::LazyDict, key)
    get!(()->(d.f)(key), d.d, key)
end

function getindex(d::LazyDict, key_first, key_rest...)
    get!(()->(d.f)(key_first, key_rest...), d.d, (key_first, key_rest...))
end

function setindex!(d::LazyDict, value, key...)
    setindex!(d.d, value, key...)
end

keytype(::Type{LazyDict{F, K, V, D}}) where {F, K, V, D} = K
valtype(::Type{LazyDict{F, K, V, D}}) where {F, K, V, D} = V

keys(d::LazyDict) = keys(d.d)
values(d::LazyDict) = values(d.d)

function map(g, d::LazyDict)
    LazyDict(g ∘ d.f, Dict(k => g(d[k]) for k in keys(d)))
end

(d::LazyDict)(k...) = d[k...]

end # module

#############
### tests ###
#############

# plus = LazyDict(+, Tuple{Int, Int}, Int)
#
# plus[1, 2] == 3
# plus(1, 2) == 3
#
# keytype(typeof(plus)) == Tuple{Int, Int}
# valtype(typeof(plus)) == Int
#
# fibs = LazyDict(0 => BigInt(0), 1 => BigInt(1)) do x
#     fibs[x-1] + fibs[x-2]
# end
#
# first_fibs = [0,1,1,2,3,5,8,13,21,34,55]
# map(fibs, 0:10) == first_fibs
#
#
# doubled_fibs = map(x->2x, fibs)
# doubled_fibs.(0:10) == 2 * first_fibs
#
# @time fibs(10000)
# @time fibs(10000)
