module DynamicDicts

    import Base: getindex, setindex!, keys, values, map

    export DynamicDict

    struct DynamicDict{D, F}
        f :: F
        d :: D
    end

    DynamicDict(f, K, V) = DynamicDict(f, Dict{K, V}())
    DynamicDict(f) = DynamicDict(f, Any, Any)

    @inline function getindex(d::DynamicDict, key)
        get!(()->(d.f)(key), d.d, key)
    end

    @inline function getindex(d::DynamicDict, key_first, key_rest...)
        # println(key_first, '\t', key_rest)
        get!(()->(d.f)(key_first, key_rest...), d.d, (key_first, key_rest...))
    end

    @inline function setindex!(d::DynamicDict, value, key...)
        setindex!(d.d, value, key...)
    end

    keys(d::DynamicDict) = keys(d.d)
    values(d::DynamicDict) = values(d.d)

    @inline function map(g, d::DynamicDict)
        DynamicDict(
            g ∘ d.f,
            Dict(k => g(d[k]) for k in keys(d))
        )
    end

    (d::DynamicDict)(k) = d[k]

end # module

#############
### tests ###
#############

# d = DynamicDict(+, Tuple{Int, Int}, Int)
# d[1,2] == 3
#
# fibs = DynamicDicts.DynamicDict(x -> fibs[x-1] + fibs[x-2], Int, BigInt)
# fibs[0], fibs[1] = 0, 1
#
# first_fibs = [0,1,1,2,3,5,8,13,21,34,55]
# [fibs[i] for i in 0:10] == first_fibs
#
# doubled_fibs = map(x->2x, fibs)
#
# [doubled_fibs[i] for i in 0:10] == 2 * first_fibs
#
# @time fibs(10000)
# [fibs(i) for i in 0:20]
# fibs(20000)
# @time fibs[10001]
#
# using Lazy
#
# fibs_lst = @lazy BigInt(0):BigInt(1):(fibs_lst + drop(1, fibs_lst));
# fibs_lst[1001]
#
# take(fibs_lst, 10)
#
# fibs = @lazy 0:1:(fibs + drop(1, fibs));
# take(20, fibs_lst)
#
# get(5, fibs_lst)
