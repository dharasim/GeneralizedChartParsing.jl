module ScoredForests

import Base: one, isone, zero, iszero, +, *, ==, rand, show

import TikzQTrees
using  TikzQTrees: TikzQTree, SimpleTree, children

export forest_scores, tree_struct, greedy_best_tree

@enum ForestTag PRIMITIVE SUM PRODUCT ZERO ONE

struct ScoredForest{V,S} <: Number
    tag   :: ForestTag
    value :: Union{V, Nothing}
    left  :: Union{ScoredForest{V,S}, Nothing}
    right :: Union{ScoredForest{V,S}, Nothing}
    score :: S
end

PrimitiveForest(v, s)         = ScoredForest(PRIMITIVE,v,nothing,nothing,s)
SumForest(left, right, s)     = ScoredForest(SUM,nothing,left,right,s)
ProductForest(left, right, s) = ScoredForest(PRODUCT,nothing,left,right,s)
ZeroForest(V, S) = ScoredForest{V,S}(ZERO,nothing,nothing,nothing,zero(S))
OneForest(V, S)  = ScoredForest{V,S}(ONE,nothing,nothing,nothing,one(S))

isprim(f::ScoredForest) = tag(f) === PRIMITIVE
issum(f::ScoredForest)  = tag(f) === SUM
isprod(f::ScoredForest) = tag(f) === PRODUCT
iszero(f::ScoredForest) = tag(f) === ZERO
isone(f::ScoredForest)  = tag(f) === ONE

tag(f::ScoredForest) = f.tag
score(f::ScoredForest) = f.score

value(f::ScoredForest{V,S}) where {V,S} = f.value :: V
left(f::ScoredForest{V,S})  where {V,S} = f.left  :: ScoredForest{V,S}
right(f::ScoredForest{V,S}) where {V,S} = f.right :: ScoredForest{V,S}

zero(::Type{ScoredForest{V,S}}) where {V,S} = ZeroForest(V, S)
one(::Type{ScoredForest{V,S}})  where {V,S} = OneForest(V, S)

function show(io::IO, f::ScoredForest{V,S}) where {V,S}
    iszero(f) && return print(io, "ZeroForest")
    isone(f)  && return print(io, "OneForest")
    isprim(f) && return print(io, "PrimitiveForest($(value(f)), $(score(f)))")
    issum(f)  && return print(io, "SumForest(left, right, $(score(f)))")
    isprod(f) && return print(io, "ProductForest(left, right, $(score(f)))")
end

function +(f::ScoredForest, g::ScoredForest)
    iszero(f) && return g
    iszero(g) && return f
    SumForest(f, g, score(f) + score(g))
end

function *(f::ScoredForest, g::ScoredForest)
    iszero(f) && return f
    iszero(g) && return g
    isone(f)  && return g
    isone(g)  && return f
    ProductForest(f, g, score(f) * score(g))
end

function rand(f::ScoredForest{V,S}) where {V,S}
    function rand!(samples, f::ScoredForest{V,S}) where {V,S}
        if isprim(f)
            push!(samples, value(f))
        elseif issum(f)
            l, r = left(f), right(f)
            rand(S) * score(f) < score(l) ? rand!(samples, l) : rand!(samples, r)
        elseif isprod(f)
            rand!(samples, left(f))
            rand!(samples, right(f))
        end
        return nothing
    end

    samples = V[]
    rand!(samples, f)
    samples
end

function greedy_best_tree(f::ScoredForest{V,S}) where {V,S}
	function best!(selection, f::ScoredForest{V,S}) where {V,S}
		if isprim(f)
			push!(selection, value(f))
        elseif issum(f)
            l, r = left(f), right(f)
			score(l) > score(r) ? best!(selection, l) : best!(selection, r)
		elseif isprod(f)
			best!(selection, left(f))
			best!(selection, right(f))
		end
		return nothing
	end

	selection = V[]
	best!(selection, f)
	selection
end

function forest_scores(basescores::AbstractMatrix{S}) where S
	tabulate(f, xs, ys) = [f(x, y) for x in xs, y in ys]
	m, n = size(basescores)

	tabulate(1:m, 1:n) do c, r
		s = basescores[c, r]
		iszero(s) ? ZeroForest(Tuple{Int, Int}, S) : PrimitiveForest((c, r), s)
	end
end

function tree_struct(categories, terminals, rules, tree::Vector{Tuple{Int,Int}})
	treevalue = TikzQTrees.value
    function apply_rules!(nodes, rules)
        if isempty(rules)
            nothing
        else
            r, rs = rules[1], rules[2:end]
            n, ns = nodes[1], nodes[2:end]
			if treevalue(n) in terminals
				apply_rules!(ns, rules)
			else
				rhs = r(treevalue(n))
	            if rhs === nothing
					apply_rules!(ns, rules)
				elseif rhs isa Tuple
	                @assert length(rhs) == 2
	                for c in r(treevalue(n))
	                    push!(children(n), SimpleTree(c, Any))
	                end
	                apply_rules!([children(n); ns], rs)
				else
					push!(children(n), SimpleTree(rhs, Any))
					apply_rules!([children(n); ns], rs)
	            end
			end
        end
        nothing
    end

	category = categories[tree[1][1]]
	root     = SimpleTree(category, Any)

	ruleidxs = getindex.(tree, 2)
	rules    = rules[ruleidxs]

    apply_rules!([root], rules)
    root
end

end # module
