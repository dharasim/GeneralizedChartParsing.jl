module Trees

import Base: parent, show, iterate, eltype, length, IteratorSize, map

export Tree, value, isleaf, isroot, parent, add_child!, tree, leaf_values, children
export @tree_str

mutable struct Tree{T}
    value    :: T
    parent  :: Union{Tree{T}, Nothing}
    children:: Vector{Tree{T}}
end

Tree(value, parent::Tree{T}) where T = Tree(value, parent, Tree{T}[])
Tree(value, T = typeof(value)) = Tree(value, nothing, Tree{T}[])

value(tree::Tree)    = tree.value
children(tree::Tree) = tree.children
isleaf(tree::Tree)   = isempty(tree.children)
isroot(tree::Tree)   = tree.parent === nothing
parent(tree::Tree)   = tree.parent

function show(io::IO, tree::Tree)
    print(io, '[', value(tree))
    for child in children(tree)
      print(io, child)
    end
    print(io, ']')
end

function add_child!(tree::Tree{T}, value::T) where T
    push!(children(tree), Tree(value, tree))
end

function add_child!(tree::Tree{T}, child::Tree{T}) where T
    child.parent = tree
    push!(children(tree), child)
end

function tree(str::AbstractString)
    node = Tree("")
    str  = replace(str, " " => "")[2:end-1]

    for c in str
        if c == '['
            add_child!(node, "")
            node = children(node)[end]
        elseif c == ']'
            node = parent(node)
        else
            node.value *= c
        end
    end
    node
end

macro tree_str(str) tree(str) end

eltype(::Type{Tree{T}}) where T = Tree{T}
IteratorSize(::Type{Tree{T}}) where T = Base.SizeUnknown()

function iterate(tree::Tree, state = [tree])
    if isempty(state)
        nothing
    else
        state[1], prepend!(state[2:end], children(state[1]))
    end
end

leaf_values(tree::Tree) = [value(node) for node in tree if isleaf(node)]

function map(f, t::Tree; uniform_type=true)
    n = uniform_type ? Tree(f(t.value)) : Tree(f(t.value), Any)
    for c in children(t)
        add_child!(n, map(f, c, uniform_type=uniform_type))
    end
    n
end

end # module

# tests
# using Main.Trees
# t = Tree("head")
# add_child!(t, "left")
# add_child!(t, "right")
# string(t) == "[head[left][right]]"
# string(tree"[C[G][C]]") == "[C[G][C]]"
# value.(collect(tree"[C[G][C]]")) == ["C", "G", "C"]
# leaf_values(tree"[C[G][C]]") == ["G", "C"]
# string(map(str->str*"m", tree"[C[G][C]]")) == "[Cm[Gm][Cm]]"
