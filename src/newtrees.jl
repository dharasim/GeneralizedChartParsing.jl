module Trees

import Base: parent, show, iterate, eltype, length, IteratorSize, map
export Tree, data, children, isleaf, isroot, parent, add_child!, tree, leaf_data
export @tree_str

mutable struct Tree{T}
    data    :: T
    parent  :: Union{Tree{T}, Nothing}
    children:: Vector{Tree{T}}
end

Tree(data, parent::Tree{T}) where T = Tree(data, parent, Tree{T}[])
Tree(data, T = typeof(data)) = Tree(data, nothing, Tree{T}[])

data(tree::Tree)     = tree.data
children(tree::Tree) = tree.children
isleaf(tree::Tree)   = isempty(tree.children)
isroot(tree::Tree)   = tree.parent === nothing
parent(tree::Tree)   = tree.parent

function show(io::IO, tree::Tree)
    print(io, '[', data(tree))
    for child in children(tree)
      print(io, child)
    end
    print(io, ']')
end

function add_child!(tree::Tree{T}, data::T) where T
    push!(children(tree), Tree(data, tree))
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
            node.data *= c
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

leaf_data(tree::Tree) = [data(node) for node in tree if isleaf(node)]

function map(f, t::Tree; uniform_type=true)
    n = uniform_type ? Tree(f(t.data)) : Tree(f(t.data), Any)
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
# data.(collect(tree"[C[G][C]]")) == ["C", "G", "C"]
# leaf_data(tree"[C[G][C]]") == ["G", "C"]
# string(map(str->str*"m", tree"[C[G][C]]")) == "[Cm[Gm][Cm]]"
