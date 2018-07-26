module Trees

import Base: parent, show, start, next, done, eltype, length, map
export Tree, data, children, isleaf, isroot, parent, add_child!, tree, leaf_data
export @tree_str

mutable struct Tree{T}
    data    :: T
    parent  :: Nullable{Tree{T}}
    children:: Vector{Tree{T}}
end

Tree{T}(data, parent::Tree{T}) = Tree(data, Nullable(parent), Vector{Tree{T}}())
Tree(data, T=typeof(data)) = Tree(data, Nullable{Tree{T}}(), Vector{Tree{T}}())

data(tree::Tree) = tree.data
children(tree::Tree) = tree.children
isleaf(tree::Tree) = isempty(tree.children)
isroot(tree::Tree) = isnull(tree.parent)
parent(tree::Tree) = get(tree.parent)

function show(io::IO, tree::Tree)
    print(io, '[', tree.data)
    for child in tree.children
      print(io, child)
    end
    print(io, ']')
end

add_child!{T}(tree::Tree{T}, data::T) = push!(tree.children, Tree(data, tree))
function add_child!{T}(tree::Tree{T}, child::Tree{T})
    child.parent = Nullable(tree)
    push!(tree.children, child)
end

function tree(str::AbstractString)
    node = Tree("")
    str  = replace(str, " ", "")[2:end-1]

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

start(tree::Tree) = [tree]
next{T}(::Tree{T}, list::Vector{Tree{T}}) =
    list[1], prepend!(list[2:end], children(list[1]))
done{T}(::Tree{T}, list::Vector{Tree{T}}) = isempty(list)
eltype{T}(::Type{Tree{T}}) = Tree{T}
length(tree::Tree) = isleaf(tree) ? 1 : 1 + sum(length(c) for c in children(tree))

leaf_data(tree::Tree) = [node.data for node in tree if isleaf(node)]

function Base.map(f, t::Tree; uniform_type=true)
    n = uniform_type ? Tree(f(t.data)) : Tree(f(t.data), Any)
    for c in t.children
        add_child!(n, map(f, c, uniform_type=uniform_type))
    end
    n
end

end # module

# tests
# using Trees
# t = Tree("head")
# add_child!(t, "left")
# add_child!(t, "right")
# string(t) == "[head[left][right]]"
# string(tree"[C[G][C]]") == "[C[G][C]]"
# data.(collect(tree"[C[G][C]]")) == ["C", "G", "C"]
# leaf_data(tree"[C[G][C]]") == ["G", "C"]
# string(map(str->str*"m", tree"[C[G][C]]")) == "[Cm[Gm][Cm]]"
