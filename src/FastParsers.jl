module FastParsers

export
    Parser,
    # basic parsers
    Always, Never, Start, End, Index,
    # modifiers
    Caseless,
    # single characters
    CharNotIn, CharIn,
    # combiners
    Maybe, Or, Seq, Many,
    # recursion
    Recursive,
    # transformers
    Range, Capture, Named, Const, GetIndex, Tr, TrSource,
    # lookahead/lookbehind
    Not, NotAhead, Ahead, NotBehind, Behind

# parsing failure indicator
struct Fail end

# parsing source (constant)
struct Src{T}
    src :: T
    len :: Int
end

@inline src(s::Src) = s.src
@inline len(s::Src) = s.len
@inline idx0(s::Src) = firstindex(src(s))

# parsing index (variable)
struct Idx{T}
    idx :: Int
    max :: Int
    char :: T
end
Idx(s::Src, idx::Int, max::Int=idx) = Idx(idx, max, idx≤len(s) ? @inbounds(src(s)[idx]) : convert(eltype(src(s)), 0))

@inline idx(i::Idx) = i.idx
@inline maxidx(i::Idx) = i.max
# @inline revert(i::Idx, i′::Idx) = i # simpler version without tracking maxidx
@inline revert(i::Idx, i′::Idx) = Idx(i.idx, max(i.max, i′.max), i.char)

function srcidx(src, idx=firstindex(src), len=lastindex(src))
    s = Src(src, len)
    i = Idx(s, idx)
    s, i
end

@inline atstart(s::Src, i::Idx) = idx(i) == idx0(s)
@inline atend(s::Src, i::Idx) = idx(i) > len(s)
@inline curchar(s::Src, i::Idx) = i.char
@inline inc(s::Src, i::Idx) = Idx(s, @inbounds(nextind(src(s), idx(i))))

"""
    Parser{R}

Abstract type of all parsers, with result type `R`.

New parser types must overload `tryparse(f::Cb{R}, p::NewParser, s::Src, i::Idx) where R`,
returning `retval(f, x, s, i′)` on success or `reterr(f, i′)` on failure.
"""
abstract type Parser{R} end

parsetype(::Type{<:Parser{R}}) where {R} = R
parsetype(p::Parser) = parsetype(typeof(p))

Parser(p::Parser) = p
Parser(c::Union{AbstractChar,Number}) = CharIn(c)
Parser(cs::Union{AbstractString,AbstractVector}) = Seq([CharIn(c) for c in cs]...)

Base.show(io::IO, ::MIME"text/plain", p::Parser) = show_indented(io, p)
function show_indented(io::IO, _p::Parser{R}, n=0, idx=nothing) where {R}
    p, name = unnamed(_p)
    print_indent(io, n)
    if idx !== nothing
        print(io, "(", idx, ") ")
    end
    leaf = n > 0 && name !== nothing
    if name !== nothing
        printstyled(name, bold=true)
        leaf || print(io, ' ')
    end
    if !leaf
        print(io, description(p))
    end
    if R != Nothing
        printstyled(" ::$R", color=:cyan)
    end
    leaf && return
    cs = (p isa Recursive && n==0) ? (get(p),) : children(p)
    for (i,c) in enumerate(cs)
        println(io)
        show_indented(io, c, n+1, length(cs)>1 ? i : nothing)
    end
    show_suffix(io, p, n)
end
print_indent(io::IO, n) =
    for i in 1:n
        print(io, "| ")
    end
description(p::Parser) = typeof(p).name
children(p::Parser) =
    if hasfield(typeof(p), :parsers)
        p.parsers
    elseif hasfield(typeof(p), :parser)
        (p.parser,)
    else
        ()
    end
show_suffix(io::IO, p::Parser, n) = nothing

"""
    Caseless(p::Parser{R}) :: Parser{R}

Converts `p` to a case-insensitive parser.
"""
Caseless(p::Parser) = error("Caseless not implemented for $(typeof(p).name)")
Caseless(p) = Caseless(Parser(p))

### CALLING CONVENTIONS
#
# Each of the following blocks of code implements a calling convention for this package.
#
# It's up to experimentation to see which works best --- calling conventions which make our
# code more type-inferrable and do less allocation are better. Conventions 3 and 4 are both
# quite good.

# const Ret{T} = Tuple{Bool, T}
# @inline isok(v::Ret) = v[1]
# @inline val(v::Ret) = v[2]
# @inline ret(::Type{R}, v) where {R} = true, (v::R)
# @inline ret(::Type{R}) where {R} = false, anyval(R)

# const Ret{T} = Union{Fail, T}
# @inline isok(v::Fail) = false
# @inline isok(v::Ret) = true
# @inline val(v::Fail) = error()
# @inline val(v::Ret) = v
# @inline ret(::Type{R}, v) where {R} = v::R
# @inline ret(::Type{R}) where {R} = Fail()

struct Ret{T}
    val :: Union{T,Fail}
end
Base.show(io::IO, v::Ret) = isok(v) ? print(io, "Ret(", repr(val(v)), ")") : print(io, "Ret(<fail>)")
@inline isok(v::Ret) = !(v.val isa Fail)
@inline val(v::Ret) = v.val isa Fail ? error() : v.val
@inline ret(::Type{R}, v) where {R} = Ret{R}(v::R)
@inline ret(::Type{R}) where {R} = Ret{R}(Fail())

# struct Ret{T}
#     ok :: Bool
#     val :: T
#     Ret{T}() where {T} = new{T}(false)
#     Ret{T}(v::T) where {T} = new{T}(true, v)
# end
# Base.show(io::IO, v::Ret) = isok(v) ? print(io, "Ret(", repr(val(v)), ")") : print(io, "Ret(<fail>)")
# @inline isok(v::Ret) = v.ok
# @inline val(v::Ret) = v.val
# @inline ret(::Type{R}, v) where {R} = Ret{R}(v::R)
# @inline ret(::Type{R}) where {R} = Ret{R}()

"""
    abstract type Cb{R}

Callbacks returning a value of type `R`.

When a parser succeeds, it calls `retval(f::Cb{R}, x, s::Src, i::Idx)` where `x` is the
parsed result, `s` is the source and `i` is the index at which parsing succeeded. This
returns a `Tuple{Ret{R}, Idx}`, which is a new result (or failure) and a new index. Usually
the new index equals the old index, but e.g. in the `Seq` parser this is used recursively to
call extra parsers.

Ordinarily `retval` will simply return `ret(R, x′::R), i` where `x′` is some new value.

When a parser fails, it calls `reterr(f::Cb{R}, i::Idx)` which always returns `ret(R), i`.
"""
abstract type Cb{R} end
@inline reterr(::Cb{R}, i::Idx) where {R} = ret(R), i

struct NothingCb <: Cb{Nothing} end
@inline retval(::NothingCb, x, ::Src, i::Idx) = ret(Nothing, nothing), i

struct IdentityCb{R} <: Cb{R} end
@inline retval(::IdentityCb{R}, x, ::Src, i::Idx) where {R} = ret(R, x), i

struct TransformCb{R,F<:Cb{R},T} <: Cb{R}
    f :: F
    tr :: T
end
@inline retval(f::TransformCb, x, s::Src, i::Idx) = retval(f.f, f.tr(x), s, i)

struct TransformSourceCb{R,F<:Cb{R},T} <: Cb{R}
    f :: F
    tr :: T
end
@inline retval(f::TransformSourceCb, x, s::Src, i::Idx) = retval(f.f, f.tr(x, src(s)), s, i)

"""
    tryparse([f::Cb], p::Parser, s, [i]) :: Tuple{Ret{R}, Idx}

Parse the source string or vector `s` from index `i`, return the parsed value and the final index.

If a callback `f` is supplied, then on success it is applied to the result.
"""
tryparse(f::Cb, p::Parser, args...) = tryparse(f, p, srcidx(args...)...)
tryparse(f::Cb, p::Parser, ::Src, ::Idx) = error("tryparse not implemented for $(typeof(p).name)")

"""
    tryparse([f, R::Type], p::Parser, s, [i]) :: Tuple{Union{R,Fail}, Int}

Parse the source string or vector `s` from index `i`, return the parsed value and the final index.
On failure, return `Fail()` and the index of a syntax error.

The meaning of `f` and `R` are as for [`parse`](@ref).
"""
function tryparse(f, ::Type{R}, p::Parser, args...) where {R}
    cb = f===identity ? IdentityCb{R}() : TransformCb{R}(IdentityCb{R}(), f)
    v, i = tryparse(cb, p, args...)
    if isok(v)
        val(v)::R, idx(i)
    else
        Fail(), maxidx(i)
    end
end
tryparse(p::Parser{R}, args...) where {R} = tryparse(identity, R, p, args...)

"""
    parse([f, R::Type], p::Parser, s; [onfail]) :: R
    parse([f, R::Type], p::Parser, s, i; [greedy=false], [onfail]) :: Tuple{R, Int}

Parse the source string or vector `s` and return the parsed value.

On parsing failure, returns `onfail(s, i′)` where `i′` is the location of a failure.
By default, this throws an error.

If `i` is not given, the entire string is parsed. Otherwise, this behaviour is controlled by
`greedy` and the index at the end of the match is also returned.

If a callback function `f` is supplied, then on success it is applied to the result.
Currently, the return type of `f` must be specified by `R`. The default behaviour is equivalent
to passing `f=identity` and `R=parsetype(p)`.
"""
function parse(f, ::Type{R}, p::Parser, s; onfail=default_onfail) where {R}
    v, i′ = tryparse(f, R, Seq(p, End())[1], s)
    if v isa Fail
        onfail(s, i′)
    else
        v::R
    end
end
function parse(f, ::Type{R}, p::Parser, s, i; onfail=default_onfail, greedy=true) where {R}
    v, i′ = tryparse(f, R, greedy ? Seq(p, End())[1] : p, s, i)
    if v isa Fail
        onfail(s, i′), i′
    else
        v::R, i′
    end
end
parse(p::Parser{R}, args...; opts...) where {R} = parse(identity, R, p, args...; opts...)

function default_onfail(s, i)
    if i ≤ lastindex(s)
        error("parsing error at index $i")
    else
        error("parsing error at end of input")
    end
end

"""
    Always() :: Parser{Nothing}

Always succeeds.
"""
struct Always <: Parser{Nothing} end
tryparse(f::Cb, p::Always, s::Src, i::Idx) = retval(f, nothing, s, i)
Caseless(p::Always) = p

"""
    Never() :: Parser{Nothing}

Never succeeds.
"""
struct Never <: Parser{Nothing} end
tryparse(f::Cb, p::Never, s::Src, i::Idx) = reterr(f, i)
Caseless(p::Never) = p

"""
    Start() :: Parser{Nothing}

Succeeds at the start of the input.
"""
struct Start <: Parser{Nothing} end
tryparse(f::Cb, p::Start, s::Src, i::Idx) = atstart(s,i) ? retval(f, nothing, s, i) : reterr(f, i)
Caseless(p::Start) = p

"""
    End() :: Parser{Nothing}

Succeeds at the end of the input.
"""
struct End <: Parser{Nothing} end
tryparse(f::Cb, p::End, s::Src, i::Idx) = atend(s,i) ? retval(f, nothing, s, i) : reterr(f, i)
Caseless(p::End) = p

"""
    CharIn(...) :: Parser{Nothing}

Succeeds if the current character is equal to or a member of any of the arguments.
"""
struct CharIn{sets} <: Parser{Nothing} end
CharIn(sets...) = CharIn{Tuple(set isa Union{Tuple,String,AbstractVector,AbstractSet} ? isbits(set) ? set : Tuple(set) : isbits(set) ? (set,) : error() for set in sets)}()
description(::CharIn{sets}) where {sets} = join([repr(x) for set in sets for x in (set isa AbstractRange ? (set,) : set)], " ")
tryparse(f::Cb, ::CharIn{sets}, s::Src, i::Idx) where {sets} =
    if atend(s, i)
        reterr(f, i)
    elseif _charin(curchar(s, i), Val(sets))
        retval(f, nothing, s, inc(s, i))
    else
        reterr(f, i)
    end
Caseless(::CharIn{sets}) where {sets} = CharIn(caseless_charsets(sets...)...)

@generated _charin(c, ::Val{sets}) where {sets} =
    foldr((set,x)->:((c ∈ $set) || $x), sets, init=false)

caseless_charsets(sets...) = Tuple(unique([set′ for set in sets for set′ in caseless_charsets(set)]))
caseless_charsets(set) = error("don't know how to make $(repr(set)) caseless")
caseless_charsets(set::Tuple) = Tuple(unique([c′ for c in set for c′ in caseless_chars(c)]))
caseless_chars(c::AbstractChar) = (lowercase(c), uppercase(c), c)

"""
    CharNotIn(...) :: Parser{Nothing}

Succeeds on any character, except those given as arguments (in the same manner as `CharIn`).
"""
struct CharNotIn{sets} <: Parser{Nothing} end
CharNotIn(sets...) = CharNotIn{Tuple(set isa Union{Tuple,String,AbstractVector,AbstractSet} ? isbits(set) ? set : Tuple(set) : isbits(set) ? (set,) : error() for set in sets)}()
description(::CharNotIn{sets}) where {sets} =
    isempty(sets) ? "Any char" : "Any char except " * join([repr(x) for set in sets for x in (set isa AbstractRange ? (set,) : set)], " ")
tryparse(f::Cb, ::CharNotIn{sets}, s::Src, i::Idx) where {sets} =
    if atend(s, i)
        reterr(f, i)
    elseif _charnotin(curchar(s, i), Val(sets))
        retval(f, nothing, s, inc(s, i))
    else
        reterr(f, i)
    end
Caseless(::CharNotIn{sets}) where {sets} = CharNotIn(caseless_charsets(sets...)...)

@generated _charnotin(c, ::Val{sets}) where {sets} =
    foldr((set,x)->:((c ∉ $set) && $x), sets, init=true)

"""
    Maybe(p::Parser, [d=nothing])

Parse using `p`, or if that fails then return `d`.
"""
struct Maybe{dflt, R, P<:Parser} <: Parser{R}
    parser :: P
end
Maybe{dflt,R}(p::Parser) where {dflt,R} = Maybe{dflt,R,typeof(p)}(p)
Maybe{dflt,R}(p) where {dflt,R} = Maybe{dflt,R}(Parser(p))
Maybe{dflt}(p::Parser) where {dflt} = Maybe{dflt, Union{typeof(dflt), parsetype(p)}}(p)
Maybe{dflt}(p) where {dflt} = Maybe{dflt}(Parser(p))
Maybe(p, dflt=nothing) = Maybe{dflt}(p)
description(p::Maybe{nothing}) = "Maybe"
description(p::Maybe{dflt}) where {dflt} = "Maybe (else $(repr(dflt)))"
function tryparse(f::Cb, p::Maybe{dflt}, s::Src, i::Idx) where {dflt}
    v, i′ = tryparse(f, p.parser, s, i)
    if isok(v)
        v, i′
    else
        retval(f, dflt, s, revert(i, i′))
    end
end
Caseless(p::Maybe{dflt,R}) where {dflt,R} = Caseless{dflt,R}(Caseless(p.parser))

"""
    Or(ps::Parser...)

Parse using the first `p` that matches.
"""
struct Or{R, Ps<:Tuple{Vararg{Parser}}} <: Parser{R}
    parsers :: Ps
end
Or{R}(ps::Parser...) where {R} = Or{R, typeof(ps)}(ps)
Or{R}(ps...) where {R} = Or{R}(map(Parser, ps)...)
Or(ps::Parser...) = Or{Union{map(parsetype, ps)...}}(ps...)
Or(ps...) = Or(map(Parser, ps)...)
description(p::Or) = "Either"
Caseless(p::Or{R}) where {R} = Or{R}(map(Caseless, p.parsers)...)

tryparse(f::Cb, p::Or, s::Src, i::Idx) = tryparse_or(f, p.parsers, s, i)

@inline tryparse_or(f::Cb, ps::Tuple{Vararg{Parser}}, s::Src, i::Idx) =
    if isempty(ps)
        reterr(f, i)
    else
        v, i′ = tryparse(f, ps[1], s, i)
        if isok(v)
            v, i′
        else
            tryparse_or(f, ps[2:end], s, revert(i, i′))
        end
    end

"""
    Seq(ps::Parser...)

Parse using each `p` in `ps` in turn.
"""
struct Seq{R, Ps<:Tuple{Vararg{Parser}}} <: Parser{R}
    parsers :: Ps
end
Seq{R}(ps::Parser...) where {R} = Seq{R,typeof(ps)}(ps)
Seq{R}(ps...) where {R} = Seq{R}(map(Parser, ps)...)
Seq(ps::Parser{Nothing}...) = Seq{Nothing}(ps...)
Seq(ps::Parser...) = Seq{Tuple{map(parsetype, ps)...}}(ps...)
Seq(ps...) = Seq(map(Parser, ps)...)
description(p::Seq) = "Sequence"
Caseless(p::Seq{R}) where {R} = Seq{R}(map(Caseless, p.parsers)...)

tryparse(f::Cb, p::Seq, s::Src, i::Idx) =
    tryparse_seq(f, p.parsers, s, i, parsetype(p)==Nothing ? nothing : ())

# Inner worker function for `tryparse(f, Seq(ps), s, i)`.
# Here, `ps` are the parsers still to run and `xs` is the value accumulated so far (either nothing or a tuple)
tryparse_seq(f::Cb, ps::Tuple{Vararg{Parser}}, s::Src, i::Idx, xs) =
    if isempty(ps)
        # no more parsers, we are finished
        retval(f, xs, s, i)
    elseif parsetype(ps[1])==Nothing || xs===nothing
        # if the inner parser ps[1] returns nothing, or we are returning nothing, then we
        # only care if the inner parser succeeds or not and can move on to the next inner parser
        v, i′ = tryparse(NothingCb(), ps[1], s, i)
        if isok(v)
            ps′ = ps[2:end]
            xs′ = xs===nothing ? nothing : (xs..., val(v))
            tryparse_seq(f, ps′, s, i′, xs′)
        else
            reterr(f, i′)
        end
    else
        # Here, the inner parser returns something other than nothing and we care about its
        # value. We *could* just call the parser with cb_identity and append the result to xs
        # but if the return value is not concrete this will be type-unstable and require some
        # memory allocations. Instead, we make the callback function itself call tryparse_seq
        # on the remaining parsers.
        #
        # Essentially we are doing the following:
        #   tryparse(R, ps[1], s, i) do x1, s1, i1
        #     tryparse(R, ps[2], s1, i1) do x2, s2, i2
        #       tryparse(R, ps[3], s2, i2) do x3, s3, i3
        #         ret(R, f((x1,x2,x3), s3, i3)...)
        #       end
        #     end
        #   end
        # instead of:
        #   v1, i1 = tryparse(ps[1], s, i)
        #   v2, i2 = tryparse(ps[1], s1, i1)
        #   v3, i3 = tryparse(ps[1], s2, i2)
        #   ret(R, f((val(x1),val(x2),val(x3)), s3, i3)...)
        tryparse(SeqCb(f, ps[2:end], xs), ps[1], s, i)
    end

struct SeqCb{R,F<:Cb{R},Ps<:Tuple{Vararg{Parser}},Xs<:Tuple} <: Cb{R}
    f :: F
    ps :: Ps
    xs :: Xs
end
retval(f::SeqCb, x, s::Src, i::Idx) = tryparse_seq(f.f, f.ps, s, i, (f.xs..., x))


"""
    agg_type(::Type{A}, ::Type{T}) :: Type

The result type of aggregating elements of type `T` with aggregator `A`. Typically a concrete subtype of `A`.

Aggregator types currently supported are: `Vector`, `Dict` (with `T` being a pair or 2-tuple), `String` and `Nothing`.

# Aggregators

An "aggregator" is just a type `A` which is used to indicate how we'd like to collect an
unknown number of values of a known type `T`. The interface has four parts:
- `xs = agg_init(A, T)` returns a mutable container into which the elements are collected. (Default: `agg_type(A,T)()`.)
- `agg_update(A, xs, x::T)` collects a new value `x` into `xs`. (Default: `push!(xs,x)`.)
- `agg_finish(A, xs)` returns the final collection. (Default: `xs`.)
- `agg_type(A, T)` is equal to `typeof(agg_finish(A, agg_init(A, T)))`.
"""
agg_type(::Type{Vector}, ::Type{T}) where {T} = Vector{T}
agg_type(::Type{Dict}, ::Type{Pair{K,V}}) where {K,V} = Dict{K,V}
agg_type(::Type{Dict}, ::Type{Tuple{K,V}}) where {K,V} = Dict{K,V}
agg_type(::Type{Nothing}, ::Type) = Nothing
agg_type(::Type{String}, ::Type) = String

"""
    agg_init(::Type{A}, ::Type{T})

A new mutable container into which values of type `T` are being aggregated.

Part of the aggregator interface. See [`agg_type`](@ref).
"""
agg_init(::Type{A}, ::Type{T}) where {A,T} = agg_type(A,T)()
agg_init(::Type{String}, ::Type) = Vector{Char}()

"""
    agg_update(::Type{A}, xs, x)

Aggregate `x` into container `xs`.

Part of the aggregator interface. See [`agg_type`](@ref).
"""
agg_update(::Type, xs, x) = push!(xs, x)
agg_update(::Type{Dict}, xs, x::Tuple{K,V}) where {K,V} = push!(xs, x[1]=>x[2])
agg_update(::Type{Nothing}, xs, x) = nothing

"""
    agg_finish(::Type{A}, xs)

The final aggregated result. Often but not always the same as `xs`.

Part of the aggregator interface. See [`agg_type`](@ref).
"""
agg_finish(::Type, xs) = xs
agg_finish(::Type{String}, xs) = String(xs)

struct AggUpdateCb{A,Xs} <: Cb{Nothing}
    xs :: Xs
end
AggUpdateCb(A::Type, xs) = AggUpdateCb{A,typeof(xs)}(xs)
retval(f::AggUpdateCb{A}, x, s, i) where {A} = (agg_update(A, f.xs, x); (ret(Nothing, nothing), i))

"""
    Many(p, [min=0], [max=nothing]; [agg], [delim])

A parser which matches between `min` and `max` copies of `p`.

If `delim` is given, it is a delimiter which must appear between items.

Results are aggregated using `agg`. If `p` is a `Parser{Nothing}` then `agg` defaults to `Nothing`, else `Vector`. See [`agg_type`](@ref).
"""
struct Many{min, max, agg, R, P<:Parser, D<:Union{Nothing,Parser}} <: Parser{R}
    parser :: P
    delimiter :: D
end
function Many(p, min::Integer=0, max::Union{Integer,Nothing}=nothing; agg=missing, delim=nothing)
    _p = Parser(p)
    _min = Int(min)
    _max = max===nothing ? nothing : Int(max)
    _agg = agg===missing ? parsetype(_p)==Nothing ? Nothing : Vector : agg
    _delim = delim===nothing ? nothing : Parser(delim)
    Many{_min, _max, _agg, agg_type(_agg, parsetype(_p)), typeof(_p), typeof(_delim)}(_p, _delim)
end
description(p::Many{min,max}) where {min,max} = string("Many (", min, max===nothing ? " or more" : " to $max", ")")
show_suffix(io::IO, p::Many, n) =
    if p.delimiter !== nothing
        println(io)
        print_indent(io, n)
        println(io, "with delimiter")
        show_indented(io, p.delimiter, n+1)
    end
Caseless(p::Many{min,max,agg}) where {min,max,agg} = Many(Caseless(p.parser), min, max; agg=agg, delim=p.delim===nothing ? nothing : Caseless(p.delim))
function tryparse(f::Cb, p::Many{min,max,agg}, s::Src, i::Idx) where {min,max,agg}
    xs = agg_init(agg, parsetype(p.parser))
    f′ = AggUpdateCb(agg, xs)
    if max === nothing || max > 0
        n = 0
        goodi = i
        while true
            # parse a value
            v, i = tryparse(f′, p.parser, s, i)
            if isok(v)
                n += 1
                goodi = i
                max !== nothing && max == n && @goto success
                # parse a delimiter
                if p.delimiter !== nothing
                    d, i = tryparse(NothingCb(), p.delimiter, s, i)
                    if isok(d)
                        # great!
                    elseif n ≥ min
                        # failed but we already have enough
                        i = revert(goodi, i)
                        @goto success
                    else
                        @goto fail
                    end
                end
            elseif n ≥ min
                # failed but we already have enough
                i = revert(goodi, i)
                @goto success
            else
                @goto fail
            end
        end
    end
    @label success
    return retval(f, agg_finish(agg, xs), s, i)
    @label fail
    return reterr(f, i)
end

"""
    Recursive{R}(f) :: Parser{R}

A parser which acts identically to `f()`.

Use it like this:
```julia
const node = Recursive(()->_node)  # get the definition of node from the future
...                                # parsers defined in terms of node
const _node = ...                  # the actual definition of node, in terms of these parsers
```

For efficiency, the function `f` should immediately return a `const` variable, so it can be inferred and inlined. Don't be tempted to put the definition inside the function.
"""
struct Recursive{R,F} <: Parser{R}
    get :: F
end
Recursive{R}(f) where {R} = Recursive{R, typeof(f)}(f)
Recursive(f) = Recursive{Any}(f)
Base.get(p::Recursive{R}) where {R} = p.get()::Parser{<:R}
tryparse(f::Cb, p::Recursive, s::Src, i::Idx) =
    tryparse(f, get(p), s, i)

"""
    Index() :: Parser{Int}

Always succeeds, with value the current index.
"""
struct Index <: Parser{Int} end
tryparse(f::Cb, p::Index, s::Src, i::Idx) = retval(f, idx(i), s, i)
Caseless(p::Index) = p

"""
    Range(p) :: Parser{UnitRange{Int}}

A parser identical to `p` but with value the range of the match.
"""
struct Range{P<:Parser} <: Parser{UnitRange{Int}}
    parser :: P
end
Range(p) = Range(Parser(p))
function tryparse(f::Cb, p::Range, s::Src, i::Idx)
    v, i′ = tryparse(NothingCb(), p.parser, s, i)
    if isok(v)
        retval(f, idx(i):prevind(src(s), idx(i′)), s, i′)
    else
        reterr(f, i′)
    end
end
Caseless(p::Range) = Range(Caseless(p.parser))

"""
    Capture(p, R::Type)
    Capture{R}(p)

Identical to `p` but the return value is the matching substring, of type `R`.

The first form will modify `R` to make it concrete, if possible, assuming the input is a `String` or `Vector{UInt8}`
- `SubString{S=String}`
- `Vector{T=UInt8}`
- `SubArray{T=UInt8,1,V=Vector{T}}`
"""
struct Capture{R, P<:Parser} <: Parser{R}
    parser :: P
end
Capture{R}(p::Parser) where {R} = Capture{R, typeof(p)}(p)
Capture{R}(p) where {R} = Capture{R}(Parser(p))
Capture(p, ::Type{R}) where {R} = Capture{R}(p)
Capture(p, ::Type{SubString}) = Capture(p, SubString{String})
Capture(p, ::Type{Vector}) = Capture(p, Vector{UInt8})
Capture(p, ::Type{SubArray{T,1,V}}) where {T,V} = Capture(p, SubArray{T,1,V,Tuple{UnitRange{Int}},true})
Capture(p, ::Type{SubArray{T,1}}) where {T} = Capture(p, SubArray{T,1,Vector{T}})
Capture(p, ::Type{SubArray{T}}) where {T} = Capture(p, SubArray{T,1})
Capture(p, ::Type{SubArray}) = Capture(p, SubArray{UInt8})
Capture(p) = Capture(p, SubString{String})
Caseless(p::Capture{R}) where {R} = Capture{R}(Caseless(p.parser))
function tryparse(f::Cb, p::Capture{R}, s::Src, i::Idx) where {R}
    v, i′ = tryparse(NothingCb(), p.parser, s, i)
    if isok(v)
        retval(f, _capture(R, src(s), idx(i), idx(i′)), s, i′)
    else
        reterr(f, i′)
    end
end

_capture(::Type{String}, s::String, i, i′) = s[i:prevind(s,i′)]
_capture(::Type{Vector}, s::Vector, i, i′) = s[i:prevind(s,i′)]
_capture(::Type{R}, s::AbstractString, i, i′) where {R} = convert(R, SubString(s, i, prevind(s, i′)))
_capture(::Type{R}, s::AbstractVector, i, i′) where {R} = convert(R, view(s, i:(i′-1)))

"""
    Tr(p, f, R) :: Parser{R}
    Tr{R}(p, f)

Apply the function `f` to the output of `p`.

Currently, the return type `R` must be explicitly provided.
"""
struct Tr{R, F, P<:Parser} <: Parser{R}
    func :: F
    parser :: P
end
Tr{R}(p::Parser, f) where {R} = Tr{R, typeof(f), typeof(p)}(f, p)
Tr{R}(p, f) where {R} = Tr{R}(Parser(p), f)
Tr(p, f, ::Type{R}) where {R} = Tr{R}(p, f)
Tr(p, ::Type{R}) where {R} = Tr(p, R, R)
description(p::Tr) = string("Transform ($(p.func))")
Caseless(p::Tr{R}) where {R} = Tr{R}(p.func, Caseless(p.parser))
tryparse(f::Cb, p::Tr, s::Src, i::Idx) =
    tryparse(TransformCb(f, p.func), p.parser, s, i)

"""
    TrSource(p, f, R) :: Parser{R}

Like `Tr(p, f, R)` but calls `f(x, s)` where `x` is the parsed value `s` is the source string.

Tip: Use `TrSource(Range(p), (i,s)->..., R)` to access both the source and the range of the match.
"""
struct TrSource{R, F, P<:Parser} <: Parser{R}
    func :: F
    parser :: P
end
TrSource{R}(p::Parser, f) where {R} = TrSource{R, typeof(f), typeof(p)}(f, p)
TrSource{R}(p, f) where {R} = TrSource{R}(Parser(p), f)
TrSource(p, f, ::Type{R}) where {R} = TrSource{R}(p, f)
TrSource(p, ::Type{R}) where {R} = TrSource(p, R, R)
description(p::TrSource) = string("Transform source ($(p.func))")
Caseless(p::TrSource{R}) where {R} = Tr{R}(p.func, Caseless(p.parser))
tryparse(f::Cb, p::TrSource, s::Src, i::Idx) =
    tryparse(TransformSourceCb(f, p.func), p.parser, s, i)

"""
    Const(p, value)
    Const{value}(p)

A parser identical to `p` but always returns the given value.
"""
struct Const{value, R, P<:Parser} <: Parser{R}
    parser :: P
end
Const{value, R}(p::Parser) where {value, R} = Const{value, R, typeof(p)}(p)
Const{value, R}(p) where {value, R} = Const{value, R}(Parser(p))
Const{value}(p) where {value} = Const{value, typeof(value)}(p)
Const(p, value) = Const{value}(p)
description(p::Const{value}) where {value} = "Const ($(repr(value)))"
Caseless(p::Const{value, R}) where {value, R} = Const{value, R}(Caseless(p.parser))
function tryparse(f::Cb, p::Const{value}, s::Src, i::Idx) where {value}
    v, i′ = tryparse(NothingCb(), p.parser, s, i)
    if isok(v)
        retval(f, value, s, i′)
    else
        reterr(f, i′)
    end
end

"""
    GetIndex(p, i)
    GetIndex{i}(p)

A parser identical to `p` but returns only the `i`th entry.

Or `i` may be a tuple of indices, and a tuple of the corresponding entries are returned.
"""
struct GetIndex{idx, R, P<:Parser} <: Parser{R}
    parser :: P
end
GetIndex{idx}(p::Parser) where {idx} = error("parser must return a `Tuple`, this returns a `$(parsetype(p))`")
GetIndex{idx}(p::Parser{Ts}) where {idx, Ts<:Tuple} =
    if idx isa Integer
        GetIndex{idx, Ts.parameters[idx], typeof(p)}(p)
    else
        GetIndex{idx, Tuple{[Ts.parameters[i] for i in idx]...}, typeof(p)}(p)
    end
GetIndex{idx}(p) where {idx} = GetIndex{idx}(Parser(p))
GetIndex(p, idx::Integer) = GetIndex{Int(idx)}(p)
GetIndex(p, idx::Tuple{Vararg{Integer}}) = GetIndex{Tuple(map(Int, idx))}(p)
description(p::GetIndex{idx}) where {idx} = idx isa Integer ? "Index $idx" : "Indices $idx"
Caseless(p::GetIndex{idx,R}) where {idx,R} = GetIndex{idx,R}(Caseless(p.parser))
tryparse(f::Cb, p::GetIndex{idx}, s::Src, i::Idx) where {idx} =
    tryparse(TransformCb(f, getindex_func{idx}()), p.parser, s, i)

struct getindex_func{idx} end

@generated (::getindex_func{idx})(x::Tuple) where {idx} =
    if idx isa Integer
        :(x[$idx])
    else
        :(($([:(x[$i]) for i in idx]...),))
    end

### LOOK AHEAD / LOOK BACK

"""
    NotAhead(p), Not(p) :: Parser{Nothing}

Negative look-ahead: succeeds if `p` fails and does not move the cursor.
"""
struct NotAhead{P<:Parser} <: Parser{Nothing}
    parser :: P
end
NotAhead(p) = NotAhead(Parser(p))
Caseless(p::NotAhead) = NotAhead(Caseless(p.parser))
function tryparse(f::Cb, p::NotAhead, s::Src, i::Idx)
    v, i′ = tryparse(NothingCb(), p.parser, s, i)
    if isok(v)
        reterr(f, i)
    else
        retval(f, nothing, s, i)
    end
end
const Not = NotAhead

"""
    Ahead(p) :: Parser{Nothing}

Positive look-ahed: identical to `p` but does not move the cursor.
"""
struct Ahead{P<:Parser} <: Parser{Nothing}
    parser :: P
end
Ahead(p) = Ahead(Parser(p))
Caseless(p::Ahead) = Ahead(Caseless(p.parser))
function tryparse(f::Cb, p::Ahead, s::Src, i::Idx)
    v, i′ = tryparse(NothingCb(), p.parser, s, i)
    if isok(v)
        retval(f, nothing, s, i)
    else
        reterr(f, i′)
    end
end

"""
    NotBehind(p) :: Parser{Nothing}

Negative look behind: succeeds if `p` does not match any substring ending here.
"""
struct NotBehind{P<:Parser} <: Parser{Nothing}
    parser :: P
end
NotBehind(p) = NotBehind(Parser(p))
Caseless(p::NotBehind) = NotBehind(Caseless(p.parser))

"""
    Behind(p) :: Parser{Nothing}

Positive look behind: succeeds if `p` succeeds on some substring ending here.
"""
struct Behind{P<:Parser} <: Parser{Nothing}
    parser :: P
end
Behind(p) = Behind(Parser(p))
Caseless(p::Behind) = Behind(Caseless(p.parser))

"""
    Named(p, name)
    Named{name}(p)

Identical to `p` but has a descriptive name.

Named parsers are not expanded out when printed, unless at the top level. They may have other uses, such as for debugging.
"""
struct Named{name, R, P<:Parser{R}} <: Parser{R}
    parser :: P
end
Named{name}(p::Parser{R}) where {name, R} = Named{name, R, typeof(p)}(p)
Named{name}(p) where {name} = Named{name}(Parser(p))
Named(p, name) = Named{name}(p)
unnamed(p::Named{name}) where {name} = unnamed(p.parser)[1], name
unnamed(p::Parser) = p, nothing
tryparse(f::Cb, p::Named, s::Src, i::Idx) = tryparse(f, p.parser, s, i)

### SUGAR

Base.:(*)(p::Parser, ps::Parser...) = Seq(p, ps...)

Base.:(+)(p::Parser, ps::Parser...) = Or(p, ps...)

Base.:(|)(p::Parser, q::Parser) = Or(p, q)
Base.:(|)(p::Parser, q::Or) = Or(p, q.parsers...)
Base.:(|)(p::Or, q::Parser) = Or(p.parsers..., q)
Base.:(|)(p::Or, q::Or) = Or(p.parsers..., q.parsers...)

Base.getindex(p::Parser, i::Integer) = GetIndex(p, i)
Base.getindex(p::Parser, i::Integer, js::Integer...) = GetIndex(p, (i,js...))

Base.getproperty(p::Parser, k::Symbol) =
    if k === :Range
        (args...; opts...) -> Range(p, args...; opts...)
    elseif k === :Capture
        (args...; opts...) -> Capture(p, args...; opts...)
    elseif k === :Const
        (args...; opts...) -> Const(p, args...; opts...)
    elseif k === :GetIndex
        (args...; opts...) -> GetIndex(p, args...; opts...)
    elseif k === :Tr
        (args...; opts...) -> Tr(p, args...; opts...)
    elseif k === :TrSource
        (args...; opts...) -> TrSource(p, args...; opts...)
    elseif k === :Named
        (args...; opts...) -> Named(p, args...; opts...)
    else
        getfield(p, k)
    end


### EXAMPLES

module Examples

    module JSON
        using ...FastParsers

        const TEST = """  [ -1.11111111111111111111111111111111111111111111111111e-2 , 2 , 3 , [] , true , false , null, { "foo" : 1, "bar" : [], "baz" : null } ]  """

        const ws = Many(CharIn(" \t\n\r")).Named(:ws)
        const value = Recursive(()->_value).Named(:value)
        const json = Seq(ws, value, ws, End())[2].Named(:json)

        const digit = CharIn('0':'9')
        const integer = Or('0', Seq(CharIn('1':'9'), Many(digit)))
        const fraction = Seq('.', Many(digit, 1))
        const exponent = Seq(CharIn("eE"), Maybe(CharIn("+-")), Many(digit, 1))
        const number = Seq(Maybe('-'), integer, Maybe(fraction), Maybe(exponent)).Range().TrSource((i,s)->parse(Float64, SubString(s,i)), Float64).Named(:number)

        const hex = CharIn('0':'9', 'a':'f', 'A':'F')
        const escape = Or(CharIn("\"\\/bfnrt"), Seq('u', hex, hex, hex, hex))
        const character = Or(CharNotIn('"', '\\', Char(0x00):Char(0x19)), Seq('\\', escape))
        const string = Seq('"', Many(character).Capture(String), '"')[2].Named(:string)

        const tru = Const("true", true).Named(:true)
        const fls = Const("false", false).Named(:false)
        const null = Const("null", nothing).Named(:null)

        const array = Seq('[', ws, Many(Seq(value, ws)[1], delim=Seq(',', ws)), ']')[3].Named(:array)

        const object = Seq('{', ws, Many(Seq(string, ws, ':', ws, value, ws)[1,5], delim=Seq(',', ws), agg=Dict), '}')[3].Named(:object)

        const _value = Or(number, string, tru, fls, null, array, object)

        # experimentation
        const basic_value = Or(number, string, tru, fls, null)
        const true_array = Seq('[', ws, Many(Seq(tru, ws)[1], delim=Seq(',', ws)), ']')[3]
        const bool_array = Seq('[', ws, Many(Seq(Or(tru, fls), ws)[1], delim=Seq(',', ws)), ']')[3]

    end

    module ARFF

        using ...FastParsers

        abstract type ARFFType end
        struct NumericType <: ARFFType end
        struct StringType <: ARFFType end
        struct DateType <: ARFFType
            format :: String
        end
        struct NominalType <: ARFFType
            classes :: Vector{String}
        end

        abstract type HeaderEntry end
        struct DataStart <: HeaderEntry end
        struct Attribute <: HeaderEntry
            name :: String
            type :: Type
        end
        struct Relation <: HeaderEntry
            name :: String
        end

        const ws = Many(' ').Named(:ws)
        const comment = Seq('%', Many(CharNotIn("\n")).Capture())[2].Named(:comment)

        # TODO: allow escape characters
        const rstring = Many(CharNotIn(Char(0x00):Char(0x20), '{', '}', ',', '%', '\'', '"'), 1).Capture(String).Named(:rstring)
        const dstring = Seq('"', Many(CharNotIn('\\','"')).Capture(String), '"')[2].Named(:dstring)
        const sstring = Seq('\'', Many(CharNotIn('\\', '\'')).Capture(String), '\'')[2].Named(:sstring)
        const string = Or(rstring, dstring, sstring).Named(:string)

        # TODO: relational type (I don't really know what this is for)
        const numeric_t = Caseless(Or("numeric", "integer", "real")).Tr(x->NumericType(), NumericType).Named(:numeric_t)
        const string_t = Caseless("string").Tr(x->StringType(), StringType).Named(:string_t)
        const date_t = Seq(Caseless("date"), ws, string)[3].Tr(x->DateType(x), DateType).Named(:date_t)
        const nominal_t = Seq('{', ws, Many(Seq(string, ws)[1], delim=Seq(',', ws)), '}')[3].Tr(x->NominalType(x), NominalType).Named(:nominal_t)
        const type = Or{ARFFType}(numeric_t, string_t, date_t, nominal_t)

        const relation = Seq(Caseless("relation"), ws, string)[3].Tr(x->Relation(x), Relation).Named(:relation)
        const attribute = Seq(Caseless("attribute"), ws, string, ws, type)[3,5].Tr(x->Attribute(x...), Attribute).Named(:attribute)
        const datastart = Caseless("data").Tr(x->DataStart(), DataStart).Named(:data)
        const header = Or{HeaderEntry}(relation, attribute, datastart)
        const header_line = Seq(ws, Maybe(Seq('@', header, ws)[2]), Maybe(comment))[2,3]

        # TODO: sparse format
        const datum = Or(Const('?', missing), string).Named(:datum)
        const data_line = Seq(ws, Many(Seq(datum, ws)[1], delim=Seq(',', ws)), Maybe(comment))[2,3]
    end
end

end # module
