

# Module deque #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


A deque is a double-ended queue.

<a name="description"></a>

## Description ##
It has amortized constant access at
both ends and it needs slightly less memory compared to a `sequence` because
it is not annotated with any monoid.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#concat-2">concat/2</a></td><td></td></tr><tr><td valign="top"><a href="#foldl-3">foldl/3</a></td><td></td></tr><tr><td valign="top"><a href="#foldr-3">foldr/3</a></td><td></td></tr><tr><td valign="top"><a href="#from_list-1">from_list/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_empty-1">is_empty/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td></td></tr><tr><td valign="top"><a href="#peekl-1">peekl/1</a></td><td></td></tr><tr><td valign="top"><a href="#peekr-1">peekr/1</a></td><td></td></tr><tr><td valign="top"><a href="#popl-1">popl/1</a></td><td></td></tr><tr><td valign="top"><a href="#popr-1">popr/1</a></td><td></td></tr><tr><td valign="top"><a href="#pushl-2">pushl/2</a></td><td></td></tr><tr><td valign="top"><a href="#pushr-2">pushr/2</a></td><td></td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="concat-2"></a>

### concat/2 ###

`concat(Xs, Ys) -> any()`


<a name="foldl-3"></a>

### foldl/3 ###

`foldl(Fun, Acc, D) -> any()`


<a name="foldr-3"></a>

### foldr/3 ###

`foldr(Fun, Acc, D) -> any()`


<a name="from_list-1"></a>

### from_list/1 ###

`from_list(L) -> any()`


<a name="is_empty-1"></a>

### is_empty/1 ###

`is_empty(X1) -> any()`


<a name="new-0"></a>

### new/0 ###

`new() -> any()`


<a name="peekl-1"></a>

### peekl/1 ###

`peekl(X1) -> any()`


<a name="peekr-1"></a>

### peekr/1 ###

`peekr(X1) -> any()`


<a name="popl-1"></a>

### popl/1 ###

`popl(X1) -> any()`


<a name="popr-1"></a>

### popr/1 ###

`popr(X1) -> any()`


<a name="pushl-2"></a>

### pushl/2 ###

`pushl(A, X2) -> any()`


<a name="pushr-2"></a>

### pushr/2 ###

`pushr(A, X2) -> any()`


<a name="size-1"></a>

### size/1 ###

`size(D) -> any()`


<a name="to_list-1"></a>

### to_list/1 ###

`to_list(D) -> any()`


