

# Module finger_tree #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-finger_tree">finger_tree()</a> ###



<pre><code>
finger_tree(V, X) = {finger_tree, {<a href="#type-meas_fun">meas_fun</a>(V, X), V, <a href="#type-op_fun">op_fun</a>(V, X)}, <a href="#type-ft_tree">ft_tree</a>(V, X)}
</code></pre>





### <a name="type-ft_digit">ft_digit()</a> ###



<pre><code>
ft_digit(X) = {digit, X} | {digit, X, X} | {digit, X, X, X} | {digit, X, X, X, X}
</code></pre>





### <a name="type-ft_node">ft_node()</a> ###



<pre><code>
ft_node(V, X) = {node, V, X, X} | {node, V, X, X, X}
</code></pre>





### <a name="type-ft_tree">ft_tree()</a> ###



<pre><code>
ft_tree(V, X) = {tree} | {tree, X} | {tree, V, <a href="#type-ft_digit">ft_digit</a>(X), <a href="#type-ft_tree">ft_tree</a>(V, <a href="#type-ft_node">ft_node</a>(V, X)), <a href="#type-ft_digit">ft_digit</a>(X)}
</code></pre>





### <a name="type-meas_fun">meas_fun()</a> ###



<pre><code>
meas_fun(V, X) = fun((X) -&gt; V)
</code></pre>





### <a name="type-op_fun">op_fun()</a> ###



<pre><code>
op_fun(V, X) = fun((X, X) -&gt; V)
</code></pre>





### <a name="type-pred_fun">pred_fun()</a> ###



<pre><code>
pred_fun(V) = fun((V) -&gt; boolean())
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#concat-2">concat/2</a></td><td></td></tr><tr><td valign="top"><a href="#dropwhile-2">dropwhile/2</a></td><td></td></tr><tr><td valign="top"><a href="#foldl-3">foldl/3</a></td><td></td></tr><tr><td valign="top"><a href="#foldr-3">foldr/3</a></td><td></td></tr><tr><td valign="top"><a href="#is_empty-1">is_empty/1</a></td><td></td></tr><tr><td valign="top"><a href="#measure-1">measure/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-3">new/3</a></td><td></td></tr><tr><td valign="top"><a href="#peekl-1">peekl/1</a></td><td></td></tr><tr><td valign="top"><a href="#peekr-1">peekr/1</a></td><td></td></tr><tr><td valign="top"><a href="#popl-1">popl/1</a></td><td></td></tr><tr><td valign="top"><a href="#popr-1">popr/1</a></td><td></td></tr><tr><td valign="top"><a href="#pushl-2">pushl/2</a></td><td></td></tr><tr><td valign="top"><a href="#pushr-2">pushr/2</a></td><td></td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td></td></tr><tr><td valign="top"><a href="#split-2">split/2</a></td><td></td></tr><tr><td valign="top"><a href="#takewhile-2">takewhile/2</a></td><td></td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="concat-2"></a>

### concat/2 ###

`concat(X1, X2) -> any()`


<a name="dropwhile-2"></a>

### dropwhile/2 ###


<pre><code>
dropwhile(P::<a href="#type-pred_fun">pred_fun</a>(V), FT::<a href="#type-finger_tree">finger_tree</a>(V, X)) -&gt; <a href="#type-finger_tree">finger_tree</a>(V, X)
</code></pre>
<br />


<a name="foldl-3"></a>

### foldl/3 ###


<pre><code>
foldl(Fun::fun((X, A) -&gt; A), A, X3::<a href="#type-finger_tree">finger_tree</a>(_V, X)) -&gt; A
</code></pre>
<br />


<a name="foldr-3"></a>

### foldr/3 ###


<pre><code>
foldr(Fun::fun((X, A) -&gt; A), A, X3::<a href="#type-finger_tree">finger_tree</a>(_V, X)) -&gt; A
</code></pre>
<br />


<a name="is_empty-1"></a>

### is_empty/1 ###


<pre><code>
is_empty(X1::<a href="#type-finger_tree">finger_tree</a>(term(), term())) -&gt; boolean()
</code></pre>
<br />


<a name="measure-1"></a>

### measure/1 ###


<pre><code>
measure(X1::<a href="#type-finger_tree">finger_tree</a>(V, _X)) -&gt; V
</code></pre>
<br />


<a name="new-3"></a>

### new/3 ###


<pre><code>
new(Meas::<a href="#type-meas_fun">meas_fun</a>(V, X), V, Op::<a href="#type-op_fun">op_fun</a>(V, X)) -&gt; <a href="#type-finger_tree">finger_tree</a>(V, X)
</code></pre>
<br />


<a name="peekl-1"></a>

### peekl/1 ###


<pre><code>
peekl(X1::<a href="#type-finger_tree">finger_tree</a>(V, X)) -&gt; <a href="#type-finger_tree">finger_tree</a>(V, X)
</code></pre>
<br />


<a name="peekr-1"></a>

### peekr/1 ###


<pre><code>
peekr(X1::<a href="#type-finger_tree">finger_tree</a>(V, X)) -&gt; <a href="#type-finger_tree">finger_tree</a>(V, X)
</code></pre>
<br />


<a name="popl-1"></a>

### popl/1 ###


<pre><code>
popl(X1::<a href="#type-finger_tree">finger_tree</a>(V, X)) -&gt; <a href="#type-finger_tree">finger_tree</a>(V, X)
</code></pre>
<br />


<a name="popr-1"></a>

### popr/1 ###


<pre><code>
popr(X1::<a href="#type-finger_tree">finger_tree</a>(V, X)) -&gt; <a href="#type-finger_tree">finger_tree</a>(V, X)
</code></pre>
<br />


<a name="pushl-2"></a>

### pushl/2 ###


<pre><code>
pushl(X, X2::<a href="#type-finger_tree">finger_tree</a>(V, X)) -&gt; <a href="#type-finger_tree">finger_tree</a>(V, X)
</code></pre>
<br />


<a name="pushr-2"></a>

### pushr/2 ###


<pre><code>
pushr(X, X2::<a href="#type-finger_tree">finger_tree</a>(V, X)) -&gt; <a href="#type-finger_tree">finger_tree</a>(V, X)
</code></pre>
<br />


<a name="size-1"></a>

### size/1 ###


<pre><code>
size(X1::<a href="#type-finger_tree">finger_tree</a>(term(), term())) -&gt; non_neg_integer()
</code></pre>
<br />


<a name="split-2"></a>

### split/2 ###


<pre><code>
split(P::<a href="#type-pred_fun">pred_fun</a>(V), X2::<a href="#type-finger_tree">finger_tree</a>(V, X)) -&gt; {<a href="#type-finger_tree">finger_tree</a>(V, X), <a href="#type-finger_tree">finger_tree</a>(V, X)}
</code></pre>
<br />


<a name="takewhile-2"></a>

### takewhile/2 ###


<pre><code>
takewhile(P::<a href="#type-pred_fun">pred_fun</a>(V), FT::<a href="#type-finger_tree">finger_tree</a>(V, X)) -&gt; <a href="#type-finger_tree">finger_tree</a>(V, X)
</code></pre>
<br />


<a name="to_list-1"></a>

### to_list/1 ###

`to_list(X1) -> any()`


