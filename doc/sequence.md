

# Module sequence #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-sequence">sequence()</a> ###



<pre><code>
sequence() = <a href="#type-sequence">sequence</a>(any())
</code></pre>





### <a name="type-sequence">sequence()</a> ###



<pre><code>
sequence(X) = {sequence, <a href="finger_tree.md#type-finger_tree">finger_tree:finger_tree</a>(integer(), X)}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#concat-2">concat/2</a></td><td></td></tr><tr><td valign="top"><a href="#foldl-3">foldl/3</a></td><td></td></tr><tr><td valign="top"><a href="#foldr-3">foldr/3</a></td><td></td></tr><tr><td valign="top"><a href="#from_list-1">from_list/1</a></td><td></td></tr><tr><td valign="top"><a href="#insert-3">insert/3</a></td><td>Insert the element such that it becomes the Nth element.</td></tr><tr><td valign="top"><a href="#is_empty-1">is_empty/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td></td></tr><tr><td valign="top"><a href="#nth-2">nth/2</a></td><td></td></tr><tr><td valign="top"><a href="#peekl-1">peekl/1</a></td><td></td></tr><tr><td valign="top"><a href="#peekr-1">peekr/1</a></td><td></td></tr><tr><td valign="top"><a href="#popl-1">popl/1</a></td><td></td></tr><tr><td valign="top"><a href="#popr-1">popr/1</a></td><td></td></tr><tr><td valign="top"><a href="#pushl-2">pushl/2</a></td><td></td></tr><tr><td valign="top"><a href="#pushr-2">pushr/2</a></td><td></td></tr><tr><td valign="top"><a href="#replace-3">replace/3</a></td><td></td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td></td></tr><tr><td valign="top"><a href="#subvec-3">subvec/3</a></td><td></td></tr><tr><td valign="top"><a href="#to_list-1">to_list/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="concat-2"></a>

### concat/2 ###


<pre><code>
concat(Seq1::<a href="#type-sequence">sequence</a>(X), Seq2::<a href="#type-sequence">sequence</a>(X)) -&gt; <a href="#type-sequence">sequence</a>(X)
</code></pre>
<br />


<a name="foldl-3"></a>

### foldl/3 ###


<pre><code>
foldl(Fun::fun((X, A) -&gt; A), A, Seq::<a href="#type-sequence">sequence</a>(X)) -&gt; A
</code></pre>
<br />


<a name="foldr-3"></a>

### foldr/3 ###


<pre><code>
foldr(Fun::fun((X, A) -&gt; A), A, Seq::<a href="#type-sequence">sequence</a>(X)) -&gt; A
</code></pre>
<br />


<a name="from_list-1"></a>

### from_list/1 ###


<pre><code>
from_list(Xs::[X]) -&gt; <a href="#type-sequence">sequence</a>(X)
</code></pre>
<br />


<a name="insert-3"></a>

### insert/3 ###


<pre><code>
insert(N::integer(), X, Seq::<a href="#type-sequence">sequence</a>(X)) -&gt; <a href="#type-sequence">sequence</a>(X)
</code></pre>
<br />

Insert the element such that it becomes the Nth element
<a name="is_empty-1"></a>

### is_empty/1 ###


<pre><code>
is_empty(X1::<a href="#type-sequence">sequence()</a>) -&gt; boolean()
</code></pre>
<br />


<a name="new-0"></a>

### new/0 ###


<pre><code>
new() -&gt; <a href="#type-sequence">sequence()</a>
</code></pre>
<br />


<a name="nth-2"></a>

### nth/2 ###


<pre><code>
nth(N::integer(), Seq::<a href="#type-sequence">sequence</a>(X)) -&gt; X
</code></pre>
<br />


<a name="peekl-1"></a>

### peekl/1 ###


<pre><code>
peekl(Seq::<a href="#type-sequence">sequence</a>(X)) -&gt; X
</code></pre>
<br />


<a name="peekr-1"></a>

### peekr/1 ###


<pre><code>
peekr(Seq::<a href="#type-sequence">sequence</a>(X)) -&gt; X
</code></pre>
<br />


<a name="popl-1"></a>

### popl/1 ###


<pre><code>
popl(Seq::<a href="#type-sequence">sequence</a>(X)) -&gt; <a href="#type-sequence">sequence</a>(X)
</code></pre>
<br />


<a name="popr-1"></a>

### popr/1 ###


<pre><code>
popr(Seq::<a href="#type-sequence">sequence</a>(X)) -&gt; <a href="#type-sequence">sequence</a>(X)
</code></pre>
<br />


<a name="pushl-2"></a>

### pushl/2 ###


<pre><code>
pushl(X, Seq::<a href="#type-sequence">sequence</a>(X)) -&gt; <a href="#type-sequence">sequence</a>(X)
</code></pre>
<br />


<a name="pushr-2"></a>

### pushr/2 ###


<pre><code>
pushr(X, Seq::<a href="#type-sequence">sequence</a>(X)) -&gt; <a href="#type-sequence">sequence</a>(X)
</code></pre>
<br />


<a name="replace-3"></a>

### replace/3 ###


<pre><code>
replace(N::integer(), X, Seq::<a href="#type-sequence">sequence</a>(X)) -&gt; <a href="#type-sequence">sequence</a>(X)
</code></pre>
<br />


<a name="size-1"></a>

### size/1 ###


<pre><code>
size(X1::<a href="#type-sequence">sequence()</a>) -&gt; integer()
</code></pre>
<br />


<a name="subvec-3"></a>

### subvec/3 ###


<pre><code>
subvec(L::integer(), H::integer(), Seq::<a href="#type-sequence">sequence</a>(X)) -&gt; <a href="#type-sequence">sequence</a>(X)
</code></pre>
<br />


<a name="to_list-1"></a>

### to_list/1 ###


<pre><code>
to_list(Seq::<a href="#type-sequence">sequence</a>(X)) -&gt; [X]
</code></pre>
<br />


