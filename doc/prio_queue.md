

# Module prio_queue #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-cmpfun">cmpfun()</a> ###



<pre><code>
cmpfun(X) = fun((X, X) -&gt; boolean())
</code></pre>





### <a name="type-maybe">maybe()</a> ###



<pre><code>
maybe(X) = {just, X} | nothing
</code></pre>





### <a name="type-prio_queue">prio_queue()</a> ###



<pre><code>
prio_queue() = <a href="#type-prio_queue">prio_queue</a>(any())
</code></pre>





### <a name="type-prio_queue">prio_queue()</a> ###



<pre><code>
prio_queue(X) = {prio_queue, <a href="#type-cmpfun">cmpfun</a>(X), <a href="finger_tree.md#type-finger_tree">finger_tree:finger_tree</a>(<a href="#type-maybe">maybe</a>(X), X)}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#is_empty-1">is_empty/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Creates a max-priority queue.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Creates a priority queue with a custom priority function.</td></tr><tr><td valign="top"><a href="#pop-1">pop/1</a></td><td></td></tr><tr><td valign="top"><a href="#push-2">push/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="is_empty-1"></a>

### is_empty/1 ###


<pre><code>
is_empty(X1::<a href="#type-prio_queue">prio_queue()</a>) -&gt; boolean()
</code></pre>
<br />


<a name="new-0"></a>

### new/0 ###


<pre><code>
new() -&gt; <a href="#type-prio_queue">prio_queue()</a>
</code></pre>
<br />

Creates a max-priority queue.
<a name="new-1"></a>

### new/1 ###


<pre><code>
new(CmpFun::<a href="#type-cmpfun">cmpfun</a>(X)) -&gt; <a href="#type-prio_queue">prio_queue</a>(X)
</code></pre>
<br />

Creates a priority queue with a custom priority function.
CmpFun is a function that returns true if the first argument has a
higher priority than the second argument, and false otherwise. If CmpFun
behaves like `<`, it is a min priority queue, if it that behaves like `>`,
it is a max priority queue.
<a name="pop-1"></a>

### pop/1 ###


<pre><code>
pop(X1::<a href="#type-prio_queue">prio_queue</a>(X)) -&gt; {X, <a href="#type-prio_queue">prio_queue</a>(X)}
</code></pre>
<br />


<a name="push-2"></a>

### push/2 ###


<pre><code>
push(X, X2::<a href="#type-prio_queue">prio_queue</a>(X)) -&gt; <a href="#type-prio_queue">prio_queue</a>(X)
</code></pre>
<br />


