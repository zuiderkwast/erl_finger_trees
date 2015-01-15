Erlang 2-3 Finger Trees
=======================

An Erlang implementation of 2-3 finger trees as described by Ralf Hinze and
Ross Paterson in their 2006 paper and on the page at
http://staff.city.ac.uk/~ross/papers/FingerTree.html.

This is a general purpose data structure that efficiently implements a deque,
but it allows specializing (by annotating it with a monoid) to efficiently
implement a sequence, priority queue and more. The module `deque` is a plain
2-3 finger tree while the others are instances of the annotated finger tree.

Generated documentation in Markdown format is available in the [doc/](doc/)
directory.

Sequence
--------

The `sequence` module implements the same data structure as the Haskell type
`Data.Sequence`.

* Access both ends in amortized O(1) time;
* access the nth element in O(log N) time;
* concatenation and splitting in O(log M) where M is the size of the smaller
  part.

There is a parse transform which introduces Erlang syntax for constructing
sequences. The syntax is `<<[foo, bar, baz]:sequence>>` which is translated
to a sequence at compile-time. Add `-compile({parse_transform, sequence_pt})`
to a source file to enable the transform.

Priority queue
--------------

* Push (insertion) in amortized O(1) time;
* Pop in O(log N) time.

Deque
-----

* Access in both ends in amortized O(1) time;
* access the nth element in O(log N) time;
* concatenation in O(log M) where M is the size of the smaller part.

Use `sequence` instead if you need efficient split.

TODO
----

* rename `sequence:subvec/3` to `slice`
* add `sequence:split/2` (where `split(N, Seq)` splits after the `N`th element)
  and `split/1` to split in or near the middle.
* add an ordered sequence (`ordseq`) implementation that is also an efficient
  "double-ended priority queue"
* add an interval tree implementation
