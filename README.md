# Advent of Code 2022 in Clojure

Over time I've added a few things to the template to make things faster:

* requires of `clojure.set` and `clojure.string`
* the `input-lines` function
* a `dbg` macro (which I could probably do without if I were any good at
  repl-driven development)

Instead of having things inlined into the template, I should probably have
them in a separate `util` namespace and require that also. If I add much more
I'll do that.

There are a few other things that would've been nice to have, and if I need
them again I'll ad them:

* The `non-delim-group?` function from day01, or a version of `partition-by`
  that assumes delimiters. (Of course, if I would just read the whole input
  into a string and then use `str/split` I could get that.)
* The `mapall` function from day13, and an equivalent `interleaveall`.
  (Probably with hyphenated names to match `partition-all`. It's possible that
  I'm missing something easy that can be done with the standard library
  functions, but it really is a pain that `map` and `interleave` just silenty
  ignore some of the data you pass them.
