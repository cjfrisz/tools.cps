# tools.cps

A library for performing continuation passing style (CPS) transformations to Clojure code. 

Planned features include:

  * Tail call optimization similar to [Clojure TCO](https://github.com/cjfrisz/clojure-tco)
  * Control operators, including:
    * [`call-with-current-continuation`](http://en.wikipedia.org/wiki/Call-with-current-continuation) (call-cc)
    * `shift` and `reset` for [delimited control](http://en.wikipedia.org/wiki/Delimited_continuation)

## License

Copyright © 2013 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
