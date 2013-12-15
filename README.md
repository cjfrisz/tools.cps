# tools.cps

A library for performing continuation passing style (CPS) transformations to Clojure code. 

Planned features include:

  * Tail call optimization similar to [Clojure TCO](https://github.com/cjfrisz/clojure-tco)
  * Control operators, including:
    * [`call-with-current-continuation`](http://en.wikipedia.org/wiki/Call-with-current-continuation) (call-cc)
    * `shift` and `reset` for [delimited control](http://en.wikipedia.org/wiki/Delimited_continuation)

## License

Copyright © 2013 Chris Frisz

Licensed under the EPL (see the file epl.html)

Includes code from [Clojure TCO](https://github.com/cjfrisz/clojure-tco),
Copyright © 2013 Chris Frisz, Daniel P. Friedman. See included
LICENSE.ctco file.
