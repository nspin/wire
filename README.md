# wire

This library provides parser combinators with neither lookahead not backtracking.
In addition to speed, these restrictions allows for parsers that always consume all of their input.
That is, nontrivial parsers will always know in advance a nonzero minimum number of bytes they will consume.
This property is useful for reading from binary communication endpoints with types like `Int -> m ByteString`.
With the ability to provide bounds on the size of input chunks, one can parse from such an endpoint without having to deal with unconsumed input of successful parses.

Of course, these restrictions come with limitations.
For example, these parsers are not `Alternative`.
In fact, *every* parser under in this library could in theory be written in terms of `getByte :: Get Word8`.
Aggregate operations are optimized, however.

Many binary network protocols and file formats are designed not to require lookahead nor backtracking.
Such protocols and formats are the intended use case for this library.
