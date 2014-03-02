# An iCal parser for Haskell

Yuppers. Trying to write a parser for the iCal specification RFC-5545 in
Haskell. I'll be adding functionality as required, starting with VEvents and
expanding from there.

### Why? Doesn't an iCal parser for Haskell exist?

Yes. And the code is completely untested and looks totally insane. This
iteration will be thoroughly unit-tested and more modular.

### Technologies

I'll be relying on Hspec for testing and Parsec for parsing. And Chris Wilson's
brain for figuring out how to actually write a parser. This shit is hard, yo.
