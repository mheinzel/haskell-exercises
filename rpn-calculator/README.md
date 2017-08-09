# rpn-calculator

Implement a [Reverse Polish Notation](https://en.wikipedia.org/wiki/Reverse_Polish_notation) calculator!

`evaluate "2 3 +"` -> 5
`evaluate "2 3 + 7 *"` -> 35


Test are provided in the `test` directory. Feel free to add some more, if you want!
You can run them using `stack test`.


= Possible extensions =

- give good error messages (with a custom error type or just a String)
  (some tests for this are provided as a comment in `Spec.hs`)
- add more binary (e.g. division or minus) or unary (e.g. abs) operators
- let your program create a step-wise trace of program execution
- build a simple REPL around it
