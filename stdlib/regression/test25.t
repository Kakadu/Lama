This file was autogenerated.
  $ ../../src/Driver.exe -runtime ../../runtime -I ../../runtime -I ../../stdlib/x64 -ds -dp test25.lama -o test 2>&1 | grep -v 'missing .note.GNU-stack'
  /usr/bin/ld: NOTE: This behaviour is deprecated and will be removed in a future version of the linker
  $ ./test
  Cloning int: 5
  Cloning string: abc
  Cloning array: [1, 2, 3, 4, 5]
  Cloning sexp: A (1, 2, 3, 4, 5)
  Cloning closure: address ok, 5, 6
