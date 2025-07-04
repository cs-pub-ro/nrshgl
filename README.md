Nrshgl
=======================

## Usefull mill commands

To run a single test

```
$ mill nrshgldut.default.testOnly #classPathToTest
```

To run all tests
```
$ mill nrshgldut.default.test
```

To generate verilog for a module, keep in mind that targets need to be addded in the file "VerilogGenerator.scala"
```
$ mill verilog #target
```

## Dependencies
* [Fixed-Point User Library for Chisel](https://github.com/ucb-bar/fixedpoint/tree/chisel5.1.0)
