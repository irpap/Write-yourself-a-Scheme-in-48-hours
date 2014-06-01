Write Yourself a Scheme in 48 Hours
===================================

My solutions to [this Haskell tutorial](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

The final version is the one of [chapter 10](https://github.com/irpap/Write-yourself-a-Scheme-in-48-hours/tree/master/chapter10-standard-library)

#### To run the REPL:
```
cd chapter10-standard-library
runhaskell SchemeParser.hs
```
Once in the REPL, you can load the standard library file, and run some code.
```
Lisp>>> (load "stdlib.scm")
(lambda ("pred" "lst") ...)
Lisp>>> (map (curry + 2) '(1 2 3 4))
(3 4 5 6)
Lisp>>> (filter even? '(1 2 3 4))
(2 4)
```  
#### Or, to run a file:
```
runhaskell SchemeParser.hs <inputfile>
```
