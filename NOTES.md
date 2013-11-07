* Parameterize HTTP stuff into typeclass constraint MonadReader Auth m
* Use free monads for HTTP
* http://skillsmatter.com/podcast/home/monads-for-free
* edwardk's free libarary
* FreeT
* MonadHttp (runHttp, runHttpMock)

```
so, say I have some free monad and I do: Foo >> Bar >> Baz                                              │ aegray
where Foo Bar and Baz are constructors for my monad instance type                                       │ aford
the result of this code will not *do* anything, but return a structure:                                 │ AgentM
Free (Foo (Free (Bar (Free (Baz (Pure ())))))), or something similar                                    │ agundry
now I can iterate over this structure using a recursive function                                        │ ahf
it's then that I *do* something                                                                         │ ahihi
either, print the structure, mock it, perform the executions, etc.                                      │ akahn
it's almost exactly the same thing as parsing, except Haskell's do-notation and operators are doing the │ akraut
parsing for you    
```
