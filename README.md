# Dotty Reflection

Dotty is the exciting new experimental language platform slated to evolve into Scala 3.  One of the big changes for Dotty is that Scala runtime reflection has been eliminated in favor of compile-time reflection.  This capability can either happen as a macro, or as an inspection of .tasty files.  This project utilizes the latter approach, as it is initiated at runtime, even though the information in the .tasty files is gathered at compile-time.

Tasty inspection takes us quite a ways down the path to replacing the old Scala reflection's abilities, however not the entire distance.  For example, Tasty inspection can look at some class ```Foo[T]``` and tell us there's a type parameter 'T', but it can't tell you at runtime that 'T' is a Double.  To get that information we must marry the rich information we get from Tasty inspection with the information we can glean at runtime through Java runtime reflection.

### Caveats
* This library is highly-experimental and subject to major change/breakage
* The content captured during reflection is geared for use in ScalaJack serialization, so your needs may be different

### Usage
(TBD)
