# Dotty Reflection

[![license](https://img.shields.io/github/license/mashape/apistatus.svg?maxAge=86400)](https://opensource.org/licenses/MIT)
[![bintray](https://api.bintray.com/packages/blocke/releases/dotty_reflection/images/download.svg)](https://bintray.com/blocke/releases/dotty-reflection/_latestVersion)

Dotty is the exciting new developmental language destined to evolve into Scala 3.  One of the big changes for Dotty is that Scala runtime reflection has been eliminated in favor of compile-time reflection, either through macros or Tasty file inspection.  

This project seeks to accomplish two goals:
* Make Dotty reflection a little more approachable by exposing a higher-level abstration for reflected things
* Allow for a runtime reflection capability (i.e. make compile-time reflection appear to work like run-time)

#### So what does it actually do???
This library is used to reflect on a class you provide and return high-level abstractions ("Info" classes) describing what was found by diving into the class and pulling out anything "interesting".  It does the work of combing through Dotty reflection internals, so you don't have to.

Full disclosure, this project is designed expressly to facilitate migration of ScalaJack, which is a heavy user of Scala runtime reflection, to Dotty so the things pulled into the abstraction are driven by ScalaJack's needs.  That said, there's quite a bit there.

### Caveat
* This library is highly-experimental and subject to major change/breakage
* The goal initially is to get this functionality working, not wind awards for beauty.  **If you have better ways to do the same thing, please submit a PR!**

### Usage
*NOTE*: Due to a dependency on a feature not included in Dotty 0.23 your client project will need to be based off 0.24.0-bin-20200320-30f8c6f-NIGHTLY or later until 0.24 comes out.

For Tasty Inspection:
```scala
import co.blocke.dotty_reflection

case class Thing(a: String)

val artifact: ConcreteType = Reflector.reflectOn[Thing]
// Concrete type here is typically a ScalaClassInfo or JavaClassInfo but could be something else if you reflected on, say, List[Foo], in which case you'd
// get back a SeqLikeInfo.

// Alternatively, if you have the Class instance:
val art2: ConcreteType = Reflector.reflectOnClass(clazz)
```
From the top-level ConcreteType you get back, you can navigate into the internals of the class, which are themselves reflected items.

### Status
At this point the core Tasty inspection is done, and it inspects quite a lot of things in the Scala ecosystem:
* Dotty/Scala 3 Tasty classes (parameterized or non-parameterized) 
* Traits (including sealed traits)
* Scala 2 case classes
* Java classes (Javabeans pattern)
* Scala 3 enum / Scala 2 Enumeration
* Union & Intersection types
* Opaque type aliases
* Try typed fields
* Either
* Option
* Collections, incl. Java Collections
* Tuple

See unit tests for detailed examples of usage.

### Acknowledgements

I wish to thank three people who have helped make this library possible, with their patient explanations and help on gitter and in code reviews.  Learning the Dotty reflection internals was a learning curve for me and these guys really helped me through it:
```
Guillaume Martres (@smarter)
Paolo G. Giarrusso (@Blaisorblade)
Nicolas Stucki (@nicolasstucki)
```

### 11 Laws Of ScalaJack Reflection
There are 11 things ScalaJack must reflect on in order to function. These are:

1. Case class vs non-case class identification
2. Identify and retrieve primary constructor method
3. Identify any mixins on the given/reflected object (specifically SJCapture)
4. Build a type parameter map [Symbol -> class]
5. Get any type members in given object (e.g. type myType = Foo)
6. Get primary constructor parameters w/types (a.k.a. class fields)
7. Determine if any class field is a Value Class and get details so it can be instantiated
8. Detect any default class field values and establish way to access (i.e. accessor method)
9. Get class and field annotations
10. Be able to pull apart collections (Map/List) with their generic types
11. Type equivalence (<:< and =:=)

If we've done the proper inventory, with these 11 laws solved for Dotty we should have enough for ScalaJack to serialize/deserialize objects to wire formats.  dotty_reflection should account for all 11 laws needed by ScalaJack, plus a few new tricks that are Dotty-specific (i.e. not used in ScalaJack--yet)