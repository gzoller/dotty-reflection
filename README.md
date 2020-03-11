# Dotty Reflection

Dotty is the exciting new experimental language platform slated to evolve into Scala 3.  One of the big changes for Dotty is that Scala runtime reflection has been eliminated in favor of compile-time reflection, either through macros or Tasty file inspection.  

This project seeks to make Dotty reflection a bit more approachable by exposing a higher-level abstraction for reflected entities (Classes, Fields, etc.) not unlike in Java runtime reflection.

Full disclosure, this project is designed expressly to facilitate migration of ScalaJack, which is a heavy user of Scala runtime reflection, to Dotty, so the things pulled into the abstraction are driven by ScalaJack's needs.  That said, there's quite a bit there and it may be useful to others.

This library is provided both as a macro body, and as stand-alone Tasty reflection, which functions pretty closely to the old Scala reflection with one or two key limitations.

### Caveats
* This library is highly-experimental and subject to major change/breakage
* As stated, the reflected content is geared for ScalaJack, so your needs may be different
* Can't (yet) handle top-level paramterized classes, e.g. Foo[Int].  Foo can have a field typed Bar[Int], no problem though

The goal initially is to get this working.  It is not beautiful!  If you have better ways to do the same thing, please submit a PR!

### Usage
For Tasty Inspection:
```scala
import co.blocke.dotty_reflection
case class Thing(a: String)

val artifact: ConcreteType = Reflector.reflectOn[Thing]
// Concrete type here is typically a ScalaClassInfo or JavaClassInfo but could be something else if you reflected on, say, List[Foo], in which case you'd
// get back a Collection_A1_Info.

// Alternatively, if you have the Class instance:
val art2: ConcreteType = Reflector.reflectOnClass(clazz)
```
From the top-level ConcreteType you get back you can navigate into the internals of the class, which are themselves reflected items.

### Status
At this point the core Tasty inspection is done, and it inspects quite a lot of things in the Scala ecosystem:
* Dotty/Scala 3 Tasty classes (parameterized or non-parameterized) (see caveat on parameterized)
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
* Tuple typed fields

There's one really crucial thing *not* here:  The ability to detect T for a parameterized class on the top-level call.  For example:
```scala
case class Thing[T](a: T)

Reflector.reflectOn[Thing[Int]] 
```
Ideally this would be able to detect that T => Int here, but it can't because we aren't in the the Tasty inspection portion of the code yet.  Note that once inside the Tasty code, for example if T was itself a parameterized thing, the library can resolve the internal types to their actual types.

So how do we get that initial, top-level T resolved?  So far... I don't know!  I'm hoping there's another answer but it may require making the intial call a macro or somethign that plugs into a macro, so then T's actual type can be read from the caller's Tasty file.

ScalaJack note: Serialized traits use a type hint, but we only serialize concrete classes, not parameterized classes.  That's good.  That means we don't need to worry about top-level parameterized types in a type hint--and--the type hint is the truly runtime bit in ScalaJack.  A hybrid solution using macros to kick things off, then Tasty inspection for the type hints should be workable.
