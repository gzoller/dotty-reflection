# Dotty Reflection

  

[![license](https://img.shields.io/github/license/mashape/apistatus.svg?maxAge=86400)](https://opensource.org/licenses/MIT)

[![bintray](https://api.bintray.com/packages/blocke/releases/dotty-reflection/images/download.svg)](https://bintray.com/blocke/releases/dotty-reflection/_latestVersion)

  

Dotty is the exciting new developmental language destined to evolve into Scala 3. One of the big changes for Dotty is that Scala runtime reflection has been eliminated in favor of compile-time reflection, either through macros or Tasty file inspection.

  

This project seeks to accomplish two goals:

* Make Dotty reflection a little more approachable by exposing a higher-level abstration for reflected things

* Allow for a runtime reflection capability (i.e. make compile-time reflection appear to work like run-time)

  

#### So what does it actually do?

This library is used to reflect on a class you provide and return high-level abstractions ("Info" classes) describing what was found by diving into the class and pulling out anything interesting. It does the work of combing through Dotty reflection internals, so you don't have to.

  

Full disclosure, this project is designed expressly to facilitate migration of ScalaJack to Dotty, which is a heavy user of Scala runtime reflection, so the things pulled into the abstraction are driven by ScalaJack's needs. That said, there's quite a bit there.

  

### Caveat

* This library is highly-experimental and subject to major change/breakage

* The goal initially is to get this functionality working, not win awards for beauty. **If you have better ways to do the same thing, please submit a PR!**

  

### Usage

In your build.sbt file be sure you've set co.blocke's releases repo in bintray as a resolver and add the current version of the library to libraryDependences:

```scala

resolvers += "co.blocke ivy resolver" at "https://dl.bintray.com/blocke/releases"

libraryDependencies += "co.blocke" %% "dotty-reflection" % CURRENT_VERSION

```

(CURRENT_VERSION value can be taken from the Download badge in this github repo.)

  

For Tasty Inspection:

```scala

import co.blocke.dotty_reflection

case class Thing(a: String)

val artifact: Transporter.RType = RType.of[Thing] // compile-time type specification
// In this case returned value would be ScalaCaseClassInfo

// Alternatively, if you have the Class instance:
val art2: myClassRType = RType.of(clazz) // run-time type specification

```

If you want to see a class in terms of a trait (used by ScalaJack's type hint/trait functionality), you can do this:

```scala

val rt = RType.inTermsOf[MyTrait](myTraitImplClazz) 
// where myTraitImplClazz is a class that implements MyTrait.

// Parameter substitution works for parameterized types:
val rt2 = RType.inTermsOf[MyTrait2[Option[String]]](myTraitImplClazz2)

```

From the top-level RType you get back, you can navigate into the internals of the class, which are themselves reflected items.

  

#### Learning to Drive

Because dotty-reflection uses macros to the fullest extent possible to do the hard work of reflecting on types, there is code running during the compile cycle (the macros). This will be non-intuitive at first. Think of this example:
```scala
// File1.scala
case class Foo(name: String)

// File2.scala
val fooRType = RType.of[Foo]
```
In a non-macro implementation if you update Foo in File1.scala you naturally expect sbt to re-compile this file, and anything that depends on Foo, and the changes will be picked up in your program.  That's NOT necessarily what happens with macros.  Remember the macro is code that is run at cmopile-time.  File2.scala needs to be re-compiled because the RType.of macro needs to be updated if you recompile Foo class.  Unfortunately sbt doesn't pick this up!  If you don't know any better you'll just re-run your program after a change and get a spectacular exception that won't mean much to you.  The solution is you need to also recompile File2.scala.


#### A Word about Performance

Compared to pre-Dotty ScalaJack, which used Scala 2.x runtime reflection, dotty-reflection is both much faster, and much slower than before. For classes that can be reflected on at compile-time (anytime you use RType.of[...]) there's a significant performance boost with dotty-reflection. Any time the library must fall back to runtime reflection ("inspection" in Dotty-speak), RType.of(...) or RType.inTermsOf[](), performance becomes alarmingly poor. The reason is that unlike Scala 2.x, which held a lot of reflection information ready-to-go in the compiled class file, Dotty must parse the .tasty file by first reading it using file IO. For a comparison: a macro-readable class (reflection) might process in 2 or 3 milliseconds. A class that needs Dotty inspection (runtime) might be anywhere from 0.2 to 2 full seconds to process. YIKES!  dotty-reflection does cache results, so this performance hit is only the first time you reflect on a runtime class.

#### Performance Update! ** RECOMMENDED **
Starting with version 0.2.0, there is now an included compiler plug-in that faintly mimics Scala 2's runtime reflection.  Classes compiled with this plugin have reflection information generated as part of the class, thus avoiding the costly .tasty file IO for runtime reflection and dramatically speeding up RType.of() and inTermsOf() calls.  YAY!  The results speak for themselves.  Here's a sample of a few of the more complex test cases:

|Test| No plugin  |With Plugin  |
|--|--|--|
|With nested case class and non-case class|2.464s|0.214s|
|Nested trait substitutions|0.345s|0.026s|
|With nested Option and List|0.282s|0.018s|
|With nested Try|0.246s|0.017s|
|With nested Map and Array|0.319s|0.018s|
|With nested case class and non-case class (inverted)|0.234s|0.013s|
|InTermsOf deep type substitution|0.239s|0.017s|

To use the compiler plugin, include this line in your build.sbt file:
```scala
addCompilerPlugin("co.blocke" %% "dotty-reflection" % CURRENT_VERSION)
```
(where CURRENT_VERSION is 0.2.0 or later)

So what if you try to use dotty-reflection with a class not compiled with the plugin?  In that case the library simply falls back to .tasty-based runtime inspection.  It'll be a slower the first time a class is encountered, but it will work fine.
 

### Status

At this point the core Tasty inspection is done, and it inspects quite a lot of things in the Scala ecosystem:

* Dotty/Scala 3 Tasty classes (parameterized or non-parameterized)

* Traits (including sealed traits)

* Scala 2 case classes

* Java classes (JavaBeans pattern)

* Scala 3 enum / Scala 2 Enumeration

* Union & Intersection types

* Opaque type aliases

* Try typed fields

* Either

* Option

* Collections, incl. Java Collections

* Tuple

  

See unit tests for detailed examples of usage.

  
  

### Limitations

* No support for prameters in Intersection or Untion types (```val t: X|Y``` or ```val u: X&Y```). This is because union/intersection types don't appear to be implemented as full classes in Scala.

  

### Acknowledgements

  

I wish to thank three people who have helped make this library possible, with their patient explanations and help on gitter and in code reviews. Learning the Dotty reflection internals was a learning curve for me and these guys really helped me through it:

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

  

If we've done the proper inventory, with these 11 laws solved for Dotty we should have enough for ScalaJack to serialize/deserialize objects to wire formats. dotty_reflection should account for all 11 laws needed by ScalaJack, plus a few new tricks that are Dotty-specific (i.e. not used in ScalaJack--yet)

  
  

### Notes:

This library can handle some pretty tricky trait type resolution (see tests), but there are limits. Some attempts to RType.inTermsOf() a deeply nested trait may fail. These will be pretty knarly and (hopefully) unlikely cases though.

  

#### Release Notes:

0.1.0 -- Macro-enabled reflector

0.2.0 -- Added optional compiler plugin for huge performance lift and true runtime reflection for RType.inTermsOf (used for trait handling in ScalaJack).  Upgraded to Dotty 0.27.0-RC1 and JDK 13
