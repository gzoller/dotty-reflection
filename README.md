
# Dotty Reflection



[![license](https://img.shields.io/github/license/mashape/apistatus.svg?maxAge=86400)](https://opensource.org/licenses/MIT)

[![bintray](https://api.bintray.com/packages/blocke/releases/dotty-reflection/images/download.svg)](https://bintray.com/blocke/releases/dotty-reflection/_latestVersion)

Dotty is the developmental language destined to evolve into Scala 3. One of the big changes for Dotty/Scala 3 vs Scala 2 is that runtime reflection has been eliminated in favor of using compile-time macros to reflect on classes.  There is also a second mechanism for class reflection called Tasty Inspection, which reads Dotty .tasty files at runtime.  In theory, Inspection works like Scala 2 runtime reflection, but don't miss the bit about file reading!  File IO is *orders of magnitude* slower than Scala 2 runtime reflection, so Inspection is to be used as a mechanism of last resort.  

This project seeks to accomplish two goals:

* Make Dotty reflection a little more approachable by exposing a higher-level abstraction for reflected things (vs diving through Dotty classes that mirror compiler internals)

* Allow for a true runtime reflection capability

That second goal, runtime reflection, poses a unique challenge.  Just how do you provide a runtime reflection ability in a language that doesn't have that facility?  How, indeed!

The solution offered by this library is to provide a compiler plugin for your code.  What the plugin does is capture reflected information on all your compiled classes and serialize that information into annotations readable at runtime.  In principle, this isn't really very different than how Scala 2 runtime reflection works.  Use of the compiler plugin is optional, but highly recommended as it avoids the very costly file IO for runtime reflection.  The results speak for themselves.  Here's a sample of a few of the more complex test cases with/without the plugin:

|Test| No plugin  |With Plugin  |
|--|--|--|
|With nested case class and non-case class|2.464s|0.061s|
|Nested trait substitutions|0.345s|0.017s|
|With nested Option and List|0.282s|0.018s|
|With nested Try|0.246s|0.012s|
|With nested Map and Array|0.319s|0.018s|
|With nested case class and non-case class (inverted)|0.234s|0.005s|
|InTermsOf deep type substitution|0.239s|0.013s|

Everything will still work fine if you elect not to use the plugin, or encounter classes that weren't compiled using the plugin.  Performance will just be slower the first time a class is reflected upon. (The library caches, so subsequent reflection on an already-seen class is 1-2ms.)

## Configuration

In your build.sbt file be sure you've set co.blocke's releases repo in bintray as a resolver and add the current version of the library to libraryDependences:

```scala
resolvers += "co.blocke ivy resolver" at "https://dl.bintray.com/blocke/releases"
libraryDependencies += "co.blocke" %% "dotty-reflection" % CURRENT_VERSION
```
(CURRENT_VERSION value can be taken from the Download badge in this github repo.)

To use the optional compiler plugin (recommended) add this to your build.sbt:
```scala
addCompilerPlugin("co.blocke" %% "dotty-reflection" % CURRENT_VERSION)
```
For best results compile all classes you intend to reflect on with this plugin enabled.

## Standard Usage

This library defines a set of "Info" classes, which are high-level abstractions representing reflected information about various Scala classes and structures.  Reflect on a class like this:
```scala
import co.blocke.dotty_reflection

case class Thing(a: String)

// >> Compile-time reflection using square brackets for type
val macroRType: RType = RType.of[Thing]  // returns ScalaCaseClassInfo

// >> Run-time reflection using parentheses for type
val cname:String = getClassWeNeedFromSomewhere()
val runtimeRType: RType = RType.of(Class.forName(cname))
```
For the second example you won't know the actual type of the class to reflect on until runtime, for example if it 
comes from an external source like a REST call.  If you're using the compiler plugin, the pre-reflected 
ScalaCaseClassInfo will be returned, otherwise file IO will read your class' .tasty file and reflect on the class, 
which is very slow the first time we encounter this class.

## Resolving Classes with Parameters using Traits
This library was principally intended to facilitate migrating ScalaJack serialization into Dotty/Scala 3.  One of ScalaJack's key features is its trait handling ability. 
```scala
trait Pet[T] {
  val name: String
  val numLegs: Int
  val special: T
  }
case class Dog[T](name: String, numLegs: Int, special: T) extends Pet[T]
case class Fish[T](name: String, numLegs: Int, special: T) extends Pet[T]

val pet: Pet[Boolean] = Dog("Spot",4,true)
```
When serializing  pet, ScalaJack would generate JSON with a type hint like this:
```json
{"_hint":"com.mystuff.Dog","name":"Spot","numLegs":4,"special":true}
```
The hint tells ScalaJack which specific Pet class to materialize upon reading this JSON (we're expecting a Pet).  So... you'll see here we just have a class name in the hint.  How do we know the type of T?  We have to tell it:
```scala
scalajack.read[Pet[Boolean]](js)
```
Pet[Boolean] is a parameterized trait.  We get the value "com.mystuff.Dog" (a concrete class) from the JSON.  We need to resolve Dog ***in terms of*** Pet[Boolean] to find the correct type of 'special'.

We accomplish this feat in dotty-reflection like this:
```scala
val resolved = RType.inTermsOf[Pet[Boolean]](Class.forName("com.mystuff.Dog"))
```
This will return a ScalaCaseClassInfo with the 'special' field correctly typed to Boolean, which it learned about by studying the specific Pet trait you gave it in the square brackets.

Here's what the process looks like internally:
##### RType.of[Pet[Boolean]:
```scala
TraitInfo(com.foo.Pet) actualParamTypes: [
   T: scala.Boolean
] with fields:
   name: java.lang.String
   numLegs: scala.Int
   special[T]: scala.Boolean
   ```

##### RType.of(Class.forName("com.foo.Dog"):
```scala
ScalaCaseClassInfo(com.foo.Dog):
   fields:
      (0) name: java.lang.String
      (1) numLegs: scala.Int
      (2)[T] special: scala.Any
```
##### RType.inTermsOf[Pet[Boolean]](Class.forName("com.foo.Dog")
```scala
ScalaCaseClassInfo(com.foo.Dog):
   fields:
      (0) name: java.lang.String
      (1) numLegs: scala.Int
      (2)[T] special: scala.Boolean
```

## Learning to Drive with Macros

 dotty-reflection uses macros to the fullest extent possible to do the hard work of reflecting on types.  Macros impact the compile/test cycle in ways that are non-intuitive at first. Think of this example:
```scala
// File1.scala
case class Foo(name: String)

// File2.scala
val fooRType = RType.of[Foo]
```
In a non-macro implementation (e.g. Scala 2 runtime reflection) if you update Foo in File1.scala you naturally expect sbt to re-compile this file, and anything that depends on Foo, and the changes will be picked up in your program, and all will be well.  

That's **not** necessarily what happens with macros!  Remember the macro code is run at compile-time.  File2.scala needs to be re-compiled because the RType.of macro needs to be re-run to pick up your changes to Foo class in File1.scala.  *Unfortunately sbt doesn't pick up this dependency!*  If you don't know any better you'll just re-run your program after a change to File1.scala, like normal, and get a spectacular exception with exotic errors that won't mean much to you.  The solution is you need to also recompile File2.scala.

This means you will be doing more re-compiling with macro-based code than you would without the macros.  It's an unfortunate cost of inconvenience and time, but the payoff is a dramatic gain in speed at runtime, and in the case of reflection in Dotty/Scala 3, using macros is the only practical way to do it.


## Status

At this point the library can reflect on quite a lot of things in the Scala ecosystem:

* Scala 3 Tasty classes (parameterized or non-parameterized) w/annotations
* Traits (including sealed traits)
* Scala 2 case classes
* Value Classes
* Java classes (JavaBeans pattern)
* Scala 3 enum / Scala 2 Enumeration
* Scala 3 Union & Intersection types
* Opaque type aliases
* Try typed fields
* Either
* Option and Java Optional
* Collections, incl. several Java Collections
* Tuples

See unit tests for detailed examples of usage.


## Limitations
* No support for parameters in Intersection or Union types (```val t: X|Y``` or ```val u: X&Y```). This is because union/intersection types don't appear to be implemented as full classes in Scala and we haven't yet figured out this would work in dotty-reflection.
* This library can handle some pretty tricky trait type resolution (see tests), but there are limits. Some attempts to RType.inTermsOf() a contorted trait structure may fail. These will be pretty gnarly cases and (hopefully) unlikely.
* The entire serialized RType tree (incl. any nested RTypes) must not exceed 64K bytes.  This is so that it will fit into a Java Annotation.

## Acknowledgements
I wish to thank three people who have helped make this library possible, with their patient explanations and help on gitter. Learning the Dotty reflection internals was certainly a learning curve for me and these guys really helped me through it:
```
Guillaume Martres (@smarter)
Paolo G. Giarrusso (@Blaisorblade)
Nicolas Stucki (@nicolasstucki)
```

## Release Notes:
0.1.0 -- Macro-enabled reflector
0.2.0 -- Added compiler plugin for performance lift and true runtime reflection for RType.inTermsOf.  Upgraded to Dotty 0.27.0-RC1 and JDK 13
0.2.1 -- bug-fix
1.0.0 -- Initial full-feature release
