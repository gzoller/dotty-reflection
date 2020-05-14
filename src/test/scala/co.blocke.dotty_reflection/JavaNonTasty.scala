package co.blocke.dotty_reflection

import munit._
import info._
import PrimitiveType._

class JavaNonTasty extends munit.FunSuite:

  test("reflect basic with capture") {
    val result = Reflector.reflectOn[co.blocke.reflect.Person]
    assertEquals( result.show(), """JavaClassInfo(co.blocke.reflect.Person):
    |   fields:
    |      (0) age: scala.Int
    |      (1) name: java.lang.String
    |      (2) other: scala.Int""".stripMargin)
    assert(result.asInstanceOf[JavaClassInfo].hasMixin("co.blocke.dotty_reflection.SJCaptureJava"))
  }

  test("create Java object") {
    val p = Reflector.reflectOn[co.blocke.reflect.Person].asInstanceOf[JavaClassInfo]
    val person = p.constructWith[co.blocke.reflect.Person](List(35, "Frank", 5))
    assertEquals(person.getName,"Frank")
    assertEquals(person.getAge,35)
    assertEquals(person.getOther,5)
    assertEquals(p.fields(1).valueAccessor.invoke(person).toString,"Frank")
    assertEquals(p.fields(0).valueAccessor.invoke(person).asInstanceOf[Int],35)
    assertEquals(p.fields(2).valueAccessor.invoke(person).asInstanceOf[Int],5)
  }

  test("Verify Java primitives") {
    val jx = Reflector.reflectOn[co.blocke.reflect.JavaTypes].asInstanceOf[JavaClassInfo]
    val number: java.lang.Number = java.lang.Integer.valueOf(123).asInstanceOf[java.lang.Number]
    val inst = jx.constructWith[co.blocke.reflect.JavaTypes](List(
      true, false, 5.toByte, 3.toByte, 'x', 'y', 1.2D, 2.3D, 4.5F, 5.6F, 1, 2, 3L, 4L, number, "something", 5.toShort, 6.toShort, "foom"
    ))

    val _a = jx.field("jBoolean").get
    _a.valueSetter.invoke(inst, java.lang.Boolean.valueOf(false))
    val a = _a.valueAccessor.invoke(inst)

    val _b = jx.field("jBoolean2").get 
    _b.valueSetter.invoke(inst, java.lang.Boolean.valueOf(true))
    val b = _b.valueAccessor.invoke(inst)

    val _c = jx.field("jByte").get 
    _c.valueSetter.invoke(inst, java.lang.Byte.valueOf(3.toByte))
    val c = _c.valueAccessor.invoke(inst)

    val _d = jx.field("jByte2").get 
    _d.valueSetter.invoke(inst, java.lang.Byte.valueOf(5.toByte))
    val d = _d.valueAccessor.invoke(inst)

    val _e = jx.field("jChar").get 
    _e.valueSetter.invoke(inst, java.lang.Character.valueOf('y'))
    val e = _e.valueAccessor.invoke(inst)

    val _f = jx.field("jCharacter").get 
    _f.valueSetter.invoke(inst, java.lang.Character.valueOf('z'))
    val f = _f.valueAccessor.invoke(inst)

    val _g = jx.field("jDouble").get 
    _g.valueSetter.invoke(inst, java.lang.Double.valueOf(2.3D))
    val g = _g.valueAccessor.invoke(inst)

    val _h = jx.field("jDouble2").get 
    _h.valueSetter.invoke(inst, java.lang.Double.valueOf(1.2D))
    val h = _h.valueAccessor.invoke(inst)

    val _i = jx.field("jFloat").get 
    _i.valueSetter.invoke(inst, java.lang.Float.valueOf(5.6F))
    val i = _i.valueAccessor.invoke(inst)

    val _j = jx.field("jFloat2").get 
    _j.valueSetter.invoke(inst, java.lang.Float.valueOf(4.5F))
    val j = _j.valueAccessor.invoke(inst)

    val _k = jx.field("jInt").get 
    _k.valueSetter.invoke(inst, java.lang.Integer.valueOf(2))
    val k = _k.valueAccessor.invoke(inst)

    val _l = jx.field("jInteger").get 
    _l.valueSetter.invoke(inst, java.lang.Integer.valueOf(1))
    val l = _l.valueAccessor.invoke(inst)

    val _m = jx.field("jLong").get 
    _m.valueSetter.invoke(inst, java.lang.Long.valueOf(4L))
    val m = _m.valueAccessor.invoke(inst)

    val _n = jx.field("jLong2").get 
    _n.valueSetter.invoke(inst, java.lang.Long.valueOf(3L))
    val n = _n.valueAccessor.invoke(inst)

    val _o = jx.field("jShort").get 
    _o.valueSetter.invoke(inst, java.lang.Short.valueOf(6.toShort))
    val o = _o.valueAccessor.invoke(inst)

    val _p = jx.field("jShort2").get 
    _p.valueSetter.invoke(inst, java.lang.Short.valueOf(5.toShort))
    val p = _p.valueAccessor.invoke(inst)

    val _q = jx.field("jString").get 
    _q.valueSetter.invoke(inst, "blather")
    val q = _q.valueAccessor.invoke(inst)

    val _r = jx.field("jObj").get 
    _r.valueSetter.invoke(inst, "empty")
    val r = _r.valueAccessor.invoke(inst)

    val _s = jx.field("jNumber").get 
    _s.valueSetter.invoke(inst, java.lang.Integer.valueOf(456).asInstanceOf[java.lang.Number])
    val s = _s.valueAccessor.invoke(inst)

    assert( a.asInstanceOf[java.lang.Boolean].booleanValue == false && a.getClass.getName == "java.lang.Boolean" )
    assert( b.asInstanceOf[java.lang.Boolean].booleanValue == true && b.getClass.getName == "java.lang.Boolean" )
    assert( c.asInstanceOf[java.lang.Byte].byteValue == 3.toByte && c.getClass.getName == "java.lang.Byte" )
    assert( d.asInstanceOf[java.lang.Byte].byteValue == 5.toByte && d.getClass.getName == "java.lang.Byte" )
    assert( e.asInstanceOf[java.lang.Character].charValue == 'y' && e.getClass.getName == "java.lang.Character" )
    assert( f.asInstanceOf[java.lang.Character].charValue == 'z' && f.getClass.getName == "java.lang.Character" )
    assert( g.asInstanceOf[java.lang.Double].doubleValue == 2.3D && g.getClass.getName == "java.lang.Double" )
    assert( h.asInstanceOf[java.lang.Double].doubleValue == 1.2D && h.getClass.getName == "java.lang.Double" )
    assert( i.asInstanceOf[java.lang.Float].floatValue == 5.6F && i.getClass.getName == "java.lang.Float" )
    assert( j.asInstanceOf[java.lang.Float].floatValue == 4.5F && j.getClass.getName == "java.lang.Float" )
    assert( k.asInstanceOf[java.lang.Integer].doubleValue == 2 && k.getClass.getName == "java.lang.Integer" )
    assert( l.asInstanceOf[java.lang.Integer].doubleValue == 1 && l.getClass.getName == "java.lang.Integer" )
    assert( m.asInstanceOf[java.lang.Long].floatValue == 4L && m.getClass.getName == "java.lang.Long" )
    assert( n.asInstanceOf[java.lang.Long].floatValue == 3L && n.getClass.getName == "java.lang.Long" )
    assert( o.asInstanceOf[java.lang.Short].doubleValue == 6.toShort && o.getClass.getName == "java.lang.Short" )
    assert( p.asInstanceOf[java.lang.Short].floatValue == 5.toShort && p.getClass.getName == "java.lang.Short" )
    assert( q.asInstanceOf[java.lang.String] == "blather" && q.getClass.getName == "java.lang.String" )
    assert( r.asInstanceOf[java.lang.Object] == "empty" && r.getClass.getName == "java.lang.String" )
    assert( s.asInstanceOf[java.lang.Number].intValue == 456 && s.getClass.getName == "java.lang.Integer" )
  }

  test("Detect parameterized Java class") {
    val wp = Class.forName("co.blocke.reflect.ParamAnno")
    val result = Reflector.reflectOnClass(wp) 
    assertEquals( result.show(), """JavaClassInfo(co.blocke.reflect.ParamAnno[T]):
    |   fields:
    |      (0) age: T
    |         annotations: Map(co.blocke.reflect.FieldAnno -> Map(idx -> 2))
    |      (1) bogus: java.lang.Boolean
    |         annotations: Map(co.blocke.reflect.Ignore -> Map())
    |      (2) name: java.lang.String
    |         annotations: Map(co.blocke.reflect.FieldAnno -> Map(idx -> 1))
    |   annotations: Map(co.blocke.reflect.ClassAnno -> Map(name -> Foom))""".stripMargin)
  }

  test("Java collection types") {
    val result = Reflector.reflectOn[co.blocke.reflect.JavaCollections]
    assertEquals( result.show(), """JavaClassInfo(co.blocke.reflect.JavaCollections):
    |   fields:
    |      (0) hMap: JavaMapInfo(java.util.HashMap[K,V]):
    |         java.lang.String
    |         java.lang.Integer
    |      (1) myArr: array of java.lang.String
    |      (2) myList: JavaListInfo(java.util.ArrayList[E]): java.lang.String
    |      (3) myQ: JavaQueueInfo(java.util.concurrent.BlockingQueue[E]): java.lang.String
    |      (4) myTree: JavaSetInfo(java.util.TreeSet[E]): java.lang.String
    |      (5) nested: array of JavaListInfo(java.util.List[E]): java.lang.Integer""".stripMargin)
  }

  test("Nested Java classes") {
    val result = Reflector.reflectOn[co.blocke.reflect.You]
    assertEquals( result.show(), """JavaClassInfo(co.blocke.reflect.You):
    |   fields:
    |      (0) sayHey: JavaClassInfo(co.blocke.reflect.Hey):
    |         fields:
    |            (0) jString: java.lang.String""".stripMargin)
  }

  test("Java parameterized class top level") {
    val result = Reflector.reflectOn[co.blocke.reflect.JavaParam[Integer]]
    assertEquals( result.show(), """JavaClassInfo(co.blocke.reflect.JavaParam[K]):
    |   fields:
    |      (0) jThing: java.lang.Integer""".stripMargin)
  }

  test("Java parameterized class field member") {
    val result = Reflector.reflectOn[co.blocke.reflect.JavaParamHolder]
    assertEquals( result.show(), """JavaClassInfo(co.blocke.reflect.JavaParamHolder):
    |   fields:
    |      (0) jFoo: JavaClassInfo(co.blocke.reflect.JavaParam[K]):
    |         fields:
    |            (0) jThing: java.lang.Integer""".stripMargin)
  }
