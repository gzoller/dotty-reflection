package co.blocke.dotty_reflection
package impl

import info._
// import extractors._
import scala.quoted._
import scala.reflect._
import scala.tasty.Reflection
import scala.util.Try


object TastyReflection extends NonCaseClassReflection:

  def reflectOnType(reflect: Reflection)(aType: reflect.Type): RType = 
    import reflect.{_, given _}

    val typeRef = aType.asInstanceOf[TypeRef]
    typeRef.classSymbol match {

      // Intersection types don't have a class symbol, so don't assume one!
      case None =>
        typeRef match {
          // Intersection Type
          //----------------------------------------
          case AndType(left,right) =>
            val resolvedLeft: RType = RType.unwindType(reflect)(left.asInstanceOf[reflect.TypeRef])
            val resolvedRight: RType = RType.unwindType(reflect)(right.asInstanceOf[reflect.TypeRef])
            IntersectionInfo(INTERSECTION_CLASS, resolvedLeft, resolvedRight)

          // Union Type
          //----------------------------------------
          case OrType(left,right) =>
            val resolvedLeft: RType = RType.unwindType(reflect)(left.asInstanceOf[reflect.TypeRef])
            val resolvedRight: RType = RType.unwindType(reflect)(right.asInstanceOf[reflect.TypeRef])
            UnionInfo(UNION_CLASS, resolvedLeft, resolvedRight)

          case u => 
            throw new ReflectException("Unsupported TypeRef: "+typeRef)
        }

      case Some(classSymbol) =>
        // Handle gobbled non-class scala.Enumeration.Value (old 2.x Enumeration class values)
        val (is2xEnumeration, className) = classSymbol.fullName match { 
          case raw if raw == ENUM_CLASSNAME => 
            val enumerationClass = typeRef.typeSymbol.fullName
            if( enumerationClass == ENUM_CLASSNAME ) then
              // If caller did NOT define a type member (type X = Value) inside their Enumeration class
              val enumClassName = typeRef.qualifier.asInstanceOf[reflect.TermRef].termSymbol.moduleClass.fullName.dropRight(1) // chop the '$' off the end!
              (true, enumClassName)
            else
              // If caller defined a type member (type X = Value) inside their Enumeration class
              (true, enumerationClass.dropRight(enumerationClass.length - enumerationClass.lastIndexOf('$')))
          case _  => (false, classSymbol.fullName)
        }

        typeRef match {
          case named: dotty.tools.dotc.core.Types.NamedType if classSymbol == Symbol.classSymbol("scala.Any") =>
            // Scala3 opaque type alias
            //----------------------------------------
            if typeRef.isOpaqueAlias then
              val translucentSuperType = typeRef.translucentSuperType
              AliasInfo(typeRef.show, RType.unwindType(reflect)(translucentSuperType))

            // Any Type
            //----------------------------------------
            else
              PrimitiveType.Scala_Any

          // Scala3 Tasty-equipped type incl. primitive types
          // Traits and classes w/type parameters are *not* here... they're AppliedTypes
          //----------------------------------------
          case named: dotty.tools.dotc.core.Types.NamedType => 
            val isTypeParam = typeRef.typeSymbol.flags.is(Flags.Param)   // Is 'T' or a "real" type?  (true if T)
            classSymbol match {
              case cs if isTypeParam => 
                TypeSymbolInfo(typeRef.name)  // TypeSymbols Foo[T] have typeRef of Any
              case cs if is2xEnumeration => 
                val enumerationClassSymbol = typeRef.qualifier.asInstanceOf[reflect.TermRef].termSymbol.moduleClass
                ScalaEnumerationInfo(enumerationClassSymbol.fullName.dropRight(1), enumerationClassSymbol.fields.map( _.name ))  // get the values of the Enumeration
              case cs => 
                // Primitive type test:
                PrimitiveType.values.find(_.name == className).getOrElse(
                  reflectOnClass(reflect)(typeRef)
                )
            }

          // Union Type
          //----------------------------------------
          case OrType(left,right) =>
            val resolvedLeft = RType.unwindType(reflect)(left.asInstanceOf[reflect.TypeRef])
            val resolvedRight = RType.unwindType(reflect)(right.asInstanceOf[reflect.TypeRef])
            UnionInfo(UNION_CLASS, resolvedLeft, resolvedRight)
        
          // Parameterized Types (classes, traits, & collections)
          //----------------------------------------
          case a @ AppliedType(t,tob) => 
            // First see if we have some sort of collection or other "wrapped" type
            val foundType: Option[RType] = extractors.ExtractorRegistry.extractors.collectFirst {
              case e if e.matches(reflect)(classSymbol) => e.extractInfo(reflect)(t, tob, classSymbol)   
            }
            foundType.getOrElse {
              // Nope--we've got a parameterized class or trait here
              reflectOnClass(reflect)(a.asInstanceOf[TypeRef])
            }
        
          case x => 
            // === No idea!  Unkonwn entity...
            UnknownInfo(className)
        }
    }


  def reflectOnClass(reflect: Reflection)(typeRef: reflect.TypeRef): RType = 
    import reflect.{_, given _}

    val className = typeRef.classSymbol.get.fullName

    object DefaultMethod {
      val reg = """\$lessinit\$greater\$default\$(\d+)""".r
      def unapply(s: reflect.Symbol): Option[Int] = reg.findFirstIn(s.toString) match {
        case Some(reg(a)) => Some(a.toInt)
        case _ => None
      }
    }

    val symbol = typeRef.classSymbol.get

    if symbol.flags.is(reflect.Flags.Scala2X) then
      symbol.fullName match {
        case PrimitiveType(t) => t
        case s => Scala2Info(s)
      }

    else if symbol.flags.is(reflect.Flags.Trait) then
      // === Trait ===
      //     >> Sealed Traits
      if symbol.flags.is(reflect.Flags.Sealed) then
        val kidsRTypes = symbol.children.map{ c => 
          c.tree match {
            case b: Bind => ObjectInfo(b.pattern.symbol.fullName)  // sealed object implementation
            case _ =>   // sealed case class implementation
              val typeDef: dotty.tools.dotc.ast.Trees.TypeDef[_] = c.tree.asInstanceOf[dotty.tools.dotc.ast.Trees.TypeDef[_]]
              RType.unwindType(reflect)(typeDef.typeOpt.asInstanceOf[reflect.Type])
          }
        }
        SealedTraitInfo(
          className, 
          kidsRTypes.toArray)
      else
        //  >> Normal (unsealed) traits
        typeRef match {
          case AppliedType(t,tob) =>  // parameterized trait
            TraitInfo(
              className, 
              tob.map(oneTob => RType.unwindType(reflect)(oneTob.asInstanceOf[reflect.TypeRef])).toArray)
          case _ =>  // non-parameterized trait
            TraitInfo(
              className, 
              Array.empty[RType])
        }

    else if symbol.flags.is(reflect.Flags.Enum) then // Found top-level enum (i.e. not part of a class), e.g. member of a collection
      val enumClassSymbol = typeRef.classSymbol.get
      enumClassSymbol.companionClass.methods // <-- This shouldn't "do" anything!  For some reason it is needed or Enums test explodes.
      val enumValues = enumClassSymbol.children.map(_.name)
      ScalaEnumInfo(symbol.fullName, enumValues)

    /*
    // === Java Class ===
    // User-written Java classes will have the source file.  Java library files will have <no file> for source
    else if symbol.pos.sourceFile.toString.endsWith(".java") || symbol.pos.sourceFile.toString == "<no file>" then
      // Reflecting Java classes requires the materialized Class, which may be available (e.g. Java collections) or not (e.g. user-written class).
      // See if we can get it...  If not create a proxy.
      scala.util.Try {
        JavaClassInspector.inspectJavaClass(Class.forName(symbol.fullName), paramMap)
      }.toOption.getOrElse(JavaClassInfo(symbol.fullName, paramMap))
    */

    // === Scala Classes ===
    else if symbol.isClassDef then
      // Get field annotatations (from body of class--they're not on the constructor fields)
      val classDef = symbol.tree.asInstanceOf[ClassDef]

      // Class annotations -> annotation map
      val annoSymbol = symbol.annots.filter( a => !a.symbol.signature.resultSig.startsWith("scala.annotation.internal."))
      val classAnnos = annoSymbol.map{ a => 
        val reflect.Apply(_, params) = a
        val annoName = a.symbol.signature.resultSig
        (annoName,(params collect {
          case NamedArg(argName, Literal(Constant(argValue))) => (argName.toString, argValue.toString)
        }).toMap)
      }.toMap

      val isValueClass = classDef.parents.collectFirst {
        case t:TypeTree if t.tpe.typeSymbol.name == "AnyVal" => t
      }.isDefined

      // Get superclass' field annotations--if any
      val dad = classDef.parents.headOption match {
        case Some(tt: TypeTree) if !isValueClass && tt.tpe.classSymbol.get.fullName != "java.lang.Object" => 
          reflectOnClass(reflect)(tt.tpe.asInstanceOf[TypeRef]) match {
            case ci: ClassInfo => Some(ci) // Any kind of class
            case _ => None // e.g. Unknown
          }
        case _ => None
      }

      // Get any case field default value accessor method names (map by field index)
      val fieldDefaultMethods = symbol.companionClass match {
        case dotty.tools.dotc.core.Symbols.NoSymbol => Map.empty[Int, (String,String)]
        case s: Symbol => symbol.companionClass.methods.collect {
          case DefaultMethod(defaultIndex) => defaultIndex-1 -> (className+"$", ("$lessinit$greater$default$"+defaultIndex))
        }.toMap
      }

      // All this mucking around in the constructor.... why not just get the case fields from the symbol?
      // Because:  symbol's case fields lose the annotations!  Pulling from contstructor ensures they are retained.
      val typeSymbols = symbol.primaryConstructor.paramSymss.head.map(_.name)
      val tob = typeRef match {
        case AppliedType(t,tob) => tob
        case _ => Nil
      }
      val typeMembers = classDef.body.collect {
        case TypeDef(typeName, typeTree) if typeSymbols.contains(typeTree.asInstanceOf[TypeTree].tpe.typeSymbol.name) =>
          val pos = typeSymbols.indexOf(typeTree.asInstanceOf[TypeTree].tpe.typeSymbol.name)
          TypeMemberInfo(
            typeName,
            typeSymbols(pos).asInstanceOf[TypeSymbol],
            RType.unwindType(reflect)(tob(pos).asInstanceOf[Type])
          )
      }

      if symbol.flags.is(reflect.Flags.Case) then
        // === Case Classes ===
        val caseFields = classDef.constructor.paramss.head.zipWithIndex.map{ (valDef, idx) => 
          val fieldType = scala.util.Try( RType.unwindType(reflect)(typeRef.memberType(symbol.caseFields(idx))) ).toOption.getOrElse(
            TypeSymbolInfo(valDef.asInstanceOf[ValDef].tpt.tpe.typeSymbol.name)
          )
          reflectOnField(reflect)(fieldType, valDef, idx, dad, fieldDefaultMethods) 
          }

        ScalaCaseClassInfo(
          className, 
          typeMembers.toArray, 
          caseFields.toArray, 
          classAnnos, 
          classDef.parents.map(_.symbol.fullName), 
          isValueClass)
      else
        // === Non-Case Classes ===
        
        // ensure all constructur fields are vals
        val constructorParams = classDef.constructor.paramss.head
        val caseFields = symbol.fields.filter( _.flags.is(Flags.ParamAccessor))
          .zipWithIndex
          .map{ (oneField, idx) => 
            if oneField.flags.is(Flags.PrivateLocal) then
              throw new ReflectException(s"Class [${symbol.fullName}]: Non-case class constructor arguments must all be 'val'")
            else
              val fieldType = RType.unwindType(reflect)(typeRef.memberType(oneField))
              reflectOnField(reflect)(fieldType, constructorParams(idx), idx, dad, fieldDefaultMethods)
          }

        inspectNonCaseClass(reflect)(
          symbol, 
          tob,
          classDef, 
          dad,
          className, 
          fieldDefaultMethods,
          typeMembers.toArray,
          caseFields.toArray, 
          classAnnos,
          classDef.parents.map(_.symbol.fullName),
          isValueClass)

    // === Other kinds of classes (non-case Scala) ===
    else
      UnknownInfo(symbol.fullName)


  def reflectOnField(reflect: Reflection)(
    fieldType: RType,
    valDef: reflect.ValDef, 
    index: Int, 
    dad: Option[ClassInfo],
    fieldDefaultMethods: Map[Int, (String,String)]
  ): FieldInfo = 
    import reflect.{_, given _}
    val fieldAnnos = {
      val baseAnnos = dad.flatMap( _.fields.find(_.name == valDef.name) ).map(_.annotations).getOrElse(Map.empty[String,Map[String,String]])
      baseAnnos ++ valDef.symbol.annots.map{ a => 
        val reflect.Apply(_, params) = a
        val annoName = a.symbol.signature.resultSig
        (annoName,(params collect {
          case NamedArg(argName, Literal(Constant(argValue))) => (argName.toString, argValue.toString)
        }).toMap)
      }.toMap
    }

    // Figure out the original type symbols, i.e. T, (if any)
    val valTypeRef = valDef.tpt.tpe.asInstanceOf[reflect.TypeRef]
    val isTypeParam = valTypeRef.typeSymbol.flags.is(reflect.Flags.Param)
    val originalTypeSymbol = if isTypeParam then Some(valTypeRef.name.asInstanceOf[TypeSymbol]) else None

    ScalaFieldInfo(index, valDef.name, fieldType, fieldAnnos, fieldDefaultMethods.get(index), originalTypeSymbol)