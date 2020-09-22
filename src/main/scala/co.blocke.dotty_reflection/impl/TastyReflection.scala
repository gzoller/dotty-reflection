package co.blocke.dotty_reflection
package impl

import info._
// import extractors._
import scala.quoted._
import scala.reflect._
import scala.tasty.Reflection
import scala.util.Try


object TastyReflection extends NonCaseClassReflection:

  def reflectOnType(reflect: Reflection)(aType: reflect.Type, fullName: String, resolveTypeSyms: Boolean): RType = 
    import reflect.{_, given _}

    scala.util.Try {
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
                ScalaEnumerationInfo(enumerationClassSymbol.fullName.dropRight(1), enumerationClassSymbol.fields.map( _.name ).toArray)  // get the values of the Enumeration
              case cs => 
                // Primitive type test:
                PrimitiveType.values.find(_.name == className).getOrElse{
                  reflectOnClass(reflect)(typeRef, RType.typeName(reflect)(typeRef), resolveTypeSyms)
                }
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
              case e if e.matches(reflect)(classSymbol) => 
                e.extractInfo(reflect)(t, tob, classSymbol)
            }
            foundType.getOrElse {
              // Nope--we've got a parameterized class or trait here
              reflectOnClass(reflect)(a.asInstanceOf[TypeRef], RType.typeName(reflect)(a), resolveTypeSyms, tob)
            }
        
          case x => 
            // === No idea!  Unkonwn entity...
            UnknownInfo(className)
        }
      }
    }.toOption.getOrElse{
      TypeSymbolInfo(aType.typeSymbol.name)
    }


  def reflectOnClass(reflect: Reflection)(typeRef: reflect.TypeRef, fullName: String, resolveTypeSyms: Boolean, appliedTob: List[reflect.TypeOrBounds] =  Nil): RType = 
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
    val typeSymbols = symbol.primaryConstructor.paramSymss.head.map(_.name.asInstanceOf[TypeSymbol])

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
          case a @ AppliedType(t,tob) =>  // parameterized trait
            val actualParamTypes = tob.map{ oneTob => 
              scala.util.Try{ 
                if resolveTypeSyms then
                  RType.unwindType(reflect)(oneTob.asInstanceOf[reflect.TypeRef])
                else if oneTob.asInstanceOf[reflect.TypeRef].typeSymbol.flags.is(Flags.Param)
                  TypeSymbolInfo(oneTob.asInstanceOf[reflect.TypeRef].name)
                else
                  RType.unwindType(reflect)(oneTob.asInstanceOf[reflect.TypeRef], false)
              }.toOption.getOrElse{
                TypeSymbolInfo(oneTob.asInstanceOf[reflect.TypeRef].name)
              }   
            }
            val paramMap: Map[TypeSymbol, RType] = typeSymbols.zip(actualParamTypes).toMap

            val traitFields = symbol.fields.zipWithIndex.map { (f,index) =>
              val fieldType = 
                // A lot of complex messiness to sew down the mapped type:  Foo[T]( val x: List[T]) where we call Foo[W] from a higher class,
                // so T -> W.  We need resolveTypeParams to sew down for deeper nested types like List.  Ugh.
                scala.util.Try{ 
                  if resolveTypeSyms then
                    RType.unwindType(reflect)(typeRef.memberType(f))
                  else 
                    f.tree match {
                      case vd: ValDef if vd.tpt.tpe.typeSymbol.flags.is(Flags.Param) =>
                        paramMap.getOrElse(
                          vd.tpt.tpe.typeSymbol.name.asInstanceOf[TypeSymbol],
                          RType.unwindType(reflect)(vd.tpt.tpe).resolveTypeParams(paramMap)
                        )
                      case _ => 
                        RType.unwindType(reflect)(typeRef.memberType(f), false)
                    }
                }.toOption.getOrElse{
                  paramMap.getOrElse(
                    f.tree.asInstanceOf[ValDef].tpt.tpe.typeSymbol.name.asInstanceOf[TypeSymbol],
                    RType.unwindType(reflect)(f.tree.asInstanceOf[ValDef].tpt.tpe).resolveTypeParams(paramMap)
                  )
                }
              val typeSym = 
                f.tree.asInstanceOf[ValDef].tpt.tpe.typeSymbol.name.asInstanceOf[TypeSymbol] match {
                  case ts if typeSymbols.contains(ts) => Some(ts)
                  case _ => None
                }
              ScalaFieldInfo(index, f.name, fieldType, Map.empty[String,Map[String,String]], None, typeSym, true)
            }
            TraitInfo(
              className,
              traitFields.toArray, 
              actualParamTypes.toArray,
              typeSymbols.toArray
            )
          case _ => 
            // non-parameterized trait
            val traitFields = symbol.tree.asInstanceOf[ClassDef].body.collect {
              case valDef: ValDef =>
                val fieldType = RType.unwindType(reflect)(typeRef.memberType(valDef.symbol))
                ScalaFieldInfo(-1, valDef.name, fieldType, Map.empty[String,Map[String,String]], None, None, true)
            }
            TraitInfo(className, traitFields.toArray)
        }

    else if symbol.flags.is(reflect.Flags.Enum) then // Found top-level enum (i.e. not part of a class), e.g. member of a collection
      val enumClassSymbol = typeRef.classSymbol.get
      enumClassSymbol.companionClass.methods // <-- This shouldn't "do" anything!  For some reason it is needed or Enums test explodes.
      val enumValues = enumClassSymbol.children.map(_.name)
      ScalaEnumInfo(symbol.fullName, enumValues.toArray)

    // === Java Class ===
    // User-written Java classes will have the source file.  Java library files will have <no file> for source
    else if symbol.pos.sourceFile.toString.endsWith(".java") || symbol.pos.sourceFile.toString == "<no file>" then
      // Reflecting Java classes requires the materialized Class, which may be available (e.g. Java collections) or not (e.g. user-written class).
      // So for now just burp forth a proxy and we'll resovle the details at runtime.
      JavaClassInfo(symbol.fullName, symbol.fullName, appliedTob.map( at => RType.unwindType(reflect)(at.asInstanceOf[TypeRef])).toArray )

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

      val isValueClass = typeRef.baseClasses.contains(Symbol.classSymbol("scala.AnyVal"))

      // Need:  Map[ TraitClass -> Map[TypeSymbol -> Path] ]
      println("Class "+className)
      implicit val ctx = reflect.rootContext.asInstanceOf[dotty.tools.dotc.core.Contexts.Context]
      val typeSymbolTypes: List[reflect.Type] = symbol.primaryConstructor.paramSymss.head.map(_.asInstanceOf[dotty.tools.dotc.core.Symbols.Symbol].denot.infoOrCompleter.asInstanceOf[reflect.Type])
      println("Syms: "+typeSymbolTypes)
      println("Parents: "+classDef.parents.collect{
        case t:reflect.TypeTree => t.tpe
      }.collect{
        case traitApplied: AppliedType if traitApplied.typeSymbol.flags.is(reflect.Flags.Trait) => 
          val found = reflectOnSymbols(reflect)( typeSymbols.toSet, RType.unwindType(reflect)(traitApplied.asInstanceOf[Type], false), Path.buildPath)
          println(">>> "+found)
          val ddad = traitApplied.classSymbol.get.tree.asInstanceOf[ClassDef].parents.collect{
            case t:reflect.TypeTree => t.tpe
          }.collect{
            case AppliedType(tz,_) => tz.appliedTo(typeSymbolTypes)
          }
          println("DDAD: "+ddad)
        })

      // Get superclass' field annotations--if any
      val dad = classDef.parents.headOption match {
        case Some(tt: TypeTree) if !isValueClass && tt.tpe.classSymbol.get.fullName != "java.lang.Object" => 
          reflectOnClass(reflect)(tt.tpe.asInstanceOf[TypeRef], RType.typeName(reflect)(tt.tpe), resolveTypeSyms) match {
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
      val tob = typeRef match {
        case AppliedType(t,tob) => tob
        case _                  => Nil
      }
      val typeMembers = classDef.body.collect {
        case TypeDef(typeName, typeTree) if typeSymbols.contains(typeTree.asInstanceOf[TypeTree].tpe.typeSymbol.name) =>
          val pos = typeSymbols.indexOf(typeTree.asInstanceOf[TypeTree].tpe.typeSymbol.name)
          TypeMemberInfo(
            typeName,
            typeSymbols(pos),
            RType.unwindType(reflect)(tob(pos).asInstanceOf[Type])
          )
      }

      val actualParamTypes = tob.map{ oneTob => 
        scala.util.Try{ 
          if resolveTypeSyms then
            RType.unwindType(reflect)(oneTob.asInstanceOf[reflect.TypeRef])
          else if oneTob.asInstanceOf[reflect.TypeRef].typeSymbol.flags.is(Flags.Param)
            TypeSymbolInfo(oneTob.asInstanceOf[reflect.TypeRef].name)
          else
            RType.unwindType(reflect)(oneTob.asInstanceOf[reflect.TypeRef], false)
        }.toOption.getOrElse{
          TypeSymbolInfo(oneTob.asInstanceOf[reflect.TypeRef].name)
        }   
      }
      val paramMap: Map[TypeSymbol, RType] = typeSymbols.zip(actualParamTypes).toMap

      if symbol.flags.is(reflect.Flags.Case) then

        // === Case Classes ===
        val caseFields = classDef.constructor.paramss.head.zipWithIndex.map{ (valDef, idx) => 
          val fieldType = scala.util.Try{ 
            if resolveTypeSyms then
              RType.unwindType(reflect)(typeRef.memberType(symbol.caseFields(idx))) 
            else if valDef.asInstanceOf[ValDef].tpt.tpe.typeSymbol.flags.is(Flags.Param)
              TypeSymbolInfo(valDef.asInstanceOf[ValDef].tpt.tpe.typeSymbol.name)
            else
              RType.unwindType(reflect)(valDef.tpt.tpe, false)
          }.toOption.getOrElse{
            TypeSymbolInfo(valDef.asInstanceOf[ValDef].tpt.tpe.typeSymbol.name)
          }
          reflectOnField(reflect)(fieldType, valDef, idx, dad, fieldDefaultMethods).resolveTypeParams( paramMap )
        }       

        ScalaCaseClassInfo(
          className,
          fullName,
          // {if typeSymbols.nonEmpty then className + actualParamTypes.map(t => t.name).mkString("[",",","]") else fullName},
          typeSymbols.toArray,
          typeMembers.toArray, 
          caseFields.toArray, 
          classAnnos, 
          classDef.parents.map(_.symbol.fullName).toArray, 
          typeSymbols.nonEmpty,
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
          typeSymbols.toArray,
          classDef, 
          dad,
          className,
          fullName,
          typeSymbols.nonEmpty,
          fieldDefaultMethods,
          typeMembers.toArray,
          caseFields.toArray, 
          classAnnos,
          classDef.parents.map(_.symbol.fullName).toArray,
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


  // Dive into the given tree (typically starting with a trait) and find the given symbols (taken from a parameterized class)

  // PROBLEM:  A trait w/no fields must somehow inherit its base traits' fields in order for this logic to work, 
  // otherwise the marker trait's fields list will be empty!
  def reflectOnSymbols(reflect: Reflection)( syms: Set[TypeSymbol], tree: RType, pathSoFar: Path ): Map[TypeSymbol, Path] =
    // Examine anything that can be an AppliedType (parameterizable)
    tree match {
      case s: TraitInfo      =>
        s.fields.foldLeft( (syms, Map.empty[TypeSymbol,Path]) ){ case ((syms2,symPathMap), oneField) => 
          oneField.fieldType match {
            case ts: TypeSymbolInfo => 
              val symName = ts.name.asInstanceOf[TypeSymbol]
              if syms2.contains(symName) then
                // Found a symbol we're looking for... remove from list and add entry in path map
                (syms2 - symName, symPathMap + (symName -> pathSoFar.add(Path.TRAIT_PATH, oneField.index.toByte).lock))
              else
                // Not a symbol we're looking for... ignore it... no change to foldLeft args
                (syms2, symPathMap)
            case other              =>
              val symsFoundMap = reflectOnSymbols(reflect)(syms2, oneField.fieldType, pathSoFar.fork.add(Path.TRAIT_PATH, oneField.index.toByte))
              (syms2 -- symsFoundMap.keySet, symsFoundMap)
            }
          }._2 // return just the paths to the found symbols
      case s: ClassInfo       =>
        ???
      case s: CollectionRType =>
        ???
      case s: LeftRightRType  =>
        ???
      case s: OptionInfo      =>
        ???
      case s: TryInfo         =>
        ???
      case s: TupleInfo       =>
        ???
    }
