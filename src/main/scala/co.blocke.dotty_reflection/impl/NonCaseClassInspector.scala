package co.blocke.dotty_reflection
package impl

import info._
import scala.tasty.Reflection


trait NonCaseClassInspector:
  self: ScalaClassInspectorLike =>

  def inspectNonCaseClass(reflect: Reflection, paramMap: Map[TypeSymbol,RType])(
    classDef:              reflect.ClassDef,
    name:                  String,
    infoClass:             Class[_],
    orderedTypeParameters: List[TypeSymbol],
    typeMembers:           List[TypeMemberInfo],
    fields:                List[FieldInfo],
    annotations:           Map[String, Map[String,String]],
    isValueClass:          Boolean
  ): ScalaClassInfo = 
    import reflect.{_, given _}

    var index: Int = fields.length - 1

    val fieldNames = fields.map(_.name)

    val varAnnos = scala.collection.mutable.Map.empty[String,Map[String, Map[String,String]]]
    val varDefDeclarations = classDef.body.collect{
        // We just want public var definitions here
        case s: ValDef if !s.symbol.flags.is(reflect.Flags.Private) 
          && !s.symbol.flags.is(reflect.Flags.Protected) 
          && !fieldNames.contains(s.name) 
          && s.symbol.flags.is(reflect.Flags.Mutable) => 
            val annoSymbol = s.symbol.annots.filter( a => !a.symbol.signature.resultSig.startsWith("scala.annotation.internal."))
            val fieldAnnos = 
              annoSymbol.map{ a => 
                val reflect.Apply(_, params) = a
                val annoName = a.symbol.signature.resultSig
                (annoName,(params collect {
                  case NamedArg(argName, Literal(Constant(argValue))) => (argName.toString, argValue.toString)
                }).toMap)
              }.toMap
            varAnnos.put(s.name, fieldAnnos) // yes, this is a side-effect but it saves mutliple field scans!
            s.name -> s.tpt.tpe.asInstanceOf[reflect.TypeRef]

        // We just want public def definitions here
        // WARNING: These defs may also include non-field functions!  Filter later...
        case d: DefDef if !d.symbol.flags.is(reflect.Flags.Private) 
          && !d.symbol.flags.is(reflect.Flags.Protected) 
          && !d.name.endsWith("_=") => d.name -> d.returnTpt.tpe.asInstanceOf[reflect.TypeRef]
    }.toMap

    val numConstructorFields = fields.length

    // Include inherited methods (var & def), including inherited!
    val dad = Reflector.reflectOnClass(infoClass.getSuperclass)
    val baseAnnos = dad match {
      case c: ScalaClassInfo => c.nonConstructorFields.map( f => f.name -> f.annotations ).toMap
      case _ => Map.empty[String,Map[String, Map[String,String]]]
    }

    // Include inherited methods (var & def), including inherited!
    val getterSetter = infoClass.getMethods.filter(_.getName.endsWith("_$eq")).map( m => (infoClass.getMethod(m.getName.dropRight(4)), m) )
    val knownAnnos = baseAnnos ++ getterSetter.map{ (fGet, fSet) =>
      val both = fGet.getAnnotations.toList ++ fSet.getAnnotations.toList
      val annoMap: Map[String,Map[String,String]] = 
        both.map{ a => 
          val parms = a.annotationType.getDeclaredMethods.toList
          (a.annotationType.getName -> parms.map(p => (p.getName, p.invoke(a).toString)).toMap)
          }.toMap
      val allMap = 
        annoMap ++ varAnnos.getOrElse(fGet.getName, Map.empty[String,Map[String,String]]) match {
          case m if m.isEmpty => baseAnnos.getOrElse(fGet.getName, Map.empty[String,Map[String,String]])
          case m => m
        }
      (fGet.getName -> allMap)
    }.toMap

    val nonConstructorFields = getterSetter.map { (fGet, fSet) =>
      val fieldName = fGet.getName

      // Figure out the original type symbols, i.e. T, (if any)
      val originalTypeSymbol = 
        if paramMap.contains(fGet.getGenericReturnType.toString.asInstanceOf[TypeSymbol])
          Some(fGet.getGenericReturnType.toString.asInstanceOf[TypeSymbol])
        else
          None

      val rtype = 
        originalTypeSymbol.flatMap( ots => paramMap.get(ots) ).getOrElse{
          if varDefDeclarations.contains(fieldName) then
            inspectType(reflect, paramMap)(varDefDeclarations(fieldName))
          else
            Reflector.reflectOnClass(fGet.getReturnType)
        }
  
      index += 1

      ScalaFieldInfo(
        index,
        fieldName,
        rtype,
        knownAnnos(fieldName),
        fGet,
        {if index >= numConstructorFields then Some(()=>fGet) else None},
        originalTypeSymbol
      )
    }.toList

    ScalaClassInfo(
      name,
      infoClass,
      orderedTypeParameters,
      typeMembers,
      fields,
      nonConstructorFields,
      annotations,
      isValueClass
    )
