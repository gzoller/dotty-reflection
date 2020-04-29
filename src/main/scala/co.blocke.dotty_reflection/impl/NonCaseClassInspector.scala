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

    // classDef.body.foldLeft( (List.empty[], List.emtpy[]) ){}
    val fieldNames = fields.map(_.name)

    val found = classDef.body.collect{
        // We just want public var definitions here
        case s: ValDef if !s.symbol.flags.is(reflect.Flags.Private) 
          && !s.symbol.flags.is(reflect.Flags.Protected) 
          && !fieldNames.contains(s.name) 
          && s.symbol.flags.is(reflect.Flags.Mutable) => s.name -> s
        // Public defs only please
        case d: DefDef if !d.symbol.flags.is(reflect.Flags.Private) 
          && !d.symbol.flags.is(reflect.Flags.Protected) => d.name -> d
      }.toMap
    val otherFieldNames = found.keySet.filterNot(_.endsWith("_="))

    // Read any annotations on all the ValDefs or DefDefs--could be any of these, so produce a map of name -> Map[String,Map[String,String]]
    val otherAnnotations = found.map{ (name,defy) => 
      val annoSymbol = defy.symbol.annots.filter( a => !a.symbol.signature.resultSig.startsWith("scala.annotation.internal."))
      val fieldAnnos = annoSymbol.map{ a => 
        val reflect.Apply(_, params) = a
        val annoName = a.symbol.signature.resultSig
        (annoName,(params collect {
          case NamedArg(argName, Literal(Constant(argValue))) => (argName.toString, argValue.toString)
        }).toMap)
      }.toMap
      name -> fieldAnnos
      }
      
    val nonConstructorFields = otherFieldNames.map(found(_)).collect { 
      case vd: ValDef if !otherAnnotations(vd.name).contains("co.blocke.dotty_reflection.Ignore") => 

        // Figure out the original type symbols, i.e. T, (if any)
        val valTypeRef = vd.tpt.tpe.asInstanceOf[reflect.TypeRef]
        val isTypeParam = valTypeRef.typeSymbol.flags.is(Flags.Param)
        val originalTypeSymbol = if isTypeParam then Some(valTypeRef.name.asInstanceOf[TypeSymbol]) else None
  
        ScalaFieldInfo(
          -1,
          vd.name,
          inspectType(reflect, paramMap)(vd.tpt.tpe.asInstanceOf[reflect.TypeRef]),
          otherAnnotations(vd.name),
          infoClass.getMethod(vd.name),
          None,
          originalTypeSymbol
        )

      case dd: DefDef if !otherAnnotations(dd.name).contains("co.blocke.dotty_reflection.Ignore") 
        && !otherAnnotations(dd.name+"_=").contains("co.blocke.dotty_reflection.Ignore") => 

        // Figure out the original type symbols, i.e. T, (if any)
          val valTypeRef = dd.returnTpt.tpe.asInstanceOf[reflect.TypeRef]
          val isTypeParam = valTypeRef.typeSymbol.flags.is(Flags.Param)
          val originalTypeSymbol = if isTypeParam then Some(valTypeRef.name.asInstanceOf[TypeSymbol]) else None
    
          ScalaFieldInfo(
            -1,
            dd.name,
            inspectType(reflect, paramMap)(dd.returnTpt.tpe.asInstanceOf[reflect.TypeRef]),
            otherAnnotations(dd.name) ++ otherAnnotations(dd.name+"_="),
            infoClass.getMethod(dd.name),
            None,
            originalTypeSymbol
          )
      }

    ScalaClassInfo(
      name,
      infoClass,
      orderedTypeParameters,
      typeMembers,
      fields,
      nonConstructorFields.toList,
      annotations,
      isValueClass
    )
