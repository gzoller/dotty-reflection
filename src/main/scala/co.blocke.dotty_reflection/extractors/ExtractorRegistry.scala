package co.blocke.dotty_reflection
package extractors

object ExtractorRegistry:

  lazy val extractors: List[impl.TypeInfoExtractor] = 
    List(
      OptionExtractor(), 
      EitherExtractor(), 
      SeqExtractor(), 
      MapExtractor(), 
      TupleExtractor(),
      CatchAllExtractor()
    )