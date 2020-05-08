package co.blocke.dotty_reflection
package extractors

object ExtractorRegistry:

  lazy val extractors: List[impl.TypeInfoExtractor[_]] = 
    List(
      OptionExtractor(),
      EitherExtractor(), 
      SeqExtractor(), 
      ScalaArrayExtractor(),
      MapExtractor(), 
      TupleExtractor(),
      TryExtractor(),
      JavaMapExtractor(),
      JavaStackExtractor(),  // This must come before Java List, because it is a list--but is created differently
      JavaListExtractor(),
      JavaSetExtractor(),
      JavaQueueExtractor(),
      OptionalExtractor()
    )