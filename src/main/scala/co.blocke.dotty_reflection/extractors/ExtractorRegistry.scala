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
      JavaQueueExtractor(),
      JavaStackExtractor(),
      JavaSetExtractor(),
      JavaListExtractor(),
      OptionalExtractor(),
      JavaMapExtractor()
    )
