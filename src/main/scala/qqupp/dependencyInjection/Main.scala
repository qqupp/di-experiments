package qqupp.dependencyInjection

object MaybeSoftWord2020 extends App {
  import Config._

  wordProcessor.composeNewDocument
}

object Config {
  val Definitions = Map(
    "Foo" -> "Just a foo?",
    "Bar" -> "Bar bar, BAR!",
    "Baz" -> "Bazinga."
  )
  val wordDictionary = DefaultWordDictionary("English", Definitions)
  val punctuationRules = DefaultPunctuationRules("English", wordDictionary)
  val wordProcessor = WordProcessor(wordDictionary, punctuationRules)
}
