package qqupp.dependencyInjection

object MaybeSoftWord2020 extends App {
  import Config._

  wordProcessor.composeNewDocument
}

object Config {
  val NonsenseDictionary = Map(
    "Foo" -> "Just a foo?",
    "Bar" -> "Bar bar, BAR!",
    "Baz" -> "Bazinga."
  )
  val wordDictionary = DefaultWordDictionary("English", NonsenseDictionary)
  val punctuationRules = DefaultPunctuationRules("English", wordDictionary)
  val wordProcessor = WordProcessor(wordDictionary, punctuationRules)
}
