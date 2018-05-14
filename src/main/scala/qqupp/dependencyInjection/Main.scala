package qqupp.dependencyInjection

object MaybeSoftWord2020 extends App with Config {

  val wp: MaybeSoftWord2020.WordProcessor = wordProcessor

  wp.composeNewDocument
}

trait Config
    extends WordProcessorComponent
    with DefaultWordDictionaryComponent
    with DefaultPunctuationRulesComponent {

  val Definitions = Map(
    "Foo" -> "Just a foo?",
    "Bar" -> "Bar bar, BAR!",
    "Baz" -> "Bazinga."
  )

  val wordDictionary =
    DefaultWordDictionary("English", Definitions)
  val punctuationRules =
    DefaultPunctuationRules("English")
  val wordProcessor = new WordProcessor
}
