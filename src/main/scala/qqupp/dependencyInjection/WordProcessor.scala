package qqupp.dependencyInjection

import scala.collection.mutable.ListBuffer

trait WordProcessorComponent {
  self: WordDictionaryComponent with PunctuationRulesComponent =>

  val wordProcessor: WordProcessor = new WordProcessor

  class WordProcessor {

    private val words = ListBuffer[Word]()

    def check =
      words.foldLeft(true)((acc, word) => acc && wordDictionary.hasWord(word))

    def highlightNonWords = words.filterNot(wordDictionary.hasWord(_))

    def breakLineCorrect(endLine: Word, newLine: Word) =
      punctuationRules.checkHyphenation(endLine, newLine)

    def composeNewDocument =
      throw new Exception("Version Compatibility problems!")

    def otherUsefulMethod: Unit = ???
  }
}
