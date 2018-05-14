package qqupp.dependencyInjection

import scala.collection.mutable.ListBuffer

class WordProcessor(dictionary: WordDictionary,
                    punctuationRules: PunctuationRules) {

  private val words = ListBuffer[Word]()

  def check =
    words.foldLeft(true)((acc, word) => acc && dictionary.hasWord(word))

  def highlightNonWords = words.filterNot(dictionary.hasWord(_))

  def breakLineCorrect(endLine: Word, newLine: Word) =
    punctuationRules.checkHyphenation(endLine, newLine)

  def composeNewDocument =
    throw new Exception("Version Compatibility problems!")

  def otherUsefulMethod: Unit = ???

}

object WordProcessor {
  def apply(dictionary: WordDictionary,
            punctuationRules: PunctuationRules): WordProcessor =
    new WordProcessor(dictionary, punctuationRules)
}
