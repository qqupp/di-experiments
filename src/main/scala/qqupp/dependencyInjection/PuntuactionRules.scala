package qqupp.dependencyInjection

trait PunctuationRules {
  def dictionary: WordDictionary
  def checkHyphenation(word1: Word, word2: Word): Boolean
}

case class DefaultPunctuationRules(lang: Lang, dictionary: WordDictionary)
    extends PunctuationRules {

  override def checkHyphenation(word1: Word, word2: Word): Boolean =
    if (dictionary.intoSyllables(word1 + word2).nonEmpty) true else false
}
