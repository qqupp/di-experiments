package qqupp.dependencyInjection

trait PunctuationRules {
  def lang: Lang
  def dictionary: WordDictionary
  def checkHyphenation(word1: Word, word2: Word): Boolean
}

class DefaultPunctuationRules(val lang: Lang, val dictionary: WordDictionary)
    extends PunctuationRules {

  override def checkHyphenation(word1: Word, word2: Word): Boolean =
    if (dictionary.intoSyllables(word1 + word2).nonEmpty) true else false
}

object DefaultPunctuationRules {
  def apply(lang: Lang, dictionary: WordDictionary): DefaultPunctuationRules =
    new DefaultPunctuationRules(lang, dictionary)
}
