package qqupp.dependencyInjection

trait PunctuationRulesComponent { self: WordDictionaryComponent =>

  type PunctuationRules <: PunctuationRulesInterface

  def punctuationRules: PunctuationRules

  trait PunctuationRulesInterface {
    def checkHyphenation(word1: Word, word2: Word): Boolean
  }
}

trait DefaultPunctuationRulesComponent extends PunctuationRulesComponent {
  self: WordDictionaryComponent =>

  type PunctuationRules = DefaultPunctuationRules

  case class DefaultPunctuationRules(lang: Lang) extends PunctuationRulesInterface {

    def checkHyphenation(word1: Word, word2: Word): Boolean =
      if (wordDictionary.intoSyllables(word1 + word2).nonEmpty) true else false
  }
}
