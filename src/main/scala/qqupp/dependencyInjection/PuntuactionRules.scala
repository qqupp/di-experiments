package qqupp.dependencyInjection

trait PunctuationRulesComponent { self: WordDictionaryComponent =>

  val punctuationRules: PunctuationRules

  trait PunctuationRules {
    def checkHyphenation(word1: Word, word2: Word): Boolean
  }
}

trait DefaultPunctuationRulesComponent extends PunctuationRulesComponent {
  self: WordDictionaryComponent =>

  case class DefaultPunctuationRules(lang: Lang) extends PunctuationRules {

    override def checkHyphenation(word1: Word, word2: Word): Boolean =
      if (wordDictionary.intoSyllables(word1 + word2).nonEmpty) true else false
  }
}
