package qqupp.dependencyInjection

trait WordDictionaryComponent { self =>

  type WordDictionary <: WordDictionaryInterface

  def wordDictionary: WordDictionary

  trait WordDictionaryInterface {
    def lang: Lang
    def hasWord(word: Word): Boolean
    def definition(word: Word): Option[Definition]
    def intoSyllables(word: Word): List[Syllable]
  }
}

trait DefaultWordDictionaryComponent extends WordDictionaryComponent { self =>

  type WordDictionary = DefaultWordDictionary

  case class DefaultWordDictionary(lang: Lang,
                                   private val definitions: Map[Word, Definition])
      extends WordDictionaryInterface {

    def hasWord(word: Word): Boolean = definitions.contains(word)

    def definition(word: Word): Option[Definition] = definitions.get(word)

    def intoSyllables(word: Word): List[Syllable] =
      word.foldRight(List[Syllable]())((c, l) => (c.toString) :: l)
  }
}
