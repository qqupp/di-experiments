package qqupp.dependencyInjection

trait WordDictionaryComponent { self =>

  val wordDictionary: WordDictionary

  trait WordDictionary {
    def lang: Lang
    def hasWord(word: Word): Boolean
    def definition(word: Word): Option[Definition]
    def intoSyllables(word: Word): List[Syllable]
  }
}

trait DefaultWordDictionaryComponent extends WordDictionaryComponent { self =>

  case class DefaultWordDictionary(lang: Lang,
                                   private val dict: Map[Word, Definition])
      extends WordDictionary {

    def hasWord(word: Word): Boolean = dict.contains(word)

    def definition(word: Word): Option[Definition] = dict.get(word)

    def intoSyllables(word: Word): List[Syllable] =
      word.foldRight(List[Syllable]())((c, l) => (c.toString) :: l)
  }
}
