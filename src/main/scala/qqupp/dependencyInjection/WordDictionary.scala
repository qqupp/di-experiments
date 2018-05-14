package qqupp.dependencyInjection

trait WordDictionary {
  def lang: Lang
  def hasWord(word: Word): Boolean
  def definition(word: Word): Option[Definition]
  def intoSyllables(word: Word): List[Syllable]
}

class DefaultWordDictionary(val lang: Lang,
                            private val definitions: Map[Word, Definition])
    extends WordDictionary {

  def hasWord(word: Word): Boolean = definitions.contains(word)

  def definition(word: Word): Option[Definition] = definitions.get(word)

  def intoSyllables(word: Word): List[Syllable] =
    word.foldRight(List[Syllable]())((c, l) => (c.toString) :: l)
}

object DefaultWordDictionary {
  def apply(lang: Lang,
            definitions: Map[Word, Definition]): DefaultWordDictionary =
    new DefaultWordDictionary(lang, definitions)
}
