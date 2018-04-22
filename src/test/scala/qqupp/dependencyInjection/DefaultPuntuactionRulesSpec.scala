package qqupp.dependencyInjection

import org.scalatest._

class DefaultPuntuactionRulesSpec extends FlatSpec with Matchers {
  import OutsideScope._

  "The checkHyphenation" should "not fail for foo" in {
    val prc = new DefaultPunctuationRulesComponent with WordDictionaryComponentMock {
      def punctuationRules: DefaultPunctuationRules =
        new DefaultPunctuationRules("Test")
    }

    dependantTypeTestCheckHyphenation(prc)(prc.punctuationRules)

    genericTypeTestCheckHyphenation(prc.punctuationRules)
  }
}

object OutsideScope extends FlatSpec with Matchers {
  def dependantTypeTestCheckHyphenation(prc: DefaultPunctuationRulesComponent)(pr: prc.PunctuationRules) =
    pr.checkHyphenation("fo", "o") shouldEqual true

  def genericTypeTestCheckHyphenation(pr: DefaultPunctuationRulesComponent#PunctuationRules) =
    pr.checkHyphenation("f", "oo") shouldEqual true
}

trait WordDictionaryComponentMock extends WordDictionaryComponent {
  val wordDictionary = new WordDictionaryMock

  type WordDictionary = WordDictionaryMock

  class WordDictionaryMock extends WordDictionaryInterface {
    override def lang: Lang = "Test"

    override def hasWord(word: Word): Boolean = word == "foo"

    override def definition(word: Word): Option[Definition] = None

    override def intoSyllables(word: Word): List[Syllable] =
      if (word == "foo") List("f", "o", "o") else List()
  }
}
