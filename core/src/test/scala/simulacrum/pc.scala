package simulacrum

import org.scalatest.WordSpec
import org.scalatest.Matchers._
import org.scalatest.OptionValues._

import org.ensime.pcplod._

class PresentationCompilerTest extends WordSpec {
  "the @typeclass annotation" should {
    "not produce warnings" in withMrPlod("example.scala") { mr: MrPlod =>
      mr.messages shouldBe 'empty

      mr.typeAtPoint('def1).value shouldBe "T"
    }
  }
}

