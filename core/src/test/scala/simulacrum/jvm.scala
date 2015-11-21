package simulacrum

import org.scalatest.{ WordSpec, Matchers }

class JvmTypeClassTest extends WordSpec with Matchers {
  "the @typeclass annotation" should {
    "generate serializable traits by default" in {
      seralizeDeserialize(Semigroup[Int]).append(1, 2) shouldBe 3
      seralizeDeserialize(Functor[List]).map(List(1, 2, 3))(_+1) shouldBe List(2, 3, 4)
    }
  }

  def seralizeDeserialize[A](value: A): A = {
    import java.io.{ ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream }
    val bytesOut = new ByteArrayOutputStream()
    val os = new ObjectOutputStream(bytesOut)
    os.writeObject(value)
    os.close()
    val is = new ObjectInputStream(new ByteArrayInputStream(bytesOut.toByteArray))
    is.readObject.asInstanceOf[A]
  }
}
