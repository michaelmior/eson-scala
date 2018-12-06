package edu.rit.cs.mmior.eson

import org.scalatest._

class FunctionalDependencySpec extends FlatSpec with Matchers {
  "a → a" should "be trivial" in {
    FunctionalDependency(Set('a), Set('a)).isTrivial shouldBe true
  }

  "ab → a" should "be trivial" in {
    FunctionalDependency(Set('a, 'b), Set('a)).isTrivial shouldBe true
  }

  "a → ab" should "not be trivial" in {
    FunctionalDependency(Set('a), Set('a, 'b)).isTrivial shouldBe false
  }
}
