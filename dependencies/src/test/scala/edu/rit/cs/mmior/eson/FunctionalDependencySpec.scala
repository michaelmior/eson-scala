package edu.rit.cs.mmior.eson

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FunctionalDependencySpec extends AnyFlatSpec with Matchers {
  "a → a" should "be trivial" in {
    FunctionalDependency(Set('a), 'a).isTrivial shouldBe true
  }

  "ab → a" should "be trivial" in {
    FunctionalDependency(Set('a, 'b), 'a).isTrivial shouldBe true
  }

  "a → b" should "not be trivial" in {
    FunctionalDependency(Set('a), 'b).isTrivial shouldBe false
  }

  "a → b" should "convert to a string" in {
    FunctionalDependency(Set('a), 'b).toString shouldBe "a → b"
  }
}
