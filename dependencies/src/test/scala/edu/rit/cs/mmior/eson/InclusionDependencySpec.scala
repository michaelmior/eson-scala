package edu.rit.cs.mmior.eson

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InclusionDependencySpec extends AnyFlatSpec with Matchers {
  "R(A) ⊆ S(B)" should "reverse to S(B) ⊆ R(A)" in {
    InclusionDependency('R, List('A), 'S, List('B)).reverse shouldBe InclusionDependency('S, List('B), 'R, List('A))
  }

  "R(A) ⊆ S(B)" should "convert to a string" in {
    InclusionDependency('R, List('A), 'S, List('B)).toString shouldBe "R(A) ⊆ S(B)"
  }

  "R(A, B) ⊆ S(A, B)" should "convert to a string" in {
    InclusionDependency('R, List('A, 'B), 'S, List('A, 'B)).toString shouldBe "R(A, B) ⊆ S(…)"
  }
}
