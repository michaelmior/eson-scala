package edu.rit.cs.mmior.eson

import org.scalatest._

class InclusionDependencySpec extends FlatSpec with Matchers {
  "R(A) ⊆ S(B)" should "reverse to S(B) ⊆ R(A)" in {
    InclusionDependency('R, List('A), 'S, List('B)).reverse shouldBe InclusionDependency('S, List('B), 'R, List('A))
  }
}
