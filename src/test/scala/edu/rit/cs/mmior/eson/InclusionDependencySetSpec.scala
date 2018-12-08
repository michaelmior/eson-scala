package edu.rit.cs.mmior.eson

import org.scalatest._

class InclusionDependencySetSpec extends FlatSpec with Matchers {
  "adding R(AB) ⊆ S(CD)" should "also add R(A) ⊆ S(C)" in {
    var inds = new InclusionDependencySet()
    inds += InclusionDependency('R, List('A, 'B), 'S, List('C, 'D))
    inds should contain (InclusionDependency('R, List('A), 'S, List('C)))
    inds.contains(InclusionDependency('R, List('A), 'S, List('C))) shouldBe true
  }

  "adding R(A) ⊆ S(B) and S(B) ⊆ T(C)" should "also add R(A) ⊆ T(C)" in {
    var inds = new InclusionDependencySet()
    inds += InclusionDependency('R, List('A), 'S, List('B))
    inds += InclusionDependency('S, List('B), 'T, List('C))
    inds should contain (InclusionDependency('R, List('A), 'T, List('C)))
  }
}
