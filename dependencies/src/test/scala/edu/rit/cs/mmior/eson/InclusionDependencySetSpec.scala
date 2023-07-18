package edu.rit.cs.mmior.eson

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InclusionDependencySetSpec extends AnyFlatSpec with Matchers {
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

  "getting INDs for R and T in R(A) ⊆ S(B) and S(B) ⊆ T(C)" should "remove anything with T" in {
    var inds = new InclusionDependencySet()
    inds += InclusionDependency('R, List('A), 'S, List('B))
    inds += InclusionDependency('S, List('B), 'T, List('C))
    inds = inds.forTables(Set('R, 'S))
    inds should not contain (InclusionDependency('R, List('A), 'T, List('C)))
    inds should contain (InclusionDependency('R, List('A), 'S, List('B)))
  }

  "getting INDs for R in R(A) ⊆ S(B) and S(B) ⊆ T(C)" should "only have the IND for R" in {
    var inds = new InclusionDependencySet()
    inds += InclusionDependency('R, List('A), 'S, List('C))
    inds += InclusionDependency('R, List('A), 'R, List('B))
    inds = inds.forTable('R)
    inds should contain only (InclusionDependency('R, List('A), 'R, List('B)))
  }
}
