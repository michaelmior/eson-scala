package edu.rit.cs.mmior.eson

import org.scalatest._

class SchemaSpec extends FlatSpec with Matchers {
  "R(UV) ⊆ S(XY) and X → Y" should "infer U → V" in {
    val schema = new Schema()
    schema += 'R
    schema += 'S
    schema += InclusionDependency('R, List('U, 'V), 'S, List('X, 'Y))
    schema.+=('S, FunctionalDependency(Set('X), 'Y))
    schema.infer

    schema.fds('R).contains(FunctionalDependency(Set('U), 'V)) shouldBe true
  }

  "R(UV) ⊆ S(XY), R(UW) ⊆ S(XZ), and X → Y" should "infer R(UVW) ⊆ S(XYZ)" in {
    val schema = new Schema()
    schema += 'R
    schema += 'S
    schema += InclusionDependency('R, List('U, 'V), 'S, List('X, 'Y))
    schema += InclusionDependency('R, List('U, 'W), 'S, List('X, 'Z))
    schema.+=('S, FunctionalDependency(Set('X), 'Y))
    schema.infer

    schema.inds.contains(InclusionDependency('R, List('U, 'V, 'W),
                                             'S, List('X, 'Y, 'Z))) shouldBe true
  }

  "R(ABCDE) with dependencies A → BC and C → DE" should "decompose to R_1(ABC) and R_2(CDE)" in {
    val schema = new Schema()
    schema += 'R
    schema.+=('R, FunctionalDependency(Set('A), 'B))
    schema.+=('R, FunctionalDependency(Set('A), 'C))
    schema.+=('R, FunctionalDependency(Set('C), 'D))
    schema.+=('R, FunctionalDependency(Set('C), 'E))
    schema.bcnf_decompose

    schema.tables should not contain 'R
    schema.tables('R_1) should equal(Set('C, 'D, 'E))
    schema.tables('R_2) should equal(Set('A, 'B, 'C))
  }
}
