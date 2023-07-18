package edu.rit.cs.mmior.eson

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import edu.rit.cs.mmior.eson.MetanomeConversions._
import edu.rit.cs.mmior.eson.{FunctionalDependency, InclusionDependency}

import de.metanome.algorithm_integration.{ColumnCombination, ColumnIdentifier, ColumnPermutation}
import de.metanome.algorithm_integration.results.{FunctionalDependency => MetanomeFD, InclusionDependency => MetanomeIND}

class MetanomeConversionsSpec extends AnyFlatSpec with Matchers {
  "R(A) ⊆ S(B)" should "convert from Metanome" in {
    val metanomeIND = new MetanomeIND(new ColumnPermutation(new ColumnIdentifier("R", "A")), new ColumnPermutation(new ColumnIdentifier("S", "B")))
    val ind: InclusionDependency = metanomeIND

    ind.leftTable shouldEqual 'R
    ind.rightTable shouldEqual 'S
    ind.leftFields shouldEqual List('A)
    ind.rightFields shouldEqual List('B)
  }

  "a → b" should "be trivial" in {
    val metanomeFD = new MetanomeFD(new ColumnCombination(new ColumnIdentifier("foo", "a")), new ColumnIdentifier("foo", "b"))
    val fd: FunctionalDependency = metanomeFD

    fd.left shouldEqual Set('a)
    fd.right shouldEqual 'b
  }
}
