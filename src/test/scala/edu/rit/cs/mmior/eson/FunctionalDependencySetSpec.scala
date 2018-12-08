package edu.rit.cs.mmior.eson

import org.scalatest._

class FunctionalDependencySetSpec extends FlatSpec with Matchers {
  "adding b → c to a → b" should "infer a → c" in {
    var fds = new FunctionalDependencySet()
    fds += FunctionalDependency(Set('a), Set('b))
    fds += FunctionalDependency(Set('b), Set('c))
    fds should contain (FunctionalDependency(Set('a), Set('c)))
    fds.contains(FunctionalDependency(Set('a), Set('c))) shouldBe true
  }
}
