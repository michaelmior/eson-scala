package edu.rit.cs.mmior.eson

import org.scalatest._

class FunctionalDependencySetSpec extends FlatSpec with Matchers {
  "adding b → c to a → b" should "infer a → c" in {
    var fds = new FunctionalDependencySet()
    fds += FunctionalDependency(Set('a), 'b)
    fds += FunctionalDependency(Set('b), 'c)
    fds should contain (FunctionalDependency(Set('a), 'c))
    fds.contains(FunctionalDependency(Set('a), 'c)) shouldBe true
  }

  "the set with b → c" should "contain ab → c" in {
    var fds = new FunctionalDependencySet()
    fds += FunctionalDependency(Set('b), 'c)
    fds.contains(FunctionalDependency(Set('a, 'b), 'c)) shouldBe true
  }

  "adding b → d to {ad → c, c → e}" should "contain ab → c and ab → e" in {
    var fds = new FunctionalDependencySet()
    fds += FunctionalDependency(Set('a, 'd), 'c)
    fds += FunctionalDependency(Set('c), 'e)
    fds += FunctionalDependency(Set('b), 'd)
    fds should contain (FunctionalDependency(Set('a, 'b), 'c))
    fds.contains(FunctionalDependency(Set('a, 'b), 'c)) shouldBe true
    fds should contain (FunctionalDependency(Set('a, 'b), 'e))
    fds.contains(FunctionalDependency(Set('a, 'b), 'e)) shouldBe true
  }
}
