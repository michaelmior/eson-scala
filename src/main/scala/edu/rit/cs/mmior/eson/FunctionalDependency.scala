package edu.rit.cs.mmior.eson

case class FunctionalDependency(left: Set[Symbol], right: Set[Symbol]) {
  def isTrivial: Boolean = {
    right subsetOf left
  }
}
