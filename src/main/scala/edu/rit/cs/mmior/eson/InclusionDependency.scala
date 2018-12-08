package edu.rit.cs.mmior.eson

case class InclusionDependency(leftTable: Symbol, leftFields: List[Symbol],
                               rightTable: Symbol, rightFields: List[Symbol]) {
  def reverse(): InclusionDependency = {
    InclusionDependency(rightTable, rightFields, leftTable, leftFields)
  }
}
