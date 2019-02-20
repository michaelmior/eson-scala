// Copyright (C) 2018 the original author or authors.
// See the LICENCE.md file distributed with this work for additional
// information regarding copyright ownership.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

package edu.rit.cs.mmior.eson

import scala.collection.mutable.{HashMap, HashSet}

class Schema {
  var inds = new InclusionDependencySet()
  var fds = HashMap.empty[Symbol, FunctionalDependencySet]
  var tables = HashMap.empty[Symbol, HashSet[Symbol]]

  def +=(table: Symbol, fd: FunctionalDependency): Unit = {
    if (!fds.contains(table)) {
      fds(table) = new FunctionalDependencySet()
    }
    fds(table) += fd
    tables(table) ++= fd.left + fd.right
    infer
  }

  def +=(ind: InclusionDependency): Unit = {
    inds += ind
    tables(ind.leftTable) ++= ind.leftFields
    tables(ind.rightTable) ++= ind.rightFields
  }

  def +=(table: Symbol): Unit = {
    tables(table) = new HashSet[Symbol]
  }

  def fold(): Unit =  {
    var folded = true
    while (folded) {
      folded =  false

      // Remove tables which are not needed
      while (inds.exists { ind =>
        if (inds.contains(ind.reverse) &&
              ind.leftFields.toSet == tables(ind.leftTable).toSet &&
              ind.leftFields.size >= ind.rightFields.size) {
          tables.remove(ind.leftTable)
          fds.remove(ind.leftTable)
          if (fds.contains(ind.leftTable) && fds(ind.leftTable).isEmpty) {
            fds.remove(ind.leftTable)
          }
          inds = inds.forTables(tables.keys.toSet - ind.leftTable, true)

          folded = true
          true
        } else {
          false
        }
      }) {}

      // Remove fields which are not needed
      while (inds.exists { ind =>
        fds(ind.leftTable).exists { fd =>
          if ((fd.left + fd.right).subsetOf(ind.leftFields.toSet)) {
            fds(ind.leftTable).removeField(fd.right)
            inds.removeField(ind.leftTable, fd.right)
            tables(ind.leftTable) -= fd.right

            folded = true
            true
          } else {
            false
          }
        }
      }) {}
    }
  }

  def bcnf_decompose(): Unit = {
    var decomposed = true
    while (decomposed) {
      decomposed = fds.exists { case (table, fdSet) =>
        var doesDecompose = false
        fdSet.getAll().exists { case (left, right) =>
          val isKey = (tables(table) -- left).subsetOf(right)
          if (!isKey) {
            doesDecompose = true

            val newLeftTable = Symbol(table.name + "_1")
            val newRightTable = Symbol(table.name + "_2")
            tables(newLeftTable) = HashSet() ++ left ++ right
            tables(newRightTable) = HashSet() ++ (tables(table) -- right)

            // Copy FDs to the new tables
            fds(newLeftTable) = new FunctionalDependencySet()
            fds(newRightTable) = new FunctionalDependencySet()
            fdSet.foreach { fd =>
              if ((fd.left + fd.right).subsetOf(tables(newLeftTable))) {
                fds(newLeftTable) += fd
              }
              if ((fd.left + fd.right).subsetOf(tables(newRightTable))) {
                fds(newRightTable) += fd
              }
            }

            // Remove old FDs
            fds.remove(table)

            // Copy INDs to the new tables
            inds.foreach { ind =>
              if (ind.leftTable == table) {
                if (ind.leftFields.toSet.subsetOf(tables(newLeftTable))) {
                  inds += InclusionDependency(newLeftTable, ind.leftFields, ind.rightTable, ind.rightFields)
                }
                if (ind.leftFields.toSet.subsetOf(tables(newRightTable))) {
                  inds += InclusionDependency(newRightTable, ind.leftFields, ind.rightTable, ind.rightFields)
                }
              }
              if (ind.rightTable == table) {
                if (ind.rightFields.toSet.subsetOf(tables(newLeftTable))) {
                  inds += InclusionDependency(ind.leftTable, ind.leftFields, newLeftTable, ind.rightFields)
                }
                if (ind.rightFields.toSet.subsetOf(tables(newRightTable))) {
                  inds += InclusionDependency(ind.leftTable, ind.leftFields, newRightTable, ind.rightFields)
                }
              }
            }

            // Remove old INDs
            inds = inds.forTables(tables.keys.toSet - table, true)

            // Remove the old table
            tables.remove(table)

            val commonFields = tables(newLeftTable) & tables(newRightTable)
            commonFields.foreach { field =>
              inds += InclusionDependency(newLeftTable, List(field), newRightTable, List(field))
              inds += InclusionDependency(newRightTable, List(field), newLeftTable, List(field))
            }
          }

          doesDecompose
        }
      }
    }
  }

  def infer(): Unit = {
    while (pullback || collect) {}
  }

  private def collect(): Boolean = {
    var changed = false
    inds.foreach { ind =>
      val left = ind.rightFields.slice(0, ind.rightFields.length - 1).toSet
      val right = ind.rightFields.last
      if (fds(ind.rightTable).contains(FunctionalDependency(left, right))) {
        val newLeft = ind.leftFields.slice(0, ind.leftFields.length - 1)
        val newRight = ind.leftFields.last
        val newFD = FunctionalDependency(newLeft.toSet, newRight)
        if (!fds.contains(ind.leftTable) || !fds(ind.leftTable).contains(newFD)) {
          this.+=(ind.leftTable, newFD)
          changed = true
        }
      }
    }

    changed
  }

  private def pullback(): Boolean = {
    var changed = false
    fds.foreach { case (table, fdSet) =>
      fdSet.foreach { fd =>
        val tableInds = inds.forTable(table, false)
        tableInds.foreach { ind =>
          if (ind.rightFields.toSet == (fd.left + fd.right)) {
            tableInds.foreach { ind2 =>
              if (ind != ind2 && (fd.left + fd.right).subsetOf(ind.rightFields.toSet) &&
                  ind.leftFields.zip(ind2.leftFields).takeWhile(x => x._1 == x._2).length == fd.left.size &&
                  fd.left.subsetOf(ind2.rightFields.toSet)) {
                val left = ind.leftFields :+ ind2.leftFields.last
                val right = fd.left.toList :+ fd.right :+ ind2.rightFields.last
                val newIND = InclusionDependency(ind.leftTable, left, ind.rightTable, right)
                if (!inds.contains(newIND) && left.distinct == left && right.distinct == right) {
                  if (newIND.leftFields.toSet.size != newIND.leftFields.size) {
                    System.exit(1)
                  }
                  if (newIND.rightFields.toSet.size != newIND.rightFields.size) {
                    System.exit(1)
                  }
                  inds += newIND
                  changed = true
                }
              }
            }
          }
        }
      }
    }
    changed
  }
}
