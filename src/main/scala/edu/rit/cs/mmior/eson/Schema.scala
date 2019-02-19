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
  var tables = HashSet.empty[Symbol]

  def +=(table: Symbol, fd: FunctionalDependency): Unit = {
    if (!fds.contains(table)) {
      fds(table) = new FunctionalDependencySet()
    }
    fds(table) += fd
    infer
  }

  def +=(ind: InclusionDependency): Unit = {
    inds += ind
  }

  def +=(table: Symbol): Unit = {
    tables += table
  }

  def infer(): Unit = {
    while (pullback || collect) {}
  }

  private def collect(): Boolean = {
    var changed = false
    inds.foreach { ind =>
      val left = ind.rightFields.slice(1, ind.rightFields.length - 1).toSet
      val right = ind.rightFields.last
      if (fds(ind.rightTable).contains(FunctionalDependency(left, right))) {
        val newLeft = ind.leftFields.slice(1, ind.leftFields.length - 1)
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
              if ((fd.left + fd.right).subsetOf(ind.rightFields.toSet) &&
                  ind.leftFields.size == fd.left.size + 1) {
                val left = ind.leftFields :+ ind2.leftFields.last
                val right = fd.left.toList :+ fd.right :+ ind2.rightFields.last
                val newIND = InclusionDependency(ind.leftTable, left, ind.rightTable, right)
                if (!inds.contains(newIND)) {
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
