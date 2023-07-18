// Copyright (C) 2018-2019 the original author or authors.
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

import scala.collection.mutable.{HashMap, LinkedHashSet}

class InclusionDependencySet() extends Traversable[InclusionDependency] {
  private val inds = HashMap.empty[(Symbol, List[Symbol], Symbol), LinkedHashSet[List[Symbol]]]

  def contains(ind: InclusionDependency): Boolean = {
    val key = (ind.leftTable, ind.leftFields, ind.rightTable)
    inds.contains(key) && inds(key).contains(ind.rightFields)
  }

  override def iterator: Iterator[InclusionDependency] = inds.flatMap { case((leftTable, leftFields, rightTable), rights) =>
    rights.map(InclusionDependency(leftTable, leftFields, rightTable, _))
  }.iterator

  private def add(ind: InclusionDependency): Unit = {
    val indexSet = (0 to ind.leftFields.size - 1).toSet.subsets.filter(s => !s.isEmpty)
    indexSet.foreach { indices =>
      indices.toList.permutations.foreach { indexList =>
        val leftFields = indexList.map(ind.leftFields(_))
        val rightFields = indexList.map(ind.rightFields(_))
        val subIND = InclusionDependency(ind.leftTable, leftFields, ind.rightTable, rightFields)
        this.addOne(subIND)
      }
    }
  }

  private def addOne(ind: InclusionDependency): Unit = {
    val key = (ind.leftTable, ind.leftFields, ind.rightTable)
    if (!inds.contains(key)) {
      inds(key) = LinkedHashSet.empty[List[Symbol]]
    }
    inds(key) += ind.rightFields
  }

  def +=(ind: InclusionDependency): InclusionDependencySet = {
    this.add(ind)
    checkClosure(List(ind))
    this
  }

  private def checkClosure(newINDs: Traversable[InclusionDependency]): Unit = {
    var newerINDs = LinkedHashSet.empty[InclusionDependency]
    newINDs.foreach { newIND =>
      this.foreach { oldIND =>
        val newerIND = if (oldIND.rightTable == newIND.leftTable && oldIND.rightFields == newIND.leftFields) {
          Some(InclusionDependency(oldIND.leftTable, oldIND.leftFields, newIND.rightTable, newIND.rightFields))
        } else if (newIND.rightTable == oldIND.leftTable && newIND.rightFields == oldIND.leftFields) {
          Some(InclusionDependency(newIND.leftTable, newIND.leftFields, oldIND.rightTable, oldIND.rightFields))
        } else {
          None
        }

        if (!newerIND.isEmpty && !this.contains(newerIND.get)) {
          newerINDs += newerIND.get
        }
      }
    }

    if (newerINDs.size > 0) {
      newerINDs.foreach(this.add)
      checkClosure(newerINDs)
    }
  }

  def removeField(table: Symbol, field: Symbol): Unit = {
    inds.retain { case ((_, leftFields, _), _) =>
      !leftFields.contains(field)
    }

    inds.foreach { case ((_, _, rightTable),  rightFields) =>
      if (rightTable == table) {
        rightFields.retain(!_.contains(field))
      }
    }
  }

  def forTable(table: Symbol, only: Boolean = true): InclusionDependencySet = {
    forTables(Set(table), only)
  }

  def forTables(tables: Set[Symbol], only: Boolean = true): InclusionDependencySet = {
    var newSet = new InclusionDependencySet()
    this.filter(ind => {
      if (only) {
        tables.contains(ind.leftTable) && tables.contains(ind.rightTable)
      } else {
        tables.contains(ind.leftTable) || tables.contains(ind.rightTable)
      }
    }).foreach { ind =>
      newSet += ind
    }
    newSet
  }
}
