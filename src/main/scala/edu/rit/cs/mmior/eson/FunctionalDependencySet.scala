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

import scala.collection.SetLike
import scala.collection.mutable.{HashMap, LinkedHashSet}

class FunctionalDependencySet extends Traversable[FunctionalDependency] {
  private val fds = HashMap.empty[Set[Symbol], LinkedHashSet[Symbol]]

  def empty: FunctionalDependencySet = new FunctionalDependencySet()

  def contains(fd: FunctionalDependency): Boolean = {
    fds.contains(fd.left) && (fd.right subsetOf fds(fd.left))
  }

  private def add(fd: FunctionalDependency): Unit = {
    if (!fds.contains(fd.left)) {
      fds(fd.left) = LinkedHashSet.empty[Symbol]
    }
    fds(fd.left) ++= fd.right
  }

  def +=(fd: FunctionalDependency): FunctionalDependencySet = {
    this.add(fd)
    checkClosure(Set(fd))
    this
  }

  override def foreach[U](f: FunctionalDependency => U): Unit = fds.flatMap {
    case(left, right) => {
      right.subsets.filter(s => !s.isEmpty).map(s => FunctionalDependency(left, s.toSet))
    }
  }.foreach(f)

  // XXX: The set must already contain newFDs
  private def checkClosure(newFDs: Traversable[FunctionalDependency]): Unit = {
    var newerFDs = LinkedHashSet.empty[FunctionalDependency]
    newFDs.foreach { newFD =>
      this.foreach { oldFD =>
        val newerFD = if (oldFD.left == newFD.right) {
          Some(FunctionalDependency(newFD.left, oldFD.right.toSet))
        } else if (newFD.left == oldFD.right) {
          Some(FunctionalDependency(oldFD.left, newFD.right))
        } else {
          None
        }

        if (!newerFD.isEmpty && !this.contains(newerFD.get)) {
          newerFDs += newerFD.get
        }
      }
    }

    if (newerFDs.size > 0) {
      newerFDs.foreach(this.add)
      checkClosure(newerFDs)
    }
  }
}
