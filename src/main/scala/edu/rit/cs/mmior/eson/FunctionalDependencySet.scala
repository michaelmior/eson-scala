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

import scala.collection.mutable.{HashMap, LinkedHashSet}

class FunctionalDependencySet extends Traversable[FunctionalDependency] {
  private val fds = HashMap.empty[Set[Symbol], LinkedHashSet[Symbol]]

  def empty: FunctionalDependencySet = new FunctionalDependencySet()

  def contains(fd: FunctionalDependency): Boolean = {
    fd.left.subsets.exists { left =>
      fds.contains(left) && (fds(left).contains(fd.right))
    }
  }

  private def add(fd: FunctionalDependency): Unit = {
    if (!fds.contains(fd.left)) {
      fds(fd.left) = LinkedHashSet.empty[Symbol]
    }
    if (fds(fd.left).contains(fd.right)) { return }
    fds(fd.left) += fd.right

    fds.foreach { case (left, rights) =>
      if (left.contains(fd.right)) {
        rights.foreach( right => {
          add(FunctionalDependency(left - fd.right ++ fd.left, right))
        })
      }
    }
  }

  def +=(fd: FunctionalDependency): FunctionalDependencySet = {
    this.add(fd)
    checkClosure(Set(fd))
    this
  }

  override def foreach[U](f: FunctionalDependency => U): Unit = fds.flatMap {
    case(left, rights) => {
      rights.map(FunctionalDependency(left, _))
    }
  }.foreach(f)

  // XXX: The set must already contain newFDs
  def checkClosure(newFDs: Traversable[FunctionalDependency]): Unit = {
    var newerFDs = LinkedHashSet.empty[FunctionalDependency]
    newFDs.foreach { newFD =>
      this.foreach { oldFD =>
        val newerFD = if (oldFD.left.contains(newFD.right)) {
          Some(FunctionalDependency(newFD.left, oldFD.right))
        } else if (newFD.left.contains(oldFD.right)) {
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
