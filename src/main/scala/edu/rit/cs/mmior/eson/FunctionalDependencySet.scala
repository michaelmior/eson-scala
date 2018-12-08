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

  override def foreach[U](f: FunctionalDependency => U) = fds.map {
    case(left, right) => FunctionalDependency(left, right.toSet)
  }.foreach(f)

  // XXX: The set must already contain newFDs
  private def checkClosure(newFDs: Traversable[FunctionalDependency]): Unit = {
    var newerFDs = LinkedHashSet.empty[FunctionalDependency]
    newFDs.foreach { newFD =>
      this.foreach { oldFD =>
        val newerFD = if (oldFD.left subsetOf newFD.right) {
          Some(FunctionalDependency(newFD.left, oldFD.right.toSet))
        } else if (newFD.left subsetOf oldFD.right) {
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
