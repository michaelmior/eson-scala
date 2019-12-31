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

import collection.JavaConverters._
import scala.language.implicitConversions

import edu.rit.cs.mmior.eson.{FunctionalDependency, InclusionDependency}

import de.metanome.algorithm_integration.{ColumnCombination, ColumnIdentifier, ColumnPermutation}
import de.metanome.algorithm_integration.results.{FunctionalDependency => MetanomeFD, InclusionDependency => MetanomeIND}

object MetanomeConversions {
  implicit def fdFromMetanome(fd: MetanomeFD): FunctionalDependency = new FunctionalDependency(fd.getDeterminant, fd.getDependant)

  implicit def indFromMetanome(ind: MetanomeIND): InclusionDependency = new InclusionDependency(
    Symbol(ind.getDependant.getColumnIdentifiers.get(0).getTableIdentifier),
    ind.getDependant,
    Symbol(ind.getReferenced.getColumnIdentifiers.get(0).getTableIdentifier),
    ind.getReferenced)

  implicit def symbolFromColumnID(col: ColumnIdentifier): Symbol = Symbol(col.getColumnIdentifier)

  implicit def symbolSetFromColumnCombo(combo: ColumnCombination): Set[Symbol] =
    combo.getColumnIdentifiers.asScala.map(c => Symbol(c.getColumnIdentifier)).toSet

  implicit def symbolListFromColumnPermutation(perm: ColumnPermutation): List[Symbol] =
    perm.getColumnIdentifiers.asScala.map(c => Symbol(c.getColumnIdentifier)).toList
}
