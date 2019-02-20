package edu.rit.cs.mmior.eson

import org.scalatest._
import matchers._

trait CustomMatchers {
  class TableWithFieldsMatcher(fields: Set[Symbol]) extends Matcher[Schema] {
    def apply(left: Schema) = {
      MatchResult(
        left.tables.exists { case (table, tableFields) => tableFields == fields },
        s"No table found with fields ${fields.map(_.name).mkString(", ")}",
        s"Table found with fields ${fields.map(_.name).mkString(", ")}",
        s""
      )
    }
  }

  def haveTableWithFields(fields: Set[Symbol]) = new TableWithFieldsMatcher(fields)

  class TableCountMatcher(count: Integer) extends Matcher[Schema] {
    def apply(left: Schema) =  {
      MatchResult(
        left.tables.size == count,
        s"Schema has ${left.tables.size} tables, ${count} expected",
        s"Schema has ${count} tables"
      )
    }
  }

  def haveTableCount(count: Integer) = new TableCountMatcher(count)
}

object CustomMatchers extends CustomMatchers
