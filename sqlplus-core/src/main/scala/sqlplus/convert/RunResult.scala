package sqlplus.convert

import sqlplus.expression.{Expression, Variable}
import sqlplus.graph.{ComparisonHyperGraph, JoinTree}

case class RunResult(candidates: List[(JoinTree, ComparisonHyperGraph, List[ExtraCondition])],
                     outputVariables: List[Variable], computations: List[(Variable, Expression)], isFull: Boolean, isFreeConnex: Boolean,
                     groupByVariables: List[Variable], aggregations: List[(Variable, String, List[Expression])],
                     optTopK: Option[TopK]) {
    override def toString: String = {
        val indent = "  "

        s"""RunResult(
           |candidates = [
           |${formatCandidates(candidates, indent)}
           |],
           |outputVariables = ${formatVariables(outputVariables)},
           |computations = [
           |${formatComputations(computations, indent)}
           |],
           |isFull = $isFull,
           |isFreeConnex = $isFreeConnex,
           |groupByVariables = ${formatVariables(groupByVariables)},
           |aggregations = [
           |${formatAggregations(aggregations, indent)}
           |],
           |optTopK = ${optTopK.fold("None")(_.toString)}
           |)""".stripMargin.replace("\n", "\n" + indent)
    }

    private def formatCandidates(candidates: List[(JoinTree, ComparisonHyperGraph, List[ExtraCondition])],
                                 indent: String): String = {
        candidates.map { case (tree, graph, conds) =>
            s"""(
               |${indent}JoinTree: ${tree.toString},
               |${indent}HyperGraph: ${graph.toString},
               |${indent}Conditions: [
               |${conds.map(c => indent*2 + c).mkString("\n")}
               |${indent}]
               |)""".stripMargin
        }.mkString(",\n" + indent*2)
    }

    private def formatVariables(vars: List[Variable]): String =
        if (vars.isEmpty) "[]" else vars.mkString("[", ", ", "]")

    private def formatComputations(comps: List[(Variable, Expression)], indent: String): String =
        comps.map { case (v, e) =>
            s"${v.toString} -> ${e.toString}"
        }.mkString(",\n" + indent)

    private def formatAggregations(aggs: List[(Variable, String, List[Expression])], indent: String): String = {
        aggs.map { case (v, op, exprs) =>
            s"""Aggregation(
               |${indent}target: ${v.toString},
               |${indent}operation: "$op",
               |${indent}parameters: [
               |${exprs.map(e => indent*2 + e.toString).mkString(",\n")}
               |${indent}]
               |)""".stripMargin
        }.mkString(",\n" + indent)
    }
}

object RunResult {
    def buildFromSingleResult(result: (JoinTree, ComparisonHyperGraph, List[ExtraCondition]),
                              outputVariables: List[Variable], computations: List[(Variable, Expression)], isFull: Boolean, isFreeConnex: Boolean,
                              groupByVariables: List[Variable], aggregations: List[(Variable, String, List[Expression])],
                              optTopK: Option[TopK]): RunResult = {
        RunResult(List(result), outputVariables, computations, isFull, isFreeConnex, groupByVariables, aggregations, optTopK)
    }
}
