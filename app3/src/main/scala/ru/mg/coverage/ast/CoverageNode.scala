package ru.mg.coverage.ast

import ru.mg.parsing.ast.EsqlAstNode
import ru.mg.parsing.broker.trace.ast.BrokerTraceAstNode

class CoverageNode(val esqlNode: EsqlAstNode, val traces: List[BrokerTraceAstNode])