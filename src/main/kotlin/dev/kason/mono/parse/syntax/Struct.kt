package dev.kason.mono.parse.syntax

import dev.kason.mono.core.rangeTo
import dev.kason.mono.parse.lexing.Token
import dev.kason.mono.parse.lexing.TokenKinds

data class StructGeneric(
	val name: Token,
	val type: TypeExpression?
) : SyntaxNode(if (type != null) name.range..type.range else name.range) {

	init {
		if (type != null) this += type
	}

	override fun toString(): String = "$name${if (type != null) ": $type" else ""}"
}

// parse T [:TypeExpr]
// if the next token is a colon, then parse the type expression
// otherwise, the type is null
fun SyntaxParser.parseGeneric(): StructGeneric {
	val name = cursor.expect { it.tokenKind == TokenKinds.Identifier }
	val type = cursor.eatIf { it.text == ":" }?.let { parseTypeExpression() }
	return StructGeneric(name, type)
}

fun SyntaxParser.parseStructGenerics(): List<StructGeneric> {
	cursor.expect { it.text == "<" }
	val generics = mutableListOf<StructGeneric>()
	while (cursor.hasNext()) {
		generics += parseGeneric()
		if (cursor.current?.text == ">") {
			break
		}
		cursor.expect { it.text == "," }
	}
	return generics
}

data class Struct(
	val structToken: Token,
	val name: String,
	val generics: List<StructGeneric>?,
	val fields: List<Field>
) : SyntaxNode(structToken.range..fields.last().range) {
	init {
		for (field in fields) {
			this += field
		}
		if (generics != null) {
			for (generic in generics) {
				this += generic
			}
		}
	}
}

data class Field(
	val name: IdentifierNode,
	val type: TypeExpression,
	val default: Expression?
) : SyntaxNode(name, type, default) {
	override fun toString(): String = "$name: $type = $default"
}

fun SyntaxParser.parseStruct(): Struct {
	val structToken = cursor.expect { it.text == "struct" }
	val name = cursor.expect { it.tokenKind == TokenKinds.Identifier }.text
	val generics = if (cursor.current?.text == "<") {
		parseStructGenerics()
	} else {
		null
	}
	cursor.eat()
	val block = parseIndentedBlock()
	// this includes statements, other expressions and non-field things.
	// we want to convert these into fields

	// each statement is either:
	// [Ident]: [Type]
	// or
	// [Ident]: [Type] = [Expression]

	// for case 1, the statement should be simply
	// TypeBinaryOperatorNode(operator=Conversion, left=IdentifierNode(name), right=type)
	// for case 2, the statement should be
	// BinaryOperatorNode(Assignment, left=<case 1>, right=expression)

	val fields = mutableListOf<Field>()
	for (statement in block.statements) {
		when (statement) {
			is TypeBinaryOperatorNode -> {
				if (statement.operator != TypeOperator.Conversion)
					error("not a field in struct $name: $statement")
				val identifierNode = statement.left as? IdentifierNode
					?: error("not a field in struct $name: $statement")
				identifierNode.removeParent()
				val type = statement.right
				type.removeParent()
				val field = Field(
					identifierNode,
					type,
					null
				)
				fields += field
			}

			is BinaryOperatorNode -> {
				if (statement.operator != AssignmentOperator.Assignment)
					error("not a field in struct $name: $statement")
				val left = statement.left as? TypeBinaryOperatorNode
					?: error("not a field in struct $name: $statement")
				val identifierNode = left.left as? IdentifierNode
					?: error("not a field in struct $name: $statement")
				identifierNode.removeParent()
				val type = left.right
				type.removeParent()
				val default = statement.right
				default.removeParent()
				val field = Field(identifierNode, type, default)
				fields += field
			}

			else -> error("not a field in struct $name: $statement")
		}
	}
	return Struct(structToken, name, generics, fields)
}