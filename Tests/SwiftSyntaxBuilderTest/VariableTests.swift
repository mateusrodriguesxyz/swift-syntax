//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import XCTest
import SwiftSyntax
import SwiftSyntaxBuilder

final class VariableTests: XCTestCase {
  func testVariableDecl() {
    let leadingTrivia = Trivia.unexpectedText("␣")

    let buildable = VariableDeclSyntax(leadingTrivia: leadingTrivia, letOrVarKeyword: .keyword(.let)) {
      PatternBindingSyntax(pattern: PatternSyntax("a"), typeAnnotation: TypeAnnotationSyntax(type: ArrayTypeSyntax(elementType: TypeSyntax("Int"))))
    }

    AssertBuildResult(buildable, "␣let a: [Int]")
  }

  func testVariableDeclWithStringParsing() {
    let testCases: [UInt: (VariableDeclSyntax, String)] = [
      #line: (
        VariableDeclSyntax("let content = try? String(contentsOf: url)"),
        "let content = try? String(contentsOf: url)"
      ),
      #line: (
        VariableDeclSyntax("let content = try! String(contentsOf: url)"),
        "let content = try! String(contentsOf: url)"
      ),
      #line: (
        VariableDeclSyntax("var newLayout: ContiguousArray<RawSyntax?>?"),
        "var newLayout: ContiguousArray<RawSyntax?>?"
      ),
      #line: (
        VariableDeclSyntax("var foo: String { myOptional!.someProperty }"),
        """
        var foo: String {
            myOptional!.someProperty
        }
        """
      ),
      #line: (
        VariableDeclSyntax("var foo: String? { myOptional?.someProperty }"),
        """
        var foo: String? {
            myOptional?.someProperty
        }
        """
      ),
      #line: (
        VariableDeclSyntax("let absoluteRaw = AbsoluteRawSyntax(raw: raw!, info: info)"),
        """
        let absoluteRaw = AbsoluteRawSyntax(raw: raw!, info: info)
        """
      ),
      #line: (
        VariableDeclSyntax("var foo: String { bar(baz!) }"),
        """
        var foo: String {
            bar(baz!)
        }
        """
      ),
      #line: (
        VariableDeclSyntax(#"var foo: String { bar ?? "" }"#),
        #"""
        var foo: String {
            bar ?? ""
        }
        """#
      ),
      #line: (
        VariableDeclSyntax("let bar = try! (foo())"),
        """
        let bar = try! (foo())
        """
      ),
      #line: (
        VariableDeclSyntax("let bar = try! !functionThatThrows()"),
        """
        let bar = try! !functionThatThrows()
        """
      ),
    ]

    for (line, testCase) in testCases {
      let (builder, expected) = testCase
      AssertBuildResult(builder, expected, line: line)
    }
  }

  func testVariableDeclWithValue() {
    let leadingTrivia = Trivia.unexpectedText("␣")

    let buildable = VariableDeclSyntax(leadingTrivia: leadingTrivia, letOrVarKeyword: .keyword(.var)) {
      PatternBindingSyntax(
        pattern: PatternSyntax("d"),
        typeAnnotation: TypeAnnotationSyntax(type: DictionaryTypeSyntax(keyType: TypeSyntax("String"), valueType: TypeSyntax("Int"))),
        initializer: InitializerClauseSyntax(value: DictionaryExprSyntax())
      )
    }

    AssertBuildResult(buildable, "␣var d: [String: Int] = [:]")
  }

  func testVariableDeclWithExplicitTrailingCommas() {
    let buildable = VariableDeclSyntax(
      letOrVarKeyword: .keyword(.let),
      bindings: [
        PatternBindingSyntax(
          pattern: PatternSyntax("a"),
          initializer: InitializerClauseSyntax(
            value: ArrayExprSyntax {
              for i in 1...3 {
                ArrayElementSyntax(
                  expression: IntegerLiteralExprSyntax(i),
                  trailingComma: .commaToken().withTrailingTrivia(.spaces(3))
                )
              }
            }
          )
        )
      ]
    )
    AssertBuildResult(
      buildable,
      """
      let a = [1,   2,   3,   ]
      """
    )
  }

  func testMultiPatternVariableDecl() {
    let buildable = VariableDeclSyntax(letOrVarKeyword: .keyword(.let)) {
      PatternBindingSyntax(
        pattern: PatternSyntax("a"),
        initializer: InitializerClauseSyntax(
          value: ArrayExprSyntax {
            for i in 1...3 {
              ArrayElementSyntax(expression: IntegerLiteralExprSyntax(i))
            }
          }
        )
      )
      PatternBindingSyntax(
        pattern: PatternSyntax("d"),
        initializer: InitializerClauseSyntax(
          value: DictionaryExprSyntax {
            for i in 1...3 {
              DictionaryElementSyntax(keyExpression: StringLiteralExprSyntax(content: "key\(i)"), valueExpression: IntegerLiteralExprSyntax(i))
            }
          }
        )
      )
      PatternBindingSyntax(pattern: PatternSyntax("i"), typeAnnotation: TypeAnnotationSyntax(type: TypeSyntax("Int")))
      PatternBindingSyntax(pattern: PatternSyntax("s"), typeAnnotation: TypeAnnotationSyntax(type: TypeSyntax("String")))
    }
    AssertBuildResult(buildable, #"let a = [1, 2, 3], d = ["key1": 1, "key2": 2, "key3": 3], i: Int, s: String"#)
  }

  func testClosureTypeVariableDecl() {
    let type = FunctionTypeSyntax(arguments: [TupleTypeElementSyntax(type: TypeSyntax("Int"))], returnType: TypeSyntax("Bool"))
    let buildable = VariableDeclSyntax(letOrVarKeyword: .keyword(.let)) {
      PatternBindingSyntax(pattern: PatternSyntax("c"), typeAnnotation: TypeAnnotationSyntax(type: type))
    }
    AssertBuildResult(buildable, "let c: (Int) -> Bool")
  }

  func testComputedProperty() {
    let testCases: [UInt: (VariableDeclSyntax, String)] = [
      #line: (
        VariableDeclSyntax(name: "test", type: TypeAnnotationSyntax(type: TypeSyntax("Int"))) {
          SequenceExprSyntax {
            IntegerLiteralExprSyntax(4)
            BinaryOperatorExprSyntax(text: "+")
            IntegerLiteralExprSyntax(5)
          }
        },
        """
        var test: Int {
            4 + 5
        }
        """
      ),
      #line: (
        VariableDeclSyntax("var foo: String") {
          ReturnStmtSyntax(#"return "hello world""#)
        },
        """
        var foo: String {
            return "hello world"
        }
        """
      ),
    ]

    for (line, testCase) in testCases {
      let (builder, expected) = testCase
      AssertBuildResult(builder, expected, line: line)
    }
  }

  func testAccessorList() {
    let buildable = VariableDeclSyntax(name: "test", type: TypeAnnotationSyntax(type: TypeSyntax("Int"))) {
      AccessorDeclSyntax(accessorKind: .keyword(.get), asyncKeyword: nil) {
        SequenceExprSyntax {
          IntegerLiteralExprSyntax(4)
          BinaryOperatorExprSyntax(text: "+")
          IntegerLiteralExprSyntax(5)
        }
      }

      AccessorDeclSyntax(accessorKind: .keyword(.willSet), asyncKeyword: nil) {}
    }

    AssertBuildResult(
      buildable,
      """
      var test: Int {
          get {
              4 + 5
          }
          willSet {
          }
      }
      """
    )
  }

  func testAttributedVariables() {
    let testCases: [UInt: (VariableDeclSyntax, String)] = [
      #line: (
        VariableDeclSyntax(
          attributes: AttributeListSyntax { AttributeSyntax(attributeName: TypeSyntax("Test")) },
          .var,
          name: "x",
          type: TypeAnnotationSyntax(type: TypeSyntax("Int"))
        ),
        """
        @Test var x: Int
        """
      ),
      #line: (
        VariableDeclSyntax(
          attributes: AttributeListSyntax { AttributeSyntax(attributeName: TypeSyntax("Test")) },
          name: "y",
          type: TypeAnnotationSyntax(type: TypeSyntax("String"))
        ) {
          StringLiteralExprSyntax(content: "Hello world!")
        },
        """
        @Test var y: String {
            "Hello world!"
        }
        """
      ),
      #line: (
        VariableDeclSyntax(
          attributes: AttributeListSyntax {
            AttributeSyntax("WithArgs") {
              TupleExprElementSyntax(expression: ExprSyntax("value1"))
              TupleExprElementSyntax(label: "label", expression: ExprSyntax("value2"))
            }
          },
          name: "z",
          type: TypeAnnotationSyntax(type: TypeSyntax("Float"))
        ) {
          FloatLiteralExprSyntax(0.0)
        },
        """
        @WithArgs(value1, label: value2) var z: Float {
            0.0
        }
        """
      ),
      #line: (
        VariableDeclSyntax(
          attributes: AttributeListSyntax {
            AttributeSyntax("WithArgs") {
              TupleExprElementSyntax(expression: ExprSyntax("value"))
            }
          },
          modifiers: [DeclModifierSyntax(name: .keyword(.public))],
          .let,
          name: "z",
          type: TypeAnnotationSyntax(type: TypeSyntax("Float"))
        ),
        """
        @WithArgs(value) public let z: Float
        """
      ),
    ]

    for (line, testCase) in testCases {
      let (builder, expected) = testCase
      AssertBuildResult(builder, expected, line: line)
    }
  }
}
