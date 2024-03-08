//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_spi(ExperimentalLanguageFeatures) import SwiftParser
import XCTest

final class TrailingCommaTests: ParserTestCase {
  func testTuple() {
    assertParse("(1, 2, 3,)", experimentalFeatures: .trailingComma)

    assertParse(
      "(1️⃣,)",
      diagnostics: [DiagnosticSpec(message: "expected value in tuple", fixIts: ["insert value"])],
      fixedSource: "(<#expression#>,)",
      experimentalFeatures: .trailingComma
    )

    assertParse(
      "ℹ️(1, 2, 3,1️⃣",
      diagnostics: [DiagnosticSpec(message: "expected ')' to end tuple", notes: [NoteSpec(message: "to match this opening '('")], fixIts: ["insert ')'"])],
      fixedSource: "(1, 2, 3,)",
      experimentalFeatures: .trailingComma
    )
  }

  func testArgumentList() {
    assertParse("f(1, 2, 3,)", experimentalFeatures: .trailingComma)
    
    assertParse(
      "fℹ️(1, 2, 3,1️⃣",
      diagnostics: [
        DiagnosticSpec(
          message: "expected ')' to end function call",
          notes: [NoteSpec(message: "to match this opening '('")],
          fixIts: ["insert ')'"]
        )
      ],
      fixedSource: "f(1, 2, 3,)",
      experimentalFeatures: [.trailingComma]
    )
    
    assertParse(
      "f(1️⃣,)",
      diagnostics: [DiagnosticSpec(message: "expected value in function call", fixIts: ["insert value"])],
      fixedSource: "f(<#expression#>,)",
      experimentalFeatures: .trailingComma
    )
  }

  func testIfConditions() {
    assertParse(
      """
      if true, f { $0 }, { true }(), { a in a == 1 }(1), { print("if") } else { print("else") }
      """,
      experimentalFeatures: .trailingComma
    )

    assertParse("if true, { if true { { } } }", experimentalFeatures: .trailingComma)

    assertParse("if true, { true } { print(0) }", experimentalFeatures: .trailingComma)

    assertParse(
      """
      if true, { print(0) }
      (1, 2, 3)
      """,
      experimentalFeatures: .trailingComma
    )

    assertParse(
      """
      if true, { print(0) }
      { }()
      """,
      experimentalFeatures: .trailingComma
    )

    assertParse(
      "if 1️⃣,{ }",
      diagnostics: [DiagnosticSpec(message: "missing condition in 'if' statement")],
      experimentalFeatures: .trailingComma
    )
  }

  func testGuardConditions() {
    assertParse("guard true, f { $0 }, { true }(), { a in a == 1 }(1), else { break }", experimentalFeatures: .trailingComma)

    assertParse(
      "guard true,1️⃣",
      diagnostics: [DiagnosticSpec(message: "expected 'else' and body in 'guard' statement", fixIts: ["insert 'else' and body"])],
      fixedSource: "guard true, else { \n}",
      experimentalFeatures: .trailingComma
    )

    assertParse(
      "guard 1️⃣, else { return }",
      diagnostics: [DiagnosticSpec(message: "expected expression in 'guard' statement", fixIts: ["insert expression"])],
      fixedSource: "guard <#expression#>, else { return }",
      experimentalFeatures: .trailingComma
    )

    assertParse(
      "guard true, 1️⃣, else { return }",
      diagnostics: [DiagnosticSpec(message: "expected expression in 'guard' statement", fixIts: ["insert expression"])],
      fixedSource: "guard true, <#expression#>, else { return }",
      experimentalFeatures: .trailingComma
    )
  }

  func testWhileConditions() {
    assertParse("while true, f { $0 }, { true }(), { a in a == 1 }(1), { print(0) }", experimentalFeatures: .trailingComma)
  }

}
