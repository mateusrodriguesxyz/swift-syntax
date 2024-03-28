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
import SwiftSyntax
import XCTest

final class MisspelledKeywordsTest: ParserTestCase {
  
  func testStatements() {
    
    assertParse("let t = Int.zelf")
    
    assertParse(
      "1️⃣fi true { }",
      diagnostics: [DiagnosticSpec(message: "did you mean to use `if` keyword?", fixIts: ["replace `fi` with `if`"])],
      fixedSource: "if true { }"
    )

    assertParse(
      "1️⃣ife true { }",
      diagnostics: [DiagnosticSpec(message: "did you mean to use `if` keyword?", fixIts: ["replace `ife` with `if`"])],
      fixedSource: "if true { }"
    )

    assertParse(
      "1️⃣guadr true else { }",
      diagnostics: [
        DiagnosticSpec(message: "did you mean to use `guard` keyword?", fixIts: ["replace `guadr` with `guard`"])
      ],
      fixedSource: "guard true else { }"
    )

    assertParse(
      "1️⃣whle true { }",
      diagnostics: [
        DiagnosticSpec(message: "did you mean to use `while` keyword?", fixIts: ["replace `whle` with `while`"])
      ],
      fixedSource: "while true { }"
    )
  }
  
  func testDeclaration() {
    assertParse(
      "1️⃣fun f() { }",
      diagnostics: [DiagnosticSpec(message: "did you mean to use `func` keyword?", fixIts: ["replace `fun` with `func`"])],
      fixedSource: "func f() { }"
    )
    assertParse(
      "1️⃣extnsion T { }",
      diagnostics: [DiagnosticSpec(message: "did you mean to use `extension` keyword?", fixIts: ["replace `extnsion` with `extension`"])],
      fixedSource: "extension T { }"
    )
    
    assertParse(
      """
      switch character {
      1️⃣cas "a":
          break
      default:
          break
      }
      """,
      diagnostics: [DiagnosticSpec(message: "did you mean to use `case` keyword?", fixIts: ["replace `cas` with `case`"])],
      fixedSource: """
      switch character {
      case "a":
          break
      default:
          break
      }
      """
    )
    
    assertParse(
    """
    func f<1️⃣eac T: Collection>(_ item: repeat each T) -> (repeat (each T).Element?) { }
    """,
    diagnostics: [DiagnosticSpec(message: "did you mean to use `each` keyword?", fixIts: ["replace `eac` with `each`"])],
    fixedSource: """
    func f<each T: Collection>(_ item: repeat each T) -> (repeat (each T).Element?) { }
    """
    )
    
  }
  
}
