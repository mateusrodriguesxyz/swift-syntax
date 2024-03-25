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
}
