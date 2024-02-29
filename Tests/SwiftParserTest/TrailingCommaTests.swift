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
  }
  
  func testArgumentList() {
    
    assertParse(
      """
      if true, { value = 1 }
      expectEqual(value, 1)
      """,
      experimentalFeatures: .trailingComma
    )
    
    assertParse("f(1, 2, 3,)", experimentalFeatures: .trailingComma)
    assertParse(
      "f(1️⃣,)",
      diagnostics: [DiagnosticSpec(message: "expected value in function call", fixIts: ["insert value"])],
      fixedSource: "f(<#expression#>,)",
      experimentalFeatures: .trailingComma
    )
  }
  
  func testIfConditions() {
        
    assertParse("if true, f { $0 }, { true }(), { value = 2 } else { value = 0 }", experimentalFeatures: .trailingComma)
    assertParse("if conditionA, { value = 1 }", experimentalFeatures: .trailingComma)
    assertParse("if conditionA, conditionB, { value = 1 }", experimentalFeatures: .trailingComma)
    assertParse("if conditionA, {x}(), { value = 1 }", experimentalFeatures: .trailingComma)
    assertParse("if conditionA, { a in a == 1 }(1), { value = 1 }", experimentalFeatures: .trailingComma)
    assertParse("if conditionA, conditionB, { value = 1 } else if conditionC, {x}(), { value = 2 }", experimentalFeatures: .trailingComma)
    
    assertParse(
      "if 1️⃣,{}",
      diagnostics: [DiagnosticSpec(message: "missing condition in 'if' statement")],
      experimentalFeatures: .trailingComma
    )
    
    assertParse("if conditionA, {}, {}", experimentalFeatures: .trailingComma)
    
    assertParse(
      """
      value = 0
      if true, { value = 1 }
      expectEqual(value, 1)
      """,
      experimentalFeatures: .trailingComma
    )
    
  }
  
  func testGuardConditions() {
    assertParse("guard conditionA, else { break }", experimentalFeatures: .trailingComma)
    assertParse("guard conditionA, conditionB, else { break }", experimentalFeatures: .trailingComma)
    assertParse("guard conditionA, {x}(), else { break }", experimentalFeatures: .trailingComma)
    assertParse("guard conditionA, { a in a == 1 }(1), else { break }", experimentalFeatures: .trailingComma)
  }
  
  func testWhileConditions() {
    assertParse(
      """
      var value = 5
      while value != 0, {
          value -= 1
      }
      expectEqual(value, 0)
      """,
      experimentalFeatures: .trailingComma
    )
    assertParse("while conditionA, {x}(), { value += 1 }", experimentalFeatures: .trailingComma)
  }
  
}
