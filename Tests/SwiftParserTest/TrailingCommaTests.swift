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

@_spi(RawSyntax) import SwiftParser
@_spi(RawSyntax) import SwiftSyntax
import XCTest

final class TrailingCommaTests: ParserTestCase {
  
  func testTuple() {
    assertParse("(1, 2, 3,)", diagnostics: [])
    assertParse(
      "(1️⃣,)",
      diagnostics: [DiagnosticSpec(message: "expected value in tuple", fixIts: ["insert value"])],
      fixedSource: "(<#expression#>,)"
    )
  }
  
  func testArgumentList() {
    assertParse("f(1, 2, 3,)", diagnostics: [])
    assertParse(
      "f(1️⃣,)",
      diagnostics: [DiagnosticSpec(message: "expected value in function call", fixIts: ["insert value"])],
      fixedSource: "f(<#expression#>,)"
    )
  }
  
  func testIfConditions() {
    
   // round-trip
        
    assertParse("if conditionA, { value = 1 }")
    assertParse("if conditionA, conditionB, { value = 1 }")
    assertParse("if conditionA, {x}(), { value = 1 }")
    assertParse("if conditionA, { a in a == 1 }(1), { value = 1 }")
    assertParse("if conditionA, conditionB, { value = 1 } else if conditionC, {x}(), { value = 2 }")
    
    assertParse(
      "if 1️⃣,{}",
      diagnostics: [DiagnosticSpec(message: "missing condition in 'if' statement")]
    )
    
    assertParse(
      "if conditionA, {}1️⃣, {}",
      diagnostics: [DiagnosticSpec(message: "extraneous code ', {}' at top level")]
    )
    
  }
  
  func testGuardConditions() {
    assertParse("guard conditionA, else { break }")
    assertParse("guard conditionA, conditionB, else { break }")
    assertParse("guard conditionA, {x}(), else { break }")
    assertParse("guard conditionA, { a in a == 1 }(1), else { break }")
  }
  
  func testWhileConditions() {
    assertParse("while conditionA, {x}(), { value += 1 }")
  }
  
}
