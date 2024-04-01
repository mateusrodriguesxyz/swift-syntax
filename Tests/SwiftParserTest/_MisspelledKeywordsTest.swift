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

import XCTest

final class MisspelledKeywordsTest: ParserTestCase {
  func testFunctionEffectSpecifiers() {
      
    assertParse(
      "func foo() 1️⃣sync { }",
      diagnostics: [
        DiagnosticSpec(message: "did you mean to use `async` keyword?", fixIts: ["replace `sync` with `async`"])
      ],
      fixedSource: "func foo() async { }"
    )

    assertParse(
      "func foo() 1️⃣trows { }",
      diagnostics: [
        DiagnosticSpec(message: "did you mean to use `throws` keyword?", fixIts: ["replace `trows` with `throws`"])
      ],
      fixedSource: "func foo() throws { }"
    )
  }

  func testSelf() {
    assertParse(
      "try decoder.decode(T.1️⃣zelf, from: data)",
      diagnostics: [
        DiagnosticSpec(message: "did you mean to use `self` keyword?", fixIts: ["replace `zelf` with `self`"])
      ],
      fixedSource: "try decoder.decode(T.self, from: data)"
    )
  }

  func testStatements() {
    assertParse(
      """
      class S {
        1️⃣ini() { }
        2️⃣dinit { }
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "did you mean to use `init` keyword?",
          fixIts: ["replace `ini` with `init`"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "did you mean to use `deinit` keyword?",
          fixIts: ["replace `dinit` with `deinit`"]
        )
      ],
      fixedSource: """
      class S {
        init() { }
        deinit { }
      }
      """
    )

    assertParse(
      "1️⃣fi true { }",
      diagnostics: [DiagnosticSpec(message: "did you mean to use `if` keyword?", fixIts: ["replace `fi` with `if`"])],
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

    assertParse(
      """
      switch char {
      case "a":
          1️⃣brek
      case "b":
          2️⃣fallthough
      case "c":
          3️⃣retur
      case "d":
          4️⃣contnue
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "did you mean to use `break` keyword?",
          fixIts: ["replace `brek` with `break`"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "did you mean to use `fallthrough` keyword?",
          fixIts: ["replace `fallthough` with `fallthrough`"]
        ),
        DiagnosticSpec(
          locationMarker: "3️⃣",
          message: "did you mean to use `return` keyword?",
          fixIts: ["replace `retur` with `return`"]
        ),
        DiagnosticSpec(
          locationMarker: "4️⃣",
          message: "did you mean to use `continue` keyword?",
          fixIts: ["replace `contnue` with `continue`"]
        ),
      ],
      fixedSource: """
        switch char {
        case "a":
            break
        case "b":
            fallthrough
        case "c":
            return
        case "d":
            continue
        }
        """
    )

    assertParse(
      """
      switch char {
      case "a": break
      case "b": 1️⃣contnue
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "did you mean to use `continue` keyword?",
          fixIts: ["replace `contnue` with `continue`"]
        )
      ],
      fixedSource: """
        switch char {
        case "a": break
        case "b": continue
        }
        """
    )

    assertParse(
      "1️⃣fr _ in (1...5) { }",
      diagnostics: [
        DiagnosticSpec(message: "did you mean to use `for` keyword?", fixIts: ["replace `fr` with `for`"])
      ],
      fixedSource: "for _ in (1...5) { }"
    )

    assertParse(
      "1️⃣thrw SomeError()",
      diagnostics: [
        DiagnosticSpec(message: "did you mean to use `throw` keyword?", fixIts: ["replace `thrw` with `throw`"])
      ],
      fixedSource: "throw SomeError()"
    )
  }

  func testDeclaration() {
    assertParse(
      "1️⃣strct S { }",
      diagnostics: [
        DiagnosticSpec(message: "did you mean to use `struct` keyword?", fixIts: ["replace `strct` with `struct`"])
      ],
      fixedSource: "struct S { }"
    )

    assertParse(
      "1️⃣clas C { }",
      diagnostics: [
        DiagnosticSpec(message: "did you mean to use `class` keyword?", fixIts: ["replace `clas` with `class`"])
      ],
      fixedSource: "class C { }"
    )

    assertParse(
      "1️⃣enume E { }",
      diagnostics: [
        DiagnosticSpec(message: "did you mean to use `enum` keyword?", fixIts: ["replace `enume` with `enum`"])
      ],
      fixedSource: "enum E { }"
    )

    assertParse(
      "1️⃣ctor A { }",
      diagnostics: [
        DiagnosticSpec(message: "did you mean to use `actor` keyword?", fixIts: ["replace `ctor` with `actor`"])
      ],
      fixedSource: "actor A { }"
    )

    assertParse(
      """
      1️⃣protocl P {
        2️⃣associatedType T
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "did you mean to use `protocol` keyword?",
          fixIts: ["replace `protocl` with `protocol`"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "did you mean to use `associatedtype` keyword?",
          fixIts: ["replace `associatedType` with `associatedtype`"]
        ),
      ],
      fixedSource: """
        protocol P {
          associatedtype T
        }
        """
    )

    assertParse(
      "1️⃣fun f() { }",
      diagnostics: [
        DiagnosticSpec(message: "did you mean to use `func` keyword?", fixIts: ["replace `fun` with `func`"])
      ],
      fixedSource: "func f() { }"
    )

    assertParse(
      "1️⃣extnsion T { }",
      diagnostics: [
        DiagnosticSpec(
          message: "did you mean to use `extension` keyword?",
          fixIts: ["replace `extnsion` with `extension`"]
        )
      ],
      fixedSource: "extension T { }"
    )

    assertParse(
      "1️⃣typealia T = String",
      diagnostics: [
        DiagnosticSpec(
          message: "did you mean to use `typealias` keyword?",
          fixIts: ["replace `typealia` with `typealias`"]
        )
      ],
      fixedSource: "typealias T = String"
    )

    assertParse(
      "1️⃣macr OptionSet<RawType>() = #externalMacro(module: \"SwiftMacros\", type: \"OptionSetMacro\")",
      diagnostics: [
        DiagnosticSpec(
          message: "did you mean to use `macro` keyword?",
          fixIts: ["replace `macr` with `macro`"]
        )
      ],
      fixedSource: "macro OptionSet<RawType>() = #externalMacro(module: \"SwiftMacros\", type: \"OptionSetMacro\")"
    )
  }

  func testSwitchCase() {
    assertParse(
      """
      switch character {
      1️⃣cas "a":
          break
      2️⃣defalt:
          break
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "did you mean to use `case` keyword?",
          fixIts: ["replace `cas` with `case`"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "did you mean to use `default` keyword?",
          fixIts: ["replace `defalt` with `default`"]
        ),
      ],
      fixedSource: """
        switch character {
        case "a":
            break
        default:
            break
        }
        """
    )
  }

  func testDeclarationModifier() {
    assertParse(
      "1️⃣lazzy var foo = 0",
      diagnostics: [
        DiagnosticSpec(
          message: "did you mean to use `lazy` keyword?",
          fixIts: ["replace `lazzy` with `lazy`"]
        )
      ],
      fixedSource: "lazy var foo = 0"
    )

    assertParse(
      "1️⃣week var foo: T?",
      diagnostics: [
        DiagnosticSpec(
          message: "did you mean to use `weak` keyword?",
          fixIts: ["replace `week` with `weak`"]
        )
      ],
      fixedSource: "weak var foo: T?"
    )

    assertParse(
      "1️⃣unwned var foo: T?",
      diagnostics: [
        DiagnosticSpec(
          message: "did you mean to use `unowned` keyword?",
          fixIts: ["replace `unwned` with `unowned`"]
        )
      ],
      fixedSource: "unowned var foo: T?"
    )

    assertParse(
      "1️⃣mutatin func foo() { }",
      diagnostics: [
        DiagnosticSpec(
          message: "did you mean to use `mutating` keyword?",
          fixIts: ["replace `mutatin` with `mutating`"]
        )
      ],
      fixedSource: "mutating func foo() { }"
    )
  }

  func testExpressionModifier() {
    assertParse(
      "let _ = 1️⃣tri foo()",
      diagnostics: [
        DiagnosticSpec(
          message: "did you mean to use `try` keyword?",
          fixIts: ["replace `tri` with `try`"]
        )
      ],
      fixedSource: "let _ = try foo()"
    )

    assertParse(
      "let _ = 1️⃣awat foo()",
      diagnostics: [
        DiagnosticSpec(
          message: "did you mean to use `await` keyword?",
          fixIts: ["replace `awat` with `await`"]
        )
      ],
      fixedSource: "let _ = await foo()"
    )

    assertParse(
      "let y = 1️⃣consme x",
      diagnostics: [
        DiagnosticSpec(
          message: "did you mean to use `consume` keyword?",
          fixIts: ["replace `consme` with `consume`"]
        )
      ],
      fixedSource: "let y = consume x"
    )

    assertParse(
      """
      for (left, right) in repeat (1️⃣eachh lhs, each rhs) {
        guard left == right else { return false }
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          message: "did you mean to use `each` keyword?",
          fixIts: ["replace `eachh` with `each`"]
        )
      ],
      fixedSource: """
        for (left, right) in repeat (each lhs, each rhs) {
          guard left == right else { return false }
        }
        """
    )
  }

  func testEachType() {
    assertParse(
      """
      func f<1️⃣eac T: Collection>(_ item: repeat 2️⃣ech T) -> (repeat (each T).Element?) { }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "did you mean to use `each` keyword?",
          fixIts: ["replace `eac` with `each`"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "did you mean to use `each` keyword?",
          fixIts: ["replace `ech` with `each`"]
        ),
      ],
      fixedSource: """
        func f<each T: Collection>(_ item: repeat each T) -> (repeat (each T).Element?) { }
        """
    )
  }

  func testSomeOrAnyType() {
    assertParse(
      "let foo: 1️⃣som T",
      diagnostics: [
        DiagnosticSpec(
          message: "did you mean to use `some` keyword?",
          fixIts: ["replace `som` with `some`"]
        )
      ],
      fixedSource: "let foo: some T"
    )

    assertParse(
      "let foo: 1️⃣ani T",
      diagnostics: [
        DiagnosticSpec(
          message: "did you mean to use `any` keyword?",
          fixIts: ["replace `ani` with `any`"]
        )
      ],
      fixedSource: "let foo: any T"
    )
  }

  func testSpecifierOptions() {
    assertParse(
      "func foo(_: 1️⃣inot Foo) { }",
      diagnostics: [
        DiagnosticSpec(
          message: "did you mean to use `inout` keyword?",
          fixIts: ["replace `inot` with `inout`"]
        )
      ],
      fixedSource: "func foo(_: inout Foo) { }"
    )

    assertParse(
      "func foo(_: 1️⃣isolate Foo) { }",
      diagnostics: [
        DiagnosticSpec(
          message: "did you mean to use `isolated` keyword?",
          fixIts: ["replace `isolate` with `isolated`"]
        )
      ],
      fixedSource: "func foo(_: isolated Foo) { }"
    )

    assertParse(
      "func foo(_: 1️⃣borowing Foo) { }",
      diagnostics: [
        DiagnosticSpec(
          message: "did you mean to use `borrowing` keyword?",
          fixIts: ["replace `borowing` with `borrowing`"]
        )
      ],
      fixedSource: "func foo(_: borrowing Foo) { }"
    )

    assertParse(
      "func foo(_: 1️⃣consumin Foo) { }",
      diagnostics: [
        DiagnosticSpec(
          message: "did you mean to use `consuming` keyword?",
          fixIts: ["replace `consumin` with `consuming`"]
        )
      ],
      fixedSource: "func foo(_: consuming Foo) { }"
    )
  }

  func testAccessorSpecifier() {
    assertParse(
      """
      var foo: Int = 0 {
        1️⃣didset { }
        2️⃣willset { }
      }
      """,
      diagnostics: [
        DiagnosticSpec(
          locationMarker: "1️⃣",
          message: "did you mean to use `didSet` keyword?",
          fixIts: ["replace `didset` with `didSet`"]
        ),
        DiagnosticSpec(
          locationMarker: "2️⃣",
          message: "did you mean to use `willSet` keyword?",
          fixIts: ["replace `willset` with `willSet`"]
        ),
      ],
      fixedSource: """
        var foo: Int = 0 {
          didSet { }
          willSet { }
        }
        """
    )
  }
}
