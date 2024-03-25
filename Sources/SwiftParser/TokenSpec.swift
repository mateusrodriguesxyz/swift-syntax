//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#if swift(>=6)
@_spi(RawSyntax) public import SwiftSyntax
#else
@_spi(RawSyntax) import SwiftSyntax
#endif

extension Keyword {

  @_spi(RawSyntax)
  public enum Match {
    case exactly
    case misspelled([SyntaxText])
  }

  @_spi(RawSyntax)
  public init?(misspelling: SyntaxText, keywords: [SyntaxText] = []) {
    //    precondition(!keywords.isEmpty)

    if let keyword = Keyword(misspelling) {
      self = keyword
      return
    }

    if let _keyword = keyword(for: misspelling, candidates: keywords) {
      self = _keyword
    } else {
      return nil
    }
  }

  @_spi(RawSyntax)
  public init?(misspelling: SyntaxText, keyword: Keyword?) {
    guard let keyword else { return nil }
    self.init(misspelling: misspelling, keywords: [keyword.defaultText])
  }

}

fileprivate func keyword(for misspelling: SyntaxText, candidates: [SyntaxText]) -> Keyword? {

  guard misspelling.count > 1 else { return nil }

  let _misspelling = misspelling.description

  var match: (keyword: SyntaxText?, distance: Int) = (nil, .max)

  print("🤔:", _misspelling)

  let candidates = candidates.filter {
    abs($0.count - misspelling.count) < 2
  }

  for keyword in candidates {
    if let distance = _misspelling.distance2(to: keyword.description, threshold: match.distance), distance < match.distance {
      match.keyword = keyword
      match.distance = distance
      if distance == 1 {
        break
      }
    }
  }

  if let keyword = match.keyword {
    let score = 1 - Double(match.distance) / Double(max(_misspelling.count, keyword.count))
    if score >= 0.5 {
      return Keyword(keyword)!
    }
  }

  return nil

}

extension String {

  func similarity(to other: String) -> Double {
    if let distance = self.distance2(to: other) {
      return 1 - Double(distance) / Double(max(self.count, other.count))
    }
    return 0.0
  }

  func distance2(to other: String, threshold: Int? = nil) -> Int? {

    let selfLength = self.count
    let otherLength = other.count
      
      var commonPrefixLength = 0
      for (char1, char2) in zip(self, other) {
          if char1 == char2 {
              commonPrefixLength += 1
          } else {
              break
          }
      }

    var da: [Character: Int] = [:]

    var d = Array(repeating: Array(repeating: 0, count: otherLength + 2), count: selfLength + 2)

    let maxdist = selfLength + otherLength

    d[0][0] = maxdist

    for i in 1...selfLength + 1 {
      d[i][0] = maxdist
      d[i][1] = i - 1
    }

    for j in 1...otherLength + 1 {
      d[0][j] = maxdist
      d[1][j] = j - 1
    }

    for i in 2...selfLength + 1 {
      var db = 1

      for j in 2...otherLength + 1 {
        let k = da[other[j - 2]!] ?? 1
        let l = db

        var cost = 1
        if self[i - 2] == other[j - 2] {
          cost = 0
          db = j
        }

        let substition = d[i - 1][j - 1] + cost
        let insertion = d[i][j - 1] + 1
        let deletion = d[i - 1][j] + 1
        let transposition = d[k - 1][l - 1] + (i - k - 1) + 1 + (j - l - 1)

        d[i][j] = Swift.min(
          substition,
          insertion,
          deletion,
          transposition
        )
      }

      da[self[i - 2]!] = i
    }

    return d[selfLength + 1][otherLength + 1]
  }

}

extension String {
  func index(_ i: Int) -> String.Index {
    if i >= 0 {
      return self.index(self.startIndex, offsetBy: i)
    } else {
      return self.index(self.endIndex, offsetBy: i)
    }
  }

  subscript(i: Int) -> Character? {
    if i >= count || i < -count {
      return nil
    }

    return self[index(i)]
  }

  subscript(r: Range<Int>) -> String {
    return String(self[index(r.lowerBound)..<index(r.upperBound)])
  }
}

/// Pre-computes the keyword a lexeme might represent. This makes matching
/// a lexeme that has been converted into `PrepareForKeyword` match cheaper to
/// match against multiple ``TokenSpec`` that assume a keyword.
struct PrepareForKeywordMatch {
  /// The kind of the lexeme.
  fileprivate let rawTokenKind: RawTokenKind

  /// If the lexeme has the same text as a keyword, that keyword, otherwise `nil`.
  fileprivate let keyword: Keyword?

  /// Whether to lexeme occurred at the start of a line.
  fileprivate let isAtStartOfLine: Bool

  @inline(__always)
  init(_ lexeme: Lexer.Lexeme, next: Lexer.Lexeme? = nil, match: Keyword.Match = .exactly) {
    self.rawTokenKind = lexeme.rawTokenKind
    switch lexeme.rawTokenKind {
    case .keyword:
      keyword = Keyword(lexeme.tokenText)
    case .identifier:
      if case .misspelled(let keywords) = match, let next, lexeme.diagnostic == nil {
        func buildKeyword() -> Keyword? {
          if lexeme.trailingTriviaText.isEmpty {
            return Keyword(lexeme.tokenText)
          }
          if lexeme.cursor.is(offset: lexeme.leadingTriviaByteLength, at: "$") {
            return Keyword(lexeme.tokenText)
          }
          let excluded: Set<SyntaxText> = ["(", ")", "{", "}", "<", ">", ".", ":", "=", "==", "!=", "?", "!", "??"]
          if excluded.contains(next.tokenText) || next.rawTokenKind == .endOfFile {
            return Keyword(lexeme.tokenText)
          }
          return Keyword(misspelling: lexeme.tokenText, keywords: keywords)
        }
        keyword = buildKeyword()
      } else {
        keyword = Keyword(lexeme.tokenText)
      }
    default:
      keyword = nil
    }
    self.isAtStartOfLine = lexeme.isAtStartOfLine
  }
}

/// Describes a token that should be consumed by the parser.
///
/// All the methods in here and all functions that take a ``TokenSpec`` need to be
/// marked `@inline(__always)` so the compiler inlines the ``RawTokenKind`` we are
/// matching against and is thus able to rule out one of the branches in
/// `matches(rawTokenKind:text:)` based on the matched kind.
@_spi(AlternateTokenIntrospection)
public struct TokenSpec {
  /// The kind we expect the token that we want to consume to have.
  /// This can be a keyword, in which case the ``TokenSpec`` will also match an
  /// identifier with the same text as the keyword and remap it to that keyword
  /// when consumed.
  ///
  /// `fileprivate` because only functions in this file should access it since
  /// they know how to handle the identifier -> keyword remapping.
  fileprivate let rawTokenKind: RawTokenKind

  /// If `rawTokenKind` is `keyword`, the keyword we are expecting. For all other
  /// values of `rawTokenKind`, this is `nil`.
  fileprivate let keyword: Keyword?

  /// If not nil, the token will be remapped to the provided kind when consumed.
  ///
  /// `fileprivate` because only functions in this file should access it since
  /// they know how to handle the identifier -> keyword remapping.
  fileprivate let remapping: RawTokenKind?

  /// The recovery precedence that should be used when consuming this token. By
  /// default this is the token precedence of `rawTokenKind` but it can be
  /// overridden.
  let recoveryPrecedence: TokenPrecedence

  /// Whether the token is allowed to be at the start of a line. Defaults to
  /// `true` but can be set to `false` to consume a token for recovery purposes
  /// that is not allowed to start a new line.
  let allowAtStartOfLine: Bool

  @inline(__always)
  init(
    _ rawTokenKind: RawTokenKind,
    remapping: RawTokenKind? = nil,
    recoveryPrecedence: TokenPrecedence? = nil,
    allowAtStartOfLine: Bool = true
  ) {
    precondition(
      rawTokenKind != .keyword,
      "To create a TokenSpec for a keyword use the initializer that takes a keyword"
    )
    self.rawTokenKind = rawTokenKind
    self.keyword = nil
    self.remapping = remapping
    self.recoveryPrecedence = recoveryPrecedence ?? TokenPrecedence(nonKeyword: rawTokenKind)
    self.allowAtStartOfLine = allowAtStartOfLine
  }

  @inline(__always)
  init(
    _ keyword: Keyword,
    remapping: RawTokenKind? = nil,
    recoveryPrecedence: TokenPrecedence? = nil,
    allowAtStartOfLine: Bool = true
  ) {
    self.rawTokenKind = .keyword
    self.keyword = keyword
    self.remapping = remapping
    self.recoveryPrecedence = recoveryPrecedence ?? TokenPrecedence(keyword)
    self.allowAtStartOfLine = allowAtStartOfLine
  }

  @inline(__always)
  func matches(
    rawTokenKind: RawTokenKind,
    keyword: @autoclosure () -> Keyword?,
    atStartOfLine: @autoclosure () -> Bool
  ) -> Bool {
    if !allowAtStartOfLine && atStartOfLine() {
      return false
    }
    if self.rawTokenKind == .keyword {
      precondition(self.keyword != nil)
      switch rawTokenKind {
      case .keyword, .identifier:
        return keyword() == self.keyword
      default:
        return false
      }
    } else {
      return rawTokenKind == self.rawTokenKind
    }
  }

  @inline(__always)
  static func ~= (kind: TokenSpec, lexeme: Lexer.Lexeme) -> Bool {
    let keyword: Keyword? =
      if case let .misspelledKeyword(k) = lexeme.diagnostic?.kind {
        k
      } else {
        Keyword(lexeme.tokenText)
      }
    return kind.matches(
      rawTokenKind: lexeme.rawTokenKind,
      //      keyword: kind.rawTokenKind == .keyword && (lexeme.rawTokenKind == .keyword) ? Keyword(misspelling: lexeme.tokenText, keyword: kind.keyword) : Keyword(lexeme.tokenText),
      //      keyword: Keyword(lexeme.tokenText),
      keyword: keyword,
      atStartOfLine: lexeme.isAtStartOfLine
    )

  }

  @inline(__always)
  static func ~= (kind: TokenSpec, token: TokenSyntax) -> Bool {
    return kind.matches(
      rawTokenKind: token.tokenView.rawKind,
      keyword: Keyword(token.tokenView.rawText),
      atStartOfLine: token.leadingTrivia.contains(where: { $0.isNewline })
    )
  }

  @inline(__always)
  static func ~= (kind: TokenSpec, token: RawTokenSyntax) -> Bool {
    return kind.matches(
      rawTokenKind: token.tokenKind,
      keyword: Keyword(token.tokenView.rawText),
      atStartOfLine: token.leadingTriviaPieces.contains(where: \.isNewline)
    )
  }

  @inline(__always)
  static func ~= (kind: TokenSpec, lexeme: PrepareForKeywordMatch) -> Bool {
    return kind.matches(
      rawTokenKind: lexeme.rawTokenKind,
      keyword: lexeme.keyword,
      atStartOfLine: lexeme.isAtStartOfLine
    )
  }

  /// Returns a ``TokenKind`` that will most likely be parsed as a token that
  /// matches this ``TokenSpec``.
  ///
  /// IMPORTANT: Should only be used when generating tokens during the
  /// modification of test cases. This should never be used in the parser itself.
  @_spi(AlternateTokenIntrospection)
  public var synthesizedTokenKind: TokenKind {
    switch rawTokenKind {
    case .binaryOperator: return .binaryOperator("+")
    case .dollarIdentifier: return .dollarIdentifier("$0")
    case .floatLiteral: return .floatLiteral("1.0")
    case .identifier: return .identifier("myIdent")
    case .integerLiteral: return .integerLiteral("1")
    case .keyword: return .keyword(keyword!)
    case .postfixOperator: return .postfixOperator("++")
    case .prefixOperator: return .prefixOperator("!")
    case .rawStringPoundDelimiter: return .rawStringPoundDelimiter("#")
    case .regexLiteralPattern: return .regexLiteralPattern(".*")
    case .regexPoundDelimiter: return .regexPoundDelimiter("#")
    case .stringSegment: return .stringSegment("abc")
    default: return TokenKind.fromRaw(kind: rawTokenKind, text: "")
    }
  }
}

extension TokenConsumer {
  /// Generates a missing token that has the expected kind of `spec`.
  @inline(__always)
  mutating func missingToken(_ spec: TokenSpec) -> Token {
    return missingToken(
      spec.remapping ?? spec.rawTokenKind,
      text: spec.keyword?.defaultText ?? spec.rawTokenKind.defaultText
    )
  }

  /// Asserts that the current token matches `spec` and consumes it, performing
  /// any necessary token kind remapping.
  ///
  /// This should only be called from parsing primitives like `consume(if:)` and
  /// `eat`. Introduce new users of this very sparingly.
  @inline(__always)
  mutating func eat(_ spec: TokenSpec) -> Token {
    precondition(spec ~= self.currentToken)
    if let remapping = spec.remapping {
      return self.consumeAnyToken(remapping: remapping)
    } else if spec.rawTokenKind == .keyword {
      //        if let defaultText = spec.keyword?.defaultText, defaultText != self.currentToken.tokenText {
      //            let keyword = Keyword(misspelling: self.currentToken.tokenText, keywords: [defaultText])
      //            if let keyword, keyword == spec.keyword {
      //                let diagnostic = self.currentToken.diagnostic
      //                self.currentToken.diagnostic = TokenDiagnostic(
      //                    combining: diagnostic,
      //                    TokenDiagnostic(.misspelledKeyword(keyword), byteOffset: self.currentToken.trailingTriviaByteLength)
      //                )
      //            }
      //        }
      return self.consumeAnyToken(remapping: .keyword)
    } else {
      return self.consumeAnyToken()
    }
  }
}
