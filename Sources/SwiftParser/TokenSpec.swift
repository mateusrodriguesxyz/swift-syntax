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

    static func misspelled<T: TokenSpecSet>(_: T.Type) -> Self where T: RawRepresentable, T.RawValue == SyntaxText {
      return .misspelled(T.allCases.map(\.rawValue))
    }

    static func misspelled(_ keywords: [Keyword]) -> Self {
      return .misspelled(keywords.map(\.defaultText))
    }
  }

  @_spi(RawSyntax)
  public init?(misspelling: SyntaxText, keywords: [SyntaxText] = []) {
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
  var match: (keyword: SyntaxText?, distance: Int) = (nil, .max)
  // A possible misspelling is only matched against keywords which length diff is 2 or less characters
  let candidates = candidates.filter {
    abs($0.count - misspelling.count) < 2
  }
  for keyword in candidates {
    if let distance = misspelling.distance(to: keyword, threshold: match.distance), distance < match.distance {
      match.keyword = keyword
      match.distance = distance
      // If distance is 1 there's not a better match ahead.
      if distance == 1 {
        break
      }
    }
  }
  // // Distance threshold is 3 to prevent longer keywords with too many misspellings.
  if let keyword = match.keyword, match.distance < 2 {
    let score = 1 - Double(match.distance) / Double(max(misspelling.count, keyword.count))
    // Similarity threshold is 50% if keyword has 2 character and 60% otherwise.
    let threshold = keyword.count == 2 ? 0.5 : 0.6
    if score >= threshold {
      return Keyword(keyword)
    }
  }
  return nil
}

extension SyntaxText {
  // Damerau–Levenshtein distance: https://en.wikipedia.org/wiki/Damerau–Levenshtein_distance
  func distance(to other: SyntaxText, threshold: Int? = nil) -> Int? {

    let selfLength = self.count
    let otherLength = other.count

    var da: [UInt8: Int] = [:]

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
        let k = da[other[j - 2]] ?? 1
        let l = db

        var cost: Int

        if self[i - 2] == other[j - 2] {
          cost = 0
          db = j
        } else {
          cost = 1
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

      da[self[i - 2]] = i
    }

    return d[selfLength + 1][otherLength + 1]
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
      if case .misspelled(let keywords) = match, !keywords.isEmpty, let next,
        let keyword = parseMisspelledKeyword(lexeme, next: next, keywords: keywords)
      {
        self.keyword = keyword
      } else {
        keyword = Keyword(lexeme.tokenText)
      }
    default:
      keyword = nil
    }

    self.isAtStartOfLine = lexeme.isAtStartOfLine
  }
}

func parseMisspelledKeyword(_ lexeme: Lexer.Lexeme, next: Lexer.Lexeme, keywords: [SyntaxText]) -> Keyword? {
  guard tokenIsPossibleMisspelledKeyword(lexeme, next: next) else { return nil }
  let keywords = filter(keywords, lexeme: lexeme, next: next)
  guard let keyword = Keyword(misspelling: lexeme.tokenText, keywords: keywords) else { return nil }
  guard validate(keyword: keyword, lexeme: lexeme, next: next) else { return nil }
  return keyword
}

func filter(_ keywords: [SyntaxText], lexeme: Lexer.Lexeme, next: Lexer.Lexeme) -> [SyntaxText] {
  keywords.filter {
    switch Keyword($0) {
    case .`init`:
      return lexeme.isAtStartOfLine && next.rawTokenKind == .leftParen
    case .`deinit`:
      return lexeme.isAtStartOfLine && next.rawTokenKind == .leftBrace
    default:
      return true
    }
  }
}

/// Checks whether a lexeme is a possible misspelled keyword ensuring that a lexeme that is very likely not intended to be a keyword is not false parsed as one.
fileprivate func tokenIsPossibleMisspelledKeyword(_ lexeme: Lexer.Lexeme, next: Lexer.Lexeme) -> Bool {
  // Keyword should have at least 1 character
  guard lexeme.tokenText.count > 1 else {
    return false
  }
  if lexeme.cursor.previousKeyword == .func {
    return false
  }
  // If next token is not `:` or at end of line it should have a trailing space
  if next.rawTokenKind != .colon, next.rawTokenKind != .leftParen,  lexeme.cursor.previousTokenKind != .period, !next.isAtStartOfLine,
    lexeme.trailingTriviaText.isEmpty
  {
    return false
  }
  // If lexeme first character is `$`, `#`or `_` is not a keyword
  if lexeme.cursor.is(offset: lexeme.leadingTriviaByteLength, at: "$", "#", "_") {
    return false
  }
  // Keywords are not usually preceded by `.`, `,` or `=`
  // Exceptions are `self` (e.g: T.self) and `if` or `switch` as expressions (e.g. = if CONDITION { } else { })
  let excludedPreviousTokenKinds: Set<RawTokenKind> = [.comma]
  if let previousTokenKind = lexeme.cursor.previousTokenKind, excludedPreviousTokenKinds.contains(previousTokenKind) {
    return false
  }
  // Keywords are not usually followed by the following token kinds
  let excludedNextTokenKinds: Set<RawTokenKind> = [
    /*.leftParen,*/ .rightAngle, /*.leftBrace,*/ /*.rightBrace, */ .leftAngle, .rightAngle, .leftSquare, .rightSquare,
    .equal,
    .period, /*.colon,*/ .infixQuestionMark, .postfixQuestionMark, .exclamationMark, .binaryOperator, .endOfFile,
  ]
  if excludedNextTokenKinds.contains(next.rawTokenKind) {
    return false
  }
  // Keywords are not usually followed by the following keywords
  let excludedNextKeywords: Set<Keyword> = [.as, .is, .else, .where, .async, .throws, .rethrows]
  if next.isLexerClassifiedKeyword, excludedNextKeywords.contains(Keyword(next.tokenText)!) {
    return false
  }
  return true
}

fileprivate func validate(keyword: Keyword, lexeme: Lexer.Lexeme, next: Lexer.Lexeme) -> Bool {
  func validatePreviousToken() -> Bool {
    switch lexeme.cursor.previousTokenKind {
    case .equal:
      // A misspelled keyword preceded by `=` is only acceptable if it's modifier
      let acceptables: Set<Keyword> = [.try, .await, .consume]
      return acceptables.contains(keyword)
    case .period:
      // A misspelled keyword preceded by `.` is only acceptable if it's `self`
      return keyword == .`self`
    default:
      return true
    }
  }

  func validateNextToken() -> Bool {
    switch next.rawTokenKind {
    // A misspelled keyword followed by `}` is only acceptable if it's a control transfer statement at start of line or preceded by `:`.
    case .rightBrace:
      let acceptables: Set<Keyword> = [.return, .break, .continue, .fallthrough]
      return acceptables.contains(keyword) && (lexeme.isAtStartOfLine || lexeme.cursor.previousTokenKind == .colon)
    case .leftParen:
      return keyword == .`init`
    // A misspelled keyword followed by `:` is only acceptable if it's a `default` at start of line.
    case .colon:
      return keyword == .default && lexeme.isAtStartOfLine
    case .leftBrace:
      let acceptables: Set<Keyword> = [.async, .throws, .didSet, .willSet, .deinit]
      return acceptables.contains(keyword)
    default:
      return true
    }
  }

  return validatePreviousToken() && validateNextToken()
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
      if case let .misspelledKeyword(keyword) = lexeme.diagnostic?.kind {
        keyword
      } else {
        Keyword(lexeme.tokenText)
      }
    return kind.matches(
      rawTokenKind: lexeme.rawTokenKind,
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
    if case let .keyword(keyword) = spec.synthesizedTokenKind, currentToken.tokenText != keyword.defaultText {
      self.addMisspelledKeywordDiagnosticToCurrentToken(keyword)
    }
    //    precondition(spec ~= self.currentToken)
    if let remapping = spec.remapping {
      return self.consumeAnyToken(remapping: remapping)
    } else if spec.rawTokenKind == .keyword {
      return self.consumeAnyToken(remapping: .keyword)
    } else {
      return self.consumeAnyToken()
    }
  }
}
