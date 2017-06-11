import java.util.regex.Matcher
import java.util.regex.Pattern

class Lexer {

    val generalPattern by lazy { initGeneralPattern() }
    val interpolationPattern by lazy { initInterpolationPattern() }

    val tokens = mutableListOf<BaseToken>()

    val fa = FiniteAutomata()

    private fun initInterpolationPattern(): Pattern {
        val patternBuilder = StringBuilder()

        InterpolationPatterns.values().forEach {
            patternBuilder.append(String.format("|(?<%s>%s)", it.name, it.pattern))
        }
        return Pattern.compile(patternBuilder.substring(1).toString(), Pattern.UNICODE_CHARACTER_CLASS)
    }

    private fun initGeneralPattern(): Pattern {
        val patternBuilder = StringBuilder()

        TokenType.values().forEach {
            patternBuilder.append(String.format("|(?<%s>%s)", it.name, it.pattern))
        }
        return Pattern.compile(patternBuilder.substring(1).toString(), Pattern.UNICODE_CHARACTER_CLASS)
    }


    fun processInput(input: String) {
        runRegex(input, null)
        //runFA(input)

        for (token in tokens) {
            println(token)
        }
    }

    private fun runFA(flattedInput: String) {
        fa.runFA(flattedInput)
    }

    private fun runRegex(flattedInput: String, parentStringToken: StringToken?) {
        val matcher = generalPattern.matcher(flattedInput)

        while (matcher.find()) {
            if (matcher.group(TokenType.BLOCKCOMMENT.name) != null) {
                println("Log: ${constructToken(matcher, TokenType.BLOCKCOMMENT, flattedInput)}")
            } else if (matcher.group(TokenType.SINGLELINECOMMENT.name) != null) {
                println("Log: ${constructToken(matcher, TokenType.SINGLELINECOMMENT, flattedInput)}")
            } else if (matcher.group(TokenType.BINARY.name) != null) {
                addToken(constructToken(matcher, TokenType.BINARY, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.FLOAT.name) != null) {
                addToken(constructToken(matcher, TokenType.FLOAT, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.LONG.name) != null) {
                addToken(constructToken(matcher, TokenType.LONG, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.DOUBLE.name) != null) {
                addToken(constructToken(matcher, TokenType.DOUBLE, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.WHITESPACE.name) != null) {
                continue
            } else if (matcher.group(TokenType.INTEGRAL.name) != null) {
                addToken(constructToken(matcher, TokenType.INTEGRAL, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.TYPEALIAS.name) != null) {
                addToken(constructToken(matcher, TokenType.TYPEALIAS, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.INTERFACE.name) != null) {
                addToken(constructToken(matcher, TokenType.INTERFACE, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.CONTINUE.name) != null) {
                addToken(constructToken(matcher, TokenType.CONTINUE, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.PACKAGE.name) != null) {
                addToken(constructToken(matcher, TokenType.PACKAGE, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.IMPORT.name) != null) {
                addToken(constructToken(matcher, TokenType.IMPORT, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.RETURN.name) != null) {
                addToken(constructToken(matcher, TokenType.RETURN, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.OBJECT.name) != null) {
                addToken(constructToken(matcher, TokenType.OBJECT, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.WHILE.name) != null) {
                addToken(constructToken(matcher, TokenType.WHILE, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.BREAK.name) != null) {
                addToken(constructToken(matcher, TokenType.BREAK, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.CLASS.name) != null) {
                addToken(constructToken(matcher, TokenType.CLASS, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.THROW.name) != null) {
                addToken(constructToken(matcher, TokenType.THROW, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.FALSE.name) != null) {
                addToken(constructToken(matcher, TokenType.FALSE, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.SUPER.name) != null) {
                addToken(constructToken(matcher, TokenType.SUPER, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.TYPEOF.name) != null) {
                addToken(constructToken(matcher, TokenType.TYPEOF, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.WHEN.name) != null) {
                addToken(constructToken(matcher, TokenType.WHEN, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.TRUE.name) != null) {
                addToken(constructToken(matcher, TokenType.TRUE, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.THIS.name) != null) {
                addToken(constructToken(matcher, TokenType.THIS, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.NULL.name) != null) {
                addToken(constructToken(matcher, TokenType.NULL, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.ELSE.name) != null) {
                addToken(constructToken(matcher, TokenType.ELSE, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.TRY.name) != null) {
                addToken(constructToken(matcher, TokenType.TRY, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.VAL.name) != null) {
                addToken(constructToken(matcher, TokenType.VAL, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.VAR.name) != null) {
                addToken(constructToken(matcher, TokenType.VAR, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.FUN.name) != null) {
                addToken(constructToken(matcher, TokenType.FUN, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.FOR.name) != null) {
                addToken(constructToken(matcher, TokenType.FOR, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.IS.name) != null) {
                addToken(constructToken(matcher, TokenType.IS, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.IN.name) != null) {
                addToken(constructToken(matcher, TokenType.IN, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.IF.name) != null) {
                addToken(constructToken(matcher, TokenType.IF, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.DO.name) != null) {
                addToken(constructToken(matcher, TokenType.DO, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.AS.name) != null) {
                addToken(constructToken(matcher, TokenType.AS, flattedInput), parentStringToken)

            } else if (matcher.group(TokenType.RESERVED.name) != null) {
                addToken(constructToken(matcher, TokenType.RESERVED, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.EQEQEQ.name) != null) {
                addToken(constructToken(matcher, TokenType.EQEQEQ, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.EXCLEQEQ.name) != null) {
                addToken(constructToken(matcher, TokenType.EXCLEQEQ, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.NOTIN.name) != null) {
                addToken(constructToken(matcher, TokenType.NOTIN, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.NOTIS.name) != null) {
                addToken(constructToken(matcher, TokenType.NOTIS, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.ASSAFE.name) != null) {
                addToken(constructToken(matcher, TokenType.ASSAFE, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.INCR.name) != null) {
                addToken(constructToken(matcher, TokenType.INCR, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.DECR.name) != null) {
                addToken(constructToken(matcher, TokenType.DECR, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.LTEQ.name) != null) {
                addToken(constructToken(matcher, TokenType.LTEQ, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.GTEQ.name) != null) {
                addToken(constructToken(matcher, TokenType.GTEQ, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.DOUBLEEQ.name) != null) {
                addToken(constructToken(matcher, TokenType.DOUBLEEQ, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.EXCLEQ.name) != null) {
                addToken(constructToken(matcher, TokenType.EXCLEQ, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.ANDAND.name) != null) {
                addToken(constructToken(matcher, TokenType.ANDAND, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.OROR.name) != null) {
                addToken(constructToken(matcher, TokenType.OROR, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.MULTEQ.name) != null) {
                addToken(constructToken(matcher, TokenType.MULTEQ, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.DIVEQ.name) != null) {
                addToken(constructToken(matcher, TokenType.DIVEQ, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.PEREQ.name) != null) {
                addToken(constructToken(matcher, TokenType.PEREQ, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.PLUSEQ.name) != null) {
                addToken(constructToken(matcher, TokenType.PLUSEQ, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.MINUSEQ.name) != null) {
                addToken(constructToken(matcher, TokenType.MINUSEQ, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.ARROW.name) != null) {
                addToken(constructToken(matcher, TokenType.ARROW, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.DOUBLEARROW.name) != null) {
                addToken(constructToken(matcher, TokenType.DOUBLEARROW, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.RANGE.name) != null) {
                addToken(constructToken(matcher, TokenType.RANGE, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.COLONCOLON.name) != null) {
                addToken(constructToken(matcher, TokenType.COLONCOLON, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.LBRACKET.name) != null) {
                addToken(constructToken(matcher, TokenType.LBRACKET, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.RBRACKET.name) != null) {
                addToken(constructToken(matcher, TokenType.RBRACKET, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.LBRACE.name) != null) {
                addToken(constructToken(matcher, TokenType.LBRACE, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.RBRACE.name) != null) {
                addToken(constructToken(matcher, TokenType.RBRACE, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.LPAR.name) != null) {
                addToken(constructToken(matcher, TokenType.LPAR, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.RPAR.name) != null) {
                addToken(constructToken(matcher, TokenType.RPAR, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.DOT.name) != null) {
                addToken(constructToken(matcher, TokenType.DOT, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.MUL.name) != null) {
                addToken(constructToken(matcher, TokenType.MUL, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.PLUS.name) != null) {
                addToken(constructToken(matcher, TokenType.PLUS, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.MINUS.name) != null) {
                addToken(constructToken(matcher, TokenType.MINUS, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.EXCL.name) != null) {
                addToken(constructToken(matcher, TokenType.EXCL, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.DIV.name) != null) {
                addToken(constructToken(matcher, TokenType.DIV, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.PERC.name) != null) {
                addToken(constructToken(matcher, TokenType.PERC, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.LT.name) != null) {
                addToken(constructToken(matcher, TokenType.LT, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.GT.name) != null) {
                addToken(constructToken(matcher, TokenType.GT, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.QUEST.name) != null) {
                addToken(constructToken(matcher, TokenType.QUEST, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.COLON.name) != null) {
                addToken(constructToken(matcher, TokenType.COLON, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.SEMICOLON.name) != null) {
                addToken(constructToken(matcher, TokenType.SEMICOLON, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.EQ.name) != null) {
                addToken(constructToken(matcher, TokenType.EQ, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.COMMA.name) != null) {
                addToken(constructToken(matcher, TokenType.COMMA, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.HASH.name) != null) {
                addToken(constructToken(matcher, TokenType.HASH, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.AT.name) != null) {
                addToken(constructToken(matcher, TokenType.AT, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.IGNORED.name) != null) {
                addToken(constructToken(matcher, TokenType.IGNORED, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.ID.name) != null) {
                addToken(constructToken(matcher, TokenType.ID, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.CHAR.name) != null) {
                addToken(constructToken(matcher, TokenType.CHAR, flattedInput), parentStringToken)
            } else if (matcher.group(TokenType.STR.name) != null) {
                val strToken = StringToken(constructToken(matcher, TokenType.STR, flattedInput), mutableListOf())

                addToken(strToken, parentStringToken)
                recognizeInterpolation(strToken)
            } else if (matcher.group(TokenType.ERROR.name) != null) {
                addToken(constructToken(matcher, TokenType.ERROR, flattedInput), parentStringToken)
            }
        }
    }

    private fun addToken(token: BaseToken, parentStringToken: StringToken?) {
        if (parentStringToken != null) {
            parentStringToken.interpolatedGroups.last().tokens.add(token)
        } else {
            tokens.add(token)
        }
    }

    private fun recognizeInterpolation(strToken: StringToken) {
        val matcher = interpolationPattern.matcher(strToken.token.lexeme)

        while (matcher.find()) {
            if (matcher.group(InterpolationPatterns.INTERPOLATIONCOMPLEX.name) != null) {
                val positionLexemePair = getPositionLexemePair(
                        matcher, InterpolationPatterns.INTERPOLATIONCOMPLEX.name, strToken.token.lexeme
                )
                val ig = InterpolatedGroup(
                        Position(positionLexemePair.first.startIndex + 1, positionLexemePair.first.endIndex - 1),
                        mutableListOf()
                )
                strToken.interpolatedGroups.add(ig)
                runRegex(positionLexemePair.second.substring(2, positionLexemePair.second.length - 1), strToken)

                println("Complex ${positionLexemePair.second} ${positionLexemePair.first}")
            } else if (matcher.group(InterpolationPatterns.INTERPOLATIONSIMPLE.name) != null) {
                val positionLexemePair = getPositionLexemePair(
                        matcher, InterpolationPatterns.INTERPOLATIONSIMPLE.name, strToken.token.lexeme
                )
                val ig = InterpolatedGroup(
                        Position(positionLexemePair.first.startIndex, positionLexemePair.first.endIndex),
                        mutableListOf()
                )
                strToken.interpolatedGroups.add(ig)
                runRegex(positionLexemePair.second.substring(1), strToken)

                println("Simple ${positionLexemePair.second} ${positionLexemePair.first}")
            }
        }
    }

    private fun constructToken(matcher: Matcher, tokenType: TokenType, input: String): Token {
        val positionLexemePair = getPositionLexemePair(matcher, tokenType.name, input)
        return Token(tokenType, positionLexemePair.second, positionLexemePair.first)
    }

    private fun getPositionLexemePair(matcher: Matcher, groupName: String, input: String):
            Pair<Position, String> {
        val position = Position(matcher.start(groupName), matcher.end(groupName))
        val lexeme = input.substring(position.startIndex, position.endIndex)

        return position to lexeme
    }
}