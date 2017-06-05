import java.util.regex.Matcher
import java.util.regex.Pattern

class Lexer {

    val pattern: Pattern

    init {
        val patternBuilder = StringBuilder()

        TokenType.values().forEach {
            patternBuilder.append(String.format("|(?<%s>%s)", it.name, it.pattern))
        }
        pattern = Pattern.compile(patternBuilder.substring(1).toString())
    }

    fun processInput(lines: List<String>) {
        val tokens = mutableListOf<Token>()
        val flattedInput = lines.reduce { acc, s -> acc + s + '\n' }

        val matcher = pattern.matcher(flattedInput)


        while (matcher.find()) {
            if (matcher.group(TokenType.BLOCKCOMMENT.name) != null) {
                println("Log: ${constructToken(matcher, TokenType.BLOCKCOMMENT, flattedInput)}")
            } else if (matcher.group(TokenType.SINGLELINECOMMENT.name) != null) {
                println("Log: ${constructToken(matcher, TokenType.SINGLELINECOMMENT, flattedInput)}")
            } else if (matcher.group(TokenType.DECIMAL.name) != null) {
                tokens.add(constructToken(matcher, TokenType.DECIMAL, flattedInput))
            } else if (matcher.group(TokenType.WHITESPACE.name) != null) {
                continue
            } else if (matcher.group(TokenType.INTEGRAL.name) != null) {
                tokens.add(constructToken(matcher, TokenType.INTEGRAL, flattedInput))
            } else if (matcher.group(TokenType.TYPEALIAS.name) != null) {
                tokens.add(constructToken(matcher, TokenType.TYPEALIAS, flattedInput))
            } else if (matcher.group(TokenType.INTERFACE.name) != null) {
                tokens.add(constructToken(matcher, TokenType.INTERFACE, flattedInput))
            } else if (matcher.group(TokenType.CONTINUE.name) != null) {
                tokens.add(constructToken(matcher, TokenType.CONTINUE, flattedInput))
            } else if (matcher.group(TokenType.PACKAGE.name) != null) {
                tokens.add(constructToken(matcher, TokenType.PACKAGE, flattedInput))
            } else if (matcher.group(TokenType.IMPORT.name) != null) {
                tokens.add(constructToken(matcher, TokenType.IMPORT, flattedInput))
            } else if (matcher.group(TokenType.RETURN.name) != null) {
                tokens.add(constructToken(matcher, TokenType.RETURN, flattedInput))
            } else if (matcher.group(TokenType.OBJECT.name) != null) {
                tokens.add(constructToken(matcher, TokenType.OBJECT, flattedInput))
            } else if (matcher.group(TokenType.WHILE.name) != null) {
                tokens.add(constructToken(matcher, TokenType.WHILE, flattedInput))
            } else if (matcher.group(TokenType.BREAK.name) != null) {
                tokens.add(constructToken(matcher, TokenType.BREAK, flattedInput))
            } else if (matcher.group(TokenType.CLASS.name) != null) {
                tokens.add(constructToken(matcher, TokenType.CLASS, flattedInput))
            } else if (matcher.group(TokenType.THROW.name) != null) {
                tokens.add(constructToken(matcher, TokenType.THROW, flattedInput))
            } else if (matcher.group(TokenType.FALSE.name) != null) {
                tokens.add(constructToken(matcher, TokenType.FALSE, flattedInput))
            } else if (matcher.group(TokenType.SUPER.name) != null) {
                tokens.add(constructToken(matcher, TokenType.SUPER, flattedInput))
            } else if (matcher.group(TokenType.TYPEOF.name) != null) {
                tokens.add(constructToken(matcher, TokenType.TYPEOF, flattedInput))
            } else if (matcher.group(TokenType.WHEN.name) != null) {
                tokens.add(constructToken(matcher, TokenType.WHEN, flattedInput))
            } else if (matcher.group(TokenType.TRUE.name) != null) {
                tokens.add(constructToken(matcher, TokenType.TRUE, flattedInput))
            } else if (matcher.group(TokenType.THIS.name) != null) {
                tokens.add(constructToken(matcher, TokenType.THIS, flattedInput))
            } else if (matcher.group(TokenType.NULL.name) != null) {
                tokens.add(constructToken(matcher, TokenType.NULL, flattedInput))
            } else if (matcher.group(TokenType.ELSE.name) != null) {
                tokens.add(constructToken(matcher, TokenType.ELSE, flattedInput))
            } else if (matcher.group(TokenType.TRY.name) != null) {
                tokens.add(constructToken(matcher, TokenType.TRY, flattedInput))
            } else if (matcher.group(TokenType.VAL.name) != null) {
                tokens.add(constructToken(matcher, TokenType.VAL, flattedInput))
            } else if (matcher.group(TokenType.VAR.name) != null) {
                tokens.add(constructToken(matcher, TokenType.VAR, flattedInput))
            } else if (matcher.group(TokenType.FUN.name) != null) {
                tokens.add(constructToken(matcher, TokenType.FUN, flattedInput))
            } else if (matcher.group(TokenType.FOR.name) != null) {
                tokens.add(constructToken(matcher, TokenType.FOR, flattedInput))
            } else if (matcher.group(TokenType.IS.name) != null) {
                tokens.add(constructToken(matcher, TokenType.IS, flattedInput))
            } else if (matcher.group(TokenType.IN.name) != null) {
                tokens.add(constructToken(matcher, TokenType.IN, flattedInput))
            } else if (matcher.group(TokenType.IF.name) != null) {
                tokens.add(constructToken(matcher, TokenType.IF, flattedInput))
            } else if (matcher.group(TokenType.DO.name) != null) {
                tokens.add(constructToken(matcher, TokenType.DO, flattedInput))
            } else if (matcher.group(TokenType.AS.name) != null) {
                tokens.add(constructToken(matcher, TokenType.AS, flattedInput))

            } else if (matcher.group(TokenType.RESERVED.name) != null) {
                tokens.add(constructToken(matcher, TokenType.RESERVED, flattedInput))
            } else if (matcher.group(TokenType.EQEQEQ.name) != null) {
                tokens.add(constructToken(matcher, TokenType.EQEQEQ, flattedInput))
            } else if (matcher.group(TokenType.EXCLEQEQ.name) != null) {
                tokens.add(constructToken(matcher, TokenType.EXCLEQEQ, flattedInput))
            } else if (matcher.group(TokenType.NOTIN.name) != null) {
                tokens.add(constructToken(matcher, TokenType.NOTIN, flattedInput))
            } else if (matcher.group(TokenType.NOTIS.name) != null) {
                tokens.add(constructToken(matcher, TokenType.NOTIS, flattedInput))
            } else if (matcher.group(TokenType.ASSAFE.name) != null) {
                tokens.add(constructToken(matcher, TokenType.ASSAFE, flattedInput))
            } else if (matcher.group(TokenType.INCR.name) != null) {
                tokens.add(constructToken(matcher, TokenType.INCR, flattedInput))
            } else if (matcher.group(TokenType.DECR.name) != null) {
                tokens.add(constructToken(matcher, TokenType.DECR, flattedInput))
            } else if (matcher.group(TokenType.LTEQ.name) != null) {
                tokens.add(constructToken(matcher, TokenType.LTEQ, flattedInput))
            } else if (matcher.group(TokenType.GTEQ.name) != null) {
                tokens.add(constructToken(matcher, TokenType.GTEQ, flattedInput))
            } else if (matcher.group(TokenType.DOUBLEEQ.name) != null) {
                tokens.add(constructToken(matcher, TokenType.DOUBLEEQ, flattedInput))
            } else if (matcher.group(TokenType.EXCLEQ.name) != null) {
                tokens.add(constructToken(matcher, TokenType.EXCLEQ, flattedInput))
            } else if (matcher.group(TokenType.ANDAND.name) != null) {
                tokens.add(constructToken(matcher, TokenType.ANDAND, flattedInput))
            } else if (matcher.group(TokenType.OROR.name) != null) {
                tokens.add(constructToken(matcher, TokenType.OROR, flattedInput))
            } else if (matcher.group(TokenType.MULTEQ.name) != null) {
                tokens.add(constructToken(matcher, TokenType.MULTEQ, flattedInput))
            } else if (matcher.group(TokenType.DIVEQ.name) != null) {
                tokens.add(constructToken(matcher, TokenType.DIVEQ, flattedInput))
            } else if (matcher.group(TokenType.PEREQ.name) != null) {
                tokens.add(constructToken(matcher, TokenType.PEREQ, flattedInput))
            } else if (matcher.group(TokenType.PLUSEQ.name) != null) {
                tokens.add(constructToken(matcher, TokenType.PLUSEQ, flattedInput))
            } else if (matcher.group(TokenType.MINUSEQ.name) != null) {
                tokens.add(constructToken(matcher, TokenType.MINUSEQ, flattedInput))
            } else if (matcher.group(TokenType.ARROW.name) != null) {
                tokens.add(constructToken(matcher, TokenType.ARROW, flattedInput))
            } else if (matcher.group(TokenType.DOUBLEARROW.name) != null) {
                tokens.add(constructToken(matcher, TokenType.DOUBLEARROW, flattedInput))
            } else if (matcher.group(TokenType.RANGE.name) != null) {
                tokens.add(constructToken(matcher, TokenType.RANGE, flattedInput))
            } else if (matcher.group(TokenType.COLONCOLON.name) != null) {
                tokens.add(constructToken(matcher, TokenType.COLONCOLON, flattedInput))
            } else if (matcher.group(TokenType.LBRACKET.name) != null) {
                tokens.add(constructToken(matcher, TokenType.LBRACKET, flattedInput))
            } else if (matcher.group(TokenType.RBRACKET.name) != null) {
                tokens.add(constructToken(matcher, TokenType.RBRACKET, flattedInput))
            } else if (matcher.group(TokenType.LBRACE.name) != null) {
                tokens.add(constructToken(matcher, TokenType.LBRACE, flattedInput))
            } else if (matcher.group(TokenType.RBRACE.name) != null) {
                tokens.add(constructToken(matcher, TokenType.RBRACE, flattedInput))
            } else if (matcher.group(TokenType.LPAR.name) != null) {
                tokens.add(constructToken(matcher, TokenType.LPAR, flattedInput))
            } else if (matcher.group(TokenType.RPAR.name) != null) {
                tokens.add(constructToken(matcher, TokenType.RPAR, flattedInput))
            } else if (matcher.group(TokenType.DOT.name) != null) {
                tokens.add(constructToken(matcher, TokenType.DOT, flattedInput))
            } else if (matcher.group(TokenType.MUL.name) != null) {
                tokens.add(constructToken(matcher, TokenType.MUL, flattedInput))
            } else if (matcher.group(TokenType.PLUS.name) != null) {
                tokens.add(constructToken(matcher, TokenType.PLUS, flattedInput))
            } else if (matcher.group(TokenType.MINUS.name) != null) {
                tokens.add(constructToken(matcher, TokenType.MINUS, flattedInput))
            } else if (matcher.group(TokenType.EXCL.name) != null) {
                tokens.add(constructToken(matcher, TokenType.EXCL, flattedInput))
            } else if (matcher.group(TokenType.DIV.name) != null) {
                tokens.add(constructToken(matcher, TokenType.DIV, flattedInput))
            } else if (matcher.group(TokenType.PERC.name) != null) {
                tokens.add(constructToken(matcher, TokenType.PERC, flattedInput))
            } else if (matcher.group(TokenType.LT.name) != null) {
                tokens.add(constructToken(matcher, TokenType.LT, flattedInput))
            } else if (matcher.group(TokenType.GT.name) != null) {
                tokens.add(constructToken(matcher, TokenType.GT, flattedInput))
            } else if (matcher.group(TokenType.QUEST.name) != null) {
                tokens.add(constructToken(matcher, TokenType.QUEST, flattedInput))
            } else if (matcher.group(TokenType.COLON.name) != null) {
                tokens.add(constructToken(matcher, TokenType.COLON, flattedInput))
            } else if (matcher.group(TokenType.SEMICOLON.name) != null) {
                tokens.add(constructToken(matcher, TokenType.SEMICOLON, flattedInput))
            } else if (matcher.group(TokenType.EQ.name) != null) {
                tokens.add(constructToken(matcher, TokenType.EQ, flattedInput))
            } else if (matcher.group(TokenType.COMMA.name) != null) {
                tokens.add(constructToken(matcher, TokenType.COMMA, flattedInput))
            } else if (matcher.group(TokenType.HASH.name) != null) {
                tokens.add(constructToken(matcher, TokenType.HASH, flattedInput))
            } else if (matcher.group(TokenType.AT.name) != null) {
                tokens.add(constructToken(matcher, TokenType.AT, flattedInput))
            } else if (matcher.group(TokenType.DOLLAR.name) != null) {
                tokens.add(constructToken(matcher, TokenType.DOLLAR, flattedInput))
            } else if (matcher.group(TokenType.BRACKETS.name) != null) {
                tokens.add(constructToken(matcher, TokenType.BRACKETS, flattedInput))
            } else if (matcher.group(TokenType.ID.name) != null) {
                tokens.add(constructToken(matcher, TokenType.ID, flattedInput))
            } else if (matcher.group(TokenType.ERROR.name) != null) {
                tokens.add(constructToken(matcher, TokenType.ERROR, flattedInput))
            }
        }

        tokens.forEach { println(it) }
    }


    private fun constructToken(matcher: Matcher, tokenType: TokenType, input: String): Token {
        val position = Position(matcher.start(tokenType.name), matcher.end(tokenType.name))
        val lexeme = input.substring(position.startIndex, position.endIndex)

        return Token(tokenType, lexeme, position)
    }
}