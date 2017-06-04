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

        lines.forEachIndexed { index, line ->
            val matcher = pattern.matcher(line)

            while (matcher.find()) {
                if (matcher.group(TokenType.NUMBER.name) != null) {
                    tokens.add(constructToken(matcher, TokenType.NUMBER, index, line))
                } else if (matcher.group(TokenType.IF.name) != null) {
                    tokens.add(constructToken(matcher, TokenType.IF, index, line))
                }
            }
        }

        tokens.forEach { println(it) }
    }

    private fun constructToken(matcher: Matcher, tokenType: TokenType, lineIndex: Int, line: String): Token {
        val position = Position(lineIndex + 1, matcher.start(tokenType.name))
        val lexeme = line.substring(position.column, matcher.end(tokenType.name))
        return Token(tokenType, lexeme, position)
    }
}