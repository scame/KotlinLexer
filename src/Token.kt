enum class TokenType(val pattern: String) {
    NUMBER("[0-9]+"), IF("if");
}

data class Token(val tokenType: TokenType, val lexeme: String, val position: Position)

data class Position(val row: Int, val column: Int)