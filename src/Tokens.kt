enum class TokenType(val pattern: String) {
    BLOCKCOMMENT("/\\*([^*]|[\\r\\n]|(\\*+([^*/]|[\\r\\n])))*\\*+/"),
    SINGLELINECOMMENT("//.*+"),

    WHITESPACE("\\s+"),

    BINARY("0b[0-1]+\\b"),
    FLOAT("[+-]?([0-9]+)([.][0-9]+)?f\\b"),
    DOUBLE("[+-]?([0-9]+[.])[0-9]+\\b"),
    LONG("[+-]?[0-9]+L\\b"),
    INTEGRAL("[+-]?[0-9]+\\b"),

    TYPEALIAS("typealias\\b"),
    INTERFACE("interface\\b"),
    CONTINUE("continue\\b"),
    PACKAGE("package\\b"),
    IMPORT("import\\b"),
    RETURN("return\\b"),
    OBJECT("object\\b"),
    WHILE("while\\b"),
    BREAK("break\\b"),
    CLASS("class\\b"),
    THROW("throw\\b"),
    FALSE("false\\b"),
    SUPER("super\\b"),
    TYPEOF("typeof\\b"),
    WHEN("when\\b"),
    TRUE("true\\b"),
    THIS("this\\b"),
    NULL("null\\b"),
    ELSE("else\\b"),
    TRY("try\\b"),
    VAL("val\\b"),
    VAR("var\\b"),
    FUN("fun\\b"),
    FOR("for\\b"),
    IS("is\\b"),
    IN("in\\b"),
    IF("if\\b"),
    DO("do\\b"),
    AS("as\\b"),

    RESERVED("\\.\\.\\."),
    EQEQEQ("==="),
    EXCLEQEQ("!=="),
    NOTIN("!in\\s+"),
    NOTIS("!is\\s+"),
    ASSAFE("as\\?"),
    INCR("\\+\\+"),
    DECR("--"),
    LTEQ("<="),
    GTEQ(">="),
    DOUBLEEQ("=="),
    EXCLEQ("!="),
    ANDAND("&&"),
    OROR("\\|\\|"),
    MULTEQ("//*="),
    DIVEQ("/="),
    PEREQ("%/"),
    PLUSEQ("//+="),
    MINUSEQ("-="),
    ARROW("->"),
    DOUBLEARROW("=>"),
    RANGE("\\.\\."),
    COLONCOLON("::"),
    LBRACKET("\\["),
    RBRACKET("\\]"),
    LBRACE("\\{"),
    RBRACE("\\}"),
    LPAR("\\("),
    RPAR("\\)"),
    DOT("\\."),
    MUL("\\*"),
    PLUS("\\+"),
    MINUS("-"),
    EXCL("!"),
    DIV("/"),
    PERC("%"),
    LT("<"),
    GT(">"),
    QUEST("\\?"),
    COLON(":"),
    SEMICOLON(";"),
    EQ("="),
    COMMA(","),
    HASH("#"),
    AT("@"),
    IGNORED("\\b_\\b"),

    CHAR("'.{1}'"),
    STR("(?s)\".*?\""),

    ID("[a-zA-Z][a-zA-Z0-9_]*"),

    ERROR(".+")
}

enum class InterpolationPatterns(val pattern: String) {
    INTERPOLATIONCOMPLEX("(?s)\\$\\{.*?}"),
    INTERPOLATIONSIMPLE("\\$\\w+"),
}

open class BaseToken

data class Token(val tokenType: TokenType, val lexeme: String, val position: Position): BaseToken()

data class StringToken(val token: Token,
                       val interpolatedGroups: MutableList<InterpolatedGroup>): BaseToken()

data class InterpolatedGroup(val position: Position, val tokens: MutableList<BaseToken>)

data class Position(val startIndex: Int, val endIndex: Int)