class LexFileReader {

    fun parseFile(): String {
        val iu = InterpolationEscaper()
        val str = iu.readFile(askForFileName())

        return str
    }

    private fun askForFileName() = "/home/scame/IdeaProjects/KotlinLexer/src/${readLine()}"
}

fun main(args: Array<String>) {
    Lexer().processInput(LexFileReader().parseFile())
}