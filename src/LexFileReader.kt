import java.io.BufferedReader
import java.io.FileReader

class LexFileReader {

    fun parseFile(): List<String> {
        val fileLines = mutableListOf<String>()
        val br = BufferedReader(FileReader(askForFileName()))
        br.lines().forEach { fileLines.add(it) }

        return fileLines
    }

    private fun askForFileName() = "/home/scame/IdeaProjects/KotlinLexer/src/${readLine()}"
}

fun main(args: Array<String>) {
    Lexer().processInput(LexFileReader().parseFile())
}