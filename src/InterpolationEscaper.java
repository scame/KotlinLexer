import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.util.Objects;

public class InterpolationEscaper {

    String readFile(String fileName) {
        String text = null;

        try {
            BufferedReader br = new BufferedReader(new FileReader(fileName));
            final StringBuilder sb = new StringBuilder();

            br.lines().forEach(s -> sb.append(s).append('\n'));
            text = buildUnescapedStr(sb.toString());

        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
        return text;
    }

    private String buildUnescapedStr(String line) {
        StringBuilder sb = new StringBuilder();

        line.chars()
                .mapToObj(i -> String.valueOf((char) (i)))
                .map(character -> Objects.equals(character, "$") ? "\\$" : character)
                .forEach(sb::append);

        return sb.toString();
    }
}
