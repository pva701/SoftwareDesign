import model.HashtagCounter;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.text.SimpleDateFormat;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 01.10.16
 */
public class Main {

    private static String readResourceFile(String fileName) throws IOException {
        File file = new File("src/main/resources/" + fileName);
        FileInputStream fis = new FileInputStream(file);
        byte[] data = new byte[(int) file.length()];
        fis.read(data);
        fis.close();
        return new String(data, "UTF-8");
    }

    public static void main(String[] args) throws IOException {
        String bearerToken = readResourceFile("auth.txt");
        String hashTag = args[0];
        int n = Integer.parseInt(args[1]);
        HashtagCounter[] counters = new TweetStatisticImpl(hashTag, n, bearerToken).computeStatistic();
        SimpleDateFormat df = new SimpleDateFormat("E MMM dd HH:mm:ss ZZZZ yyyy");
        for (HashtagCounter counter: counters) {
            String from = df.format(counter.getFrom());
            String to = df.format(counter.getTo());
            System.out.println(from + " - " + to + " : " + counter.getCount());
        }
    }
}
