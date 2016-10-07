package utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 07.10.16
 */
public class Utils {

    public static String readResourceFile(String fileName) throws IOException {
        File file = new File("src/test/resources/" + fileName);
        FileInputStream fis = new FileInputStream(file);
        byte[] data = new byte[(int) file.length()];
        fis.read(data);
        fis.close();
        return new String(data, "UTF-8");
    }
}
