package interfaces;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.Map;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 01.10.16
 */
public interface TwitterRequester {

    String request(String url, Map<String, String> params) throws IOException;
    String request(String url, String params) throws IOException;
}
