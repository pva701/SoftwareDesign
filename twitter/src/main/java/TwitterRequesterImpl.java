import interfaces.TwitterRequester;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.util.Map;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 01.10.16
 */
public class TwitterRequesterImpl implements TwitterRequester {

    private String host;
    private String bearerToken;

    public TwitterRequesterImpl(String host, String bearerToken) {
        this.host = host;
        this.bearerToken = bearerToken;
    }

    @Override
    public String request(String url, Map<String, String> params) throws IOException {
        String result = "?";
        boolean first = true;
        for (Map.Entry<String, String> entry: params.entrySet()) {
            if (!first) {
                result += "&";
            }
            first = false;
            result += entry.getKey() + "=" + URLEncoder.encode(entry.getValue(), "UTF-8");
        }
        return request(url, result);
    }

    @Override
    public String request(String url, String params) throws IOException {
        //String urlStr = "https://api.twitter.com/1.1/search/tweets.json?q=itmo";
        String urlStr = host + url + params;

        URL obj = new URL(urlStr);
        HttpURLConnection con = (HttpURLConnection) obj.openConnection();
        // optional default is GET
        con.setRequestMethod("GET");
        con.addRequestProperty("Authorization", "Bearer " + bearerToken);

        BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
        String inputLine, result = "";
        try {
            while ((inputLine = in.readLine()) != null) {
                result += inputLine;
            }
        } finally {
            try {
                in.close();
            } catch (Exception ignore) {}
        }
        return result;
    }
}
