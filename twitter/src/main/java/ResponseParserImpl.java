import com.fasterxml.jackson.databind.ObjectMapper;
import interfaces.ResponseParser;
import model.SearchResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 01.10.16
 */
public class ResponseParserImpl implements ResponseParser {

    private final Logger LOGGER = LoggerFactory.getLogger(TweetStreamerImpl.class);

    private ObjectMapper objectMapper = new ObjectMapper();

    @Override
    public SearchResponse parseResponse(String json) throws IOException {
        try {
            return objectMapper.readValue(json, SearchResponse.class);
        } catch (IOException e) {
            LOGGER.warn("Parsing of JSON failed", e);
            throw e;
        }
    }
}
