import com.fasterxml.jackson.databind.ObjectMapper;
import interfaces.ResponseParser;
import model.SearchResponse;

import java.io.IOException;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 01.10.16
 */
public class ResponseParserImpl implements ResponseParser {
    private ObjectMapper objectMapper = new ObjectMapper();

    @Override
    public SearchResponse parseResponse(String json) throws IOException {
        return objectMapper.readValue(json, SearchResponse.class);
    }
}
