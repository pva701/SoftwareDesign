package interfaces;

import model.SearchResponse;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 01.10.16
 */
public interface ResponseParser {
    SearchResponse parseResponse(String json) throws Exception;
}
