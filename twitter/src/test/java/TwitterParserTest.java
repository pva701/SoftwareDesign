import interfaces.ResponseParser;
import model.SearchResponse;
import org.junit.Assert;
import org.junit.Test;

import static utils.Utils.readResourceFile;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 03.10.16
 */
public class TwitterParserTest {

    @Test
    public void test_SuccessParsing() throws Exception {
        String responseExampleJson = readResourceFile("ResponseExample.json");
        ResponseParser parser = new ResponseParserImpl();
        SearchResponse response = parser.parseResponse(responseExampleJson);

        Assert.assertEquals(response.getStatuses().length, 15);
        Assert.assertNotEquals(response.getSearchMetadata(), null);
        Assert.assertEquals(response.getSearchMetadata().getCount(), 15);
        Assert.assertEquals(response.getSearchMetadata().getMaxId(), 782962647151108096L);
    }

    @Test(expected = Exception.class)
    public void test_FailedParsing() throws Exception {
        ResponseParser parser = new ResponseParserImpl();
        String responseExampleJson = readResourceFile("ResponseExample.json");
        SearchResponse response = parser.parseResponse(responseExampleJson.substring(0, responseExampleJson.length() - 1));
    }
}
