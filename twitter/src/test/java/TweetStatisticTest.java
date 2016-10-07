import com.xebialabs.restito.server.StubServer;
import interfaces.ResponseParser;
import interfaces.TweetStatistic;
import interfaces.TwitterRequester;
import model.HashtagCounter;
import org.glassfish.grizzly.http.Method;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.io.IOException;

import static com.xebialabs.restito.builder.stub.StubHttp.whenHttp;
import static com.xebialabs.restito.builder.verify.VerifyHttp.verifyHttp;
import static com.xebialabs.restito.semantics.Action.stringContent;
import static com.xebialabs.restito.semantics.Condition.*;
import static utils.Utils.readResourceFile;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 04.10.16
 */
public class TweetStatisticTest {
    public static final int PORT = 32324;

    private StubServer server;

    @Before
    public void start() {
        server = new StubServer(PORT).run();
    }

    @After
    public void stop() {
        server.stop();
    }

    private static final String EMPTY_RESPONSE = "{\"search_metadata\":{}, \"statuses\":[]}";

    @Test
    public void test1_SpbHashtag_Count30() throws IOException {
        TwitterRequester requester = new TwitterRequesterImpl("http://localhost:" + PORT, "");
        ResponseParser parser = new ResponseParserImpl();
        TweetStreamerImpl streamer = new TweetStreamerImpl("spb", requester, parser);
        streamer.setCount(30);
        TweetStatistic tweetStatistic = new TweetStatisticImpl(24, streamer);

        whenHttp(server).
                match(get("/1.1/search/tweets.json"), parameter("q", "#spb"), parameter("count", "30")).
                then(stringContent(readResourceFile("SpbResponse_Page1_Count30.json")));

        whenHttp(server).
                match(get("/1.1/search/tweets.json"), parameter("q", "#spb"), parameter("max_id", "784457035450646527")).
                then(stringContent(readResourceFile("SpbResponse_Page2_Count30.json")));

        whenHttp(server).
                match(get("/1.1/search/tweets.json"), parameter("q", "#spb"), parameter("max_id", "784430094332854272")).
                then(stringContent(EMPTY_RESPONSE));

        HashtagCounter[] tweets = tweetStatistic.computeStatistic();
        for (HashtagCounter h: tweets) {
            System.out.println(h.getCount());
        }

        verifyHttp(server).times(3,
                method(Method.GET),
                uri("/1.1/search/tweets.json")
        );
    }

    @Test
    public void test1_MosowHashtag() throws IOException {
        TwitterRequester requester = new TwitterRequesterImpl("http://localhost:" + PORT, "");
        ResponseParser parser = new ResponseParserImpl();
        TweetStreamerImpl streamer = new TweetStreamerImpl("moscow", requester, parser);
        streamer.setCount(15);
        TweetStatistic tweetStatistic = new TweetStatisticImpl(24, streamer);

        whenHttp(server).
                match(method(Method.GET), uri("/1.1/search/tweets.json"), parameter("q", "#moscow"), parameter("count", "15")).
                then(stringContent(readResourceFile("MoscowResponse_Page1.json")));

        whenHttp(server).
                match(method(Method.GET), uri("/1.1/search/tweets.json"), parameter("q", "#moscow"), parameter("max_id", "784487897051328512")).
                then(stringContent(EMPTY_RESPONSE));

        HashtagCounter[] tweets = tweetStatistic.computeStatistic();
        for (HashtagCounter h: tweets) {
            System.out.println(h.getCount());
        }

        verifyHttp(server).times(2,
                method(Method.GET),
                uri("/1.1/search/tweets.json")
        );
    }
}
