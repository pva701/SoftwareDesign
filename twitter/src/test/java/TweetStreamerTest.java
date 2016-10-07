import interfaces.ResponseParser;
import interfaces.TweetStreamer;
import interfaces.TwitterRequester;
import model.SearchMetadata;
import model.SearchResponse;
import model.Tweet;
import org.junit.Assert;
import org.junit.Test;

import java.util.Date;

import static org.mockito.Mockito.*;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 07.10.16
 */
public class TweetStreamerTest {

    private static final SearchResponse EMPTY_SEARCH_RESPONSE =
            new SearchResponse(
                SearchMetadata.builder().count(0).build(),
                new Tweet[0]);
    
    private static final int MAX_COUNT = 15;
    
    @Test
    public void test1_ZeroTweets() throws Exception {
        TwitterRequester twitterRequester = mock(TwitterRequesterImpl.class);
        when(twitterRequester.request(anyString(), anyString())).thenReturn("");
        when(twitterRequester.request(anyString(), anyMap())).thenCallRealMethod();

        ResponseParser responseParser = mock(ResponseParser.class);
        when(responseParser.parseResponse("")).thenReturn(EMPTY_SEARCH_RESPONSE);

        TweetStreamer tweetStreamer = new TweetStreamerImpl("moscow", twitterRequester, responseParser);
        checkTweets(tweetStreamer, new Tweet[0]);
    }

    @Test
    public void test2_OnePage() throws Exception {
        TwitterRequester twitterRequester = mock(TwitterRequesterImpl.class);
        when(twitterRequester.request(anyString(), anyString())).thenReturn("");
        when(twitterRequester.request(anyString(), anyMap())).thenCallRealMethod();

        Date d1 = new Date(System.currentTimeMillis());
        Date d2 = new Date(d1.getTime() - 10000);
        Date d3 = new Date(d2.getTime() - 10000);
        Tweet[] page = new Tweet[]{new Tweet(d1), new Tweet(d2), new Tweet(d3)};
        ResponseParser responseParser = mock(ResponseParser.class);
        SearchResponse sr = new SearchResponse(
                                                SearchMetadata.builder().count(MAX_COUNT).build(),
                                                page);
        when(responseParser.parseResponse("")).thenReturn(sr).thenReturn(EMPTY_SEARCH_RESPONSE);
        TweetStreamerImpl tweetStreamer = new TweetStreamerImpl("moscow", twitterRequester, responseParser);
        tweetStreamer.setCount(MAX_COUNT);
        checkTweets(tweetStreamer, page);
    }

    @Test
    public void test2_TwoPage() throws Exception {
        TwitterRequester twitterRequester = mock(TwitterRequesterImpl.class);
        when(twitterRequester.request(anyString(), anyString())).thenReturn("");
        when(twitterRequester.request(anyString(), anyMap())).thenCallRealMethod();

        Date d1 = new Date(System.currentTimeMillis());
        Date d2 = new Date(d1.getTime() - 10000);
        Date d3 = new Date(d2.getTime() - 10000);
        Tweet[] page1 = new Tweet[]{new Tweet(d1), new Tweet(d2)};
        Tweet[] page2 = new Tweet[]{new Tweet(d3)};
        ResponseParser responseParser = mock(ResponseParser.class);
        SearchResponse sr1 = new SearchResponse(
                SearchMetadata.builder().count(2).nextResults("").build(),
                page1);
        SearchResponse sr2 = new SearchResponse(
                SearchMetadata.builder().count(2).build(),
                page2);
        when(responseParser.parseResponse("")).thenReturn(sr1).thenReturn(sr2).thenReturn(EMPTY_SEARCH_RESPONSE);
        TweetStreamerImpl tweetStreamer = new TweetStreamerImpl("moscow", twitterRequester, responseParser);
        tweetStreamer.setCount(2);
        checkTweets(tweetStreamer, new Tweet[]{new Tweet(d1), new Tweet(d2), new Tweet(d3)});
    }

    private void checkTweets(TweetStreamer streamer, Tweet[] tweets) {
        int i = 0;
        for (Tweet tweet: streamer) {
            Assert.assertTrue(tweets[i].equals(tweet));
            ++i;
        }
        Assert.assertEquals(tweets.length, i);
    }
}
