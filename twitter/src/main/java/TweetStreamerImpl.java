import interfaces.ResponseParser;
import interfaces.TweetStreamer;
import interfaces.TwitterRequester;
import model.SearchMetadata;
import model.SearchResponse;
import model.Tweet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 01.10.16
 */
public class TweetStreamerImpl implements TweetStreamer {

    private final Logger LOGGER = LoggerFactory.getLogger(TweetStreamerImpl.class);

    private final String hashtag;
    private final TwitterRequester requester;
    private final ResponseParser parser;
    private SearchMetadata prevMetadata;

    private int count = 30;
    private String apiVersion = "1.1";
    private String searchPath = "/search/tweets.json";
    private String searchURL;

    public TweetStreamerImpl(String hashtag,
                             TwitterRequester requester,
                             ResponseParser parser) {
        this.hashtag = hashtag;
        this.requester = requester;
        this.parser = parser;
    }

    public String getApiVersion() {
        return apiVersion;
    }

    public void setApiVersion(String apiVersion) {
        searchURL = null;
        this.apiVersion = apiVersion;
    }

    public String getSearchPath() {
        return searchPath;
    }

    public void setSearchPath(String searchPath) {
        searchURL = null;
        this.searchPath = searchPath;
    }

    private String getSearchURL() {
        if (searchURL == null) {
            searchURL = "/" + apiVersion + searchPath;
        }
        return searchURL;
    }

    public int getCount() {
        return count;
    }

    public void setCount(int count) {
        this.count = count;
    }

    public Iterator<Tweet> iterator() {
        return this;
    }

    private final LinkedList <Tweet> twittsQueue = new LinkedList<Tweet>();
    private boolean hasNext = true;

    public boolean hasNext() {
        if (twittsQueue.isEmpty()) {
            if (!hasNext) {
                return false;
            }
            getNewTwits();
        }
        return !twittsQueue.isEmpty();
    }

    public Tweet next() {
        if (!hasNext()) {
            throw new NoSuchElementException();
        }
        return twittsQueue.poll();
    }

    private void getNewTwits() {
        assert hasNext;
        try {
            String json;
            if (prevMetadata != null) {
                json = requester.request(getSearchURL(), prevMetadata.getNextResults());
            } else {
                Map<String, String> map = new HashMap<>();
                map.put("q", "#" + hashtag);
                map.put("count", count + "");
                json = requester.request(getSearchURL(), map);
            }
            SearchResponse response = parser.parseResponse(json);
            prevMetadata = response.getSearchMetadata();
            hasNext = !(prevMetadata.getNextResults() == null || response.getStatuses().length < count);
            Collections.addAll(twittsQueue, response.getStatuses());
        } catch (Exception e) {
            LOGGER.info("GET Request to Twitter failed", e);
            hasNext = false;
        }
    }
}
