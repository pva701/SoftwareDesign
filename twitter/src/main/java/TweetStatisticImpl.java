import interfaces.ResponseParser;
import interfaces.TweetStatistic;
import interfaces.TweetStreamer;
import interfaces.TwitterRequester;
import model.HashtagCounter;
import model.Tweet;

import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.concurrent.TimeUnit;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 03.10.16
 */
public class TweetStatisticImpl implements TweetStatistic {
    private final TweetStreamer streamer;
    private String hashtag;
    private int n;

    public TweetStatisticImpl(int n,
                              TweetStreamer streamer) {
        this.n = n;
        this.streamer = streamer;
    }

    public TweetStatisticImpl(String hashtag,
                              int n,
                              TwitterRequester requester,
                              ResponseParser parser) {
        this.hashtag = hashtag;
        this.n = n;
        this.streamer = new TweetStreamerImpl(hashtag, requester,  parser);
    }

    public TweetStatisticImpl(String hashtag, int n, String bearerToken) {
        this(hashtag, n,
             new TwitterRequesterImpl("https://api.twitter.com", bearerToken),
             new ResponseParserImpl());
    }

    public HashtagCounter[] computeStatistic() {
        Date now = new Date();
        Date cur = now;
        HashtagCounter[] ret = new HashtagCounter[n];

        for (int i = 0; i < n; ++i) {
            Date newCur = Date.from(cur.toInstant().minus(1, ChronoUnit.HOURS));
            ret[i] = new HashtagCounter(newCur, cur, 0);
            cur = newCur;
        }

        for (Tweet tweet: streamer) {
            int hours = (int)getDateDiff(tweet.getCreatedAt(), now, TimeUnit.HOURS);
            if (hours >= n) {
                break;
            }
            ret[hours].incCount();
        }
        return ret;
    }

    private static long getDateDiff(Date date1, Date date2, TimeUnit timeUnit) {
        long diffInMillies = date2.getTime() - date1.getTime();
        return timeUnit.convert(diffInMillies,TimeUnit.MILLISECONDS);
    }
}
