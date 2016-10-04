package interfaces;

import model.Tweet;

import java.util.Iterator;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 01.10.16
 */
public interface TweetStreamer extends Iterable<Tweet>, Iterator<Tweet> {
}
