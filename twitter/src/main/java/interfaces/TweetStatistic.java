package interfaces;

import model.HashtagCounter;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 03.10.16
 */
public interface TweetStatistic {

    HashtagCounter[] computeStatistic();
}
