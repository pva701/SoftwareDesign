package model;

import java.util.Date;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 04.10.16
 */
public class HashtagCounter {
    private Date from;
    private Date to;
    private int count;

    public HashtagCounter(Date from, Date to, int count) {
        this.from = from;
        this.to = to;
        this.count = count;
    }

    public Date getFrom() {
        return from;
    }

    public Date getTo() {
        return to;
    }

    public int getCount() {
        return count;
    }

    public void incCount() {
        count++;
    }
}
