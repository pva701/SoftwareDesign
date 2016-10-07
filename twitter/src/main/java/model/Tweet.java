package model;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Date;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 01.10.16
 */

@JsonIgnoreProperties(ignoreUnknown = true)
public class Tweet {

    //Mon Oct 03 15:16:56 +0000 2016
    @JsonProperty("created_at")
    @JsonFormat(shape=JsonFormat.Shape.STRING, pattern="E MMM dd HH:mm:ss ZZZZ yyyy")
    private Date createdAt;

    public Tweet() {}

    public Tweet(Date createdAt) {
        this.createdAt = createdAt;
    }

    public Date getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(Date createdAt) {
        this.createdAt = createdAt;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Tweet tweet = (Tweet) o;

        return createdAt != null ? createdAt.equals(tweet.createdAt) : tweet.createdAt == null;

    }

    @Override
    public int hashCode() {
        return createdAt != null ? createdAt.hashCode() : 0;
    }
}
