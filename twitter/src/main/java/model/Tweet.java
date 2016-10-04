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

    public Date getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(Date createdAt) {
        this.createdAt = createdAt;
    }
}
