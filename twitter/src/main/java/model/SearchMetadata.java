package model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 04.10.16
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class SearchMetadata {

    @JsonProperty("max_id")
    private long maxId;

    @JsonProperty("next_results")
    private String nextResults;

    @JsonProperty("count")
    private int count;

    public SearchMetadata() {}

    public long getMaxId() {
        return maxId;
    }

    public void setMaxId(long maxId) {
        this.maxId = maxId;
    }

    public String getNextResults() {
        return nextResults;
    }

    public void setNextResults(String nextResults) {
        this.nextResults = nextResults;
    }

    public int getCount() {
        return count;
    }

    public void setCount(int count) {
        this.count = count;
    }
}
