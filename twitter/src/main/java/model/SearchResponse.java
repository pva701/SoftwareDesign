package model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 03.10.16
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class SearchResponse {

    private Tweet[] statuses;

    @JsonProperty("search_metadata")
    private SearchMetadata searchMetadata;

    public SearchResponse() {}

    public Tweet[] getStatuses() {
        return statuses;
    }

    public void setStatuses(Tweet[] statuses) {
        this.statuses = statuses;
    }

    public SearchMetadata getSearchMetadata() {
        return searchMetadata;
    }

    public void setSearchMetadata(SearchMetadata searchMetadata) {
        this.searchMetadata = searchMetadata;
    }
}
