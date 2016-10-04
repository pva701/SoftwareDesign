package ru.imfo.peresadin;

import java.util.*;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 23.09.16
 */
public class LRUCache<K, V> extends LinkedHashMap<K, V> {
    private final int maxSize;
    private static final float DEFAULT_LOAD_FACTOR = 0.75f;
    private static final int DEFAULT_MAX_SIZE = 1024;

    public LRUCache(int maxSize, float loadFactor) {
        super(maxSize, loadFactor);
        this.maxSize = maxSize;
    }

    public LRUCache(int maxSize) {
        this(maxSize, DEFAULT_LOAD_FACTOR);
    }

    public LRUCache() {
        this(DEFAULT_MAX_SIZE);
    }

    @Override
    protected boolean removeEldestEntry(Map.Entry<K, V> eldest) {
        return size() > maxSize;
    }

    @Override
    public V put(K key, V value) {
        if (key == null || value == null) {
            throw new IllegalArgumentException("key and value mustn't be null");
        }
        V prev = remove(key);
        V noElement = super.put(key, value);
        assert noElement == null;
        return prev;
    }

    @Override
    public V get(Object key) {
        if (key == null) {
            throw new IllegalArgumentException("key mustn't be null");
        }
        V prev = remove(key);
        if (prev == null) {
            return null;
        }
        V noElement = super.put((K)key, prev);
        assert noElement == null;
        return prev;
    }
}
