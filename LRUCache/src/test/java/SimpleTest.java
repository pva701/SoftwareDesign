import org.junit.Assert;
import org.junit.Test;
import ru.imfo.peresadin.LRUCache;

import java.util.Map;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 23.09.16
 */
public class SimpleTest {

    @Test
    public void puts() {
        LRUCache<Integer, Integer> cache = new LRUCache<Integer, Integer>(2);
        cache.put(1, 1);
        cache.put(2, 2);
        cache.put(3, 3);
        Assert.assertEquals(cache.size(), 2);
        for (Map.Entry<Integer, Integer> entry: cache.entrySet()) {
            Assert.assertTrue(entry.getKey() != 1);
        }
    }
}
