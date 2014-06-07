
import java.util.Map;

/*
 * Tuple class for usage of pairs
* WARNING: setters do not clone. If this by any means can alter the state of your program
* Either do static-typed implementation or insert clones
 */

/**
 *
 * @author frmendes
 * @param <K>
 * @param <V>
 */
public class Tuple<K, V> implements  Map.Entry<K, V>{
    private K key;
    private V value;
    
    /**
     *
     * @param key
     * @param value
     */
    public Tuple(K key, V value) {
        this.key = key;
        this.value = value;
    }
    
    /**
     *
     * @param T
     */
    public Tuple(Tuple T) {
        this.key = (K)T.getKey();
        this.value = (V)T.getValue();
    }
    
    @Override
    public K getKey() {
        return this.key;
    }

    @Override
    public V getValue() {
        return this.value;
    }

    @Override
    public V setValue(V v) {
        V oldValue = this.value;
        this.value = v;
        return oldValue;
    }
}
