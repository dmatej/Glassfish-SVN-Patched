package com.ctc.wstx.util;

import java.util.HashMap;

/**
 * Singleton class that implements "fast intern" functionality, essentially
 * adding a layer that caches Strings that have been previously intern()ed,
 * but that probably shouldn't be added to symbol tables.
 * This is usually used by improving intern()ing of things like namespace
 * URIs.
 *<p>
 * Note: that this class extends {@link HashMap} is an implementation
 * detail -- no code should ever directly call Map methods.
 */
public final class InternCache
    extends HashMap
{
    /**
     * Let's create cache big enough to usually have enough space for
     * all entries... (assuming NS URIs only)
     */
    private final static int DEFAULT_SIZE = 64;

    private final static InternCache sInstance = new InternCache();

    private InternCache() {
        /* Let's also try to seriously minimize collisions... since
         * collisions are likely to be more costly here, with longer
         * Strings; so let's use 2/3 ratio (67%) instead of default
         * (75%)
         */
        super(DEFAULT_SIZE, 0.6666f);
    }

    public static InternCache getInstance() {
        return sInstance;
    }

    public synchronized String intern(String input) {
        String result = (String) get(input);
        if (result == null) {
            result = input.intern();
            put(result, result);
        }
        return result;
    }
}

