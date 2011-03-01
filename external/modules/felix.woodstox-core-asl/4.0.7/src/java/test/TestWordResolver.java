package test;

import java.util.*;

import com.ctc.wstx.util.*;

public class TestWordResolver
{
    final int Rounds;

    final String[] mKeywords;
    final char[][] mKeyArrays;

    final HashMap mHashMap;
    final TreeMap mTreeMap;
    final WordResolver mWordResolver;
    final WordSet mWordSet;
    final SymbolTable mSymbolTable;

    public TestWordResolver(String[] args) {
        mKeywords = args;
        mKeyArrays = new char[args.length][];
        mTreeMap = new TreeMap();
        mSymbolTable = new SymbolTable(true, 8);
        TreeSet wordSet = new TreeSet();
        for (int i = 0; i < args.length; ++i) {
            String word = args[i];
            mTreeMap.put(word, word);
            wordSet.add(word);
            char[] arr = word.toCharArray();
            mKeyArrays[i] = arr;
            mSymbolTable.findSymbol(word);
        }
        
        mWordResolver = WordResolver.constructInstance(wordSet);
        mWordSet = WordSet.constructSet(wordSet);
        mHashMap = new HashMap(mTreeMap);

        Rounds = 100000 / args.length;
    }

    public void test()
    {
        int count = 0;

        while (true) {
            String type;
            long now = System.currentTimeMillis();
            int hitCount;

            switch (count % 5) {
            case 0:
                //hitCount = testMap(mHashMap);
                hitCount = testHashMap();
                //hitCount = testHashMap2();
                type = "HashMap";
                break;
            case 1:
                hitCount = testWordResolver();
                type = "WordResolver";
                break;
            case 2:
                hitCount = testSymbolTable();
                type = "SymbolTable";
                break;
            case 3:
                hitCount = testWordSet();
                type = "WordSet";
                break;
            default:
                //hitCount = testMap(mTreeMap);
                hitCount = testTreeMap();
                type = "TreeMap";
                break;
            }

            now = System.currentTimeMillis() - now;
            System.out.println("Test '"+type+"' ["+hitCount+"], "+now+" msecs.");

            ++count;

            try {
                Thread.sleep(100L);
                System.gc();
                Thread.sleep(100L);
            } catch (Throwable t) { }
        }
    }

    int testWordResolver()
    {
        int count = 0;
        WordResolver res = mWordResolver;
        char[][] keyA = mKeyArrays;
        int ROUNDS = this.Rounds;
        for (int i = 0; i < ROUNDS; ++i) {
            for (int j = 0, len = keyA.length; j < len; ++j) {
                char[] key = keyA[j];
                String val = res.find(key, 0, key.length);
                if (val != null) {
                    ++count;
                }
            }
        }
        return count;
    }

    int testWordSet()
    {
        int count = 0;
        WordSet set = mWordSet;
        char[][] keyA = mKeyArrays;
        int ROUNDS = this.Rounds;
        for (int i = 0; i < ROUNDS; ++i) {
            for (int j = 0, len = keyA.length; j < len; ++j) {
                char[] key = keyA[j];
                if (set.contains(key, 0, key.length)) {
                    ++count;
                }
            }
        }
        return count;
    }

    int testSymbolTable()
    {
        int count = 0;
        SymbolTable st = mSymbolTable;
        char[][] keyA = mKeyArrays;
        int ROUNDS = this.Rounds;
        for (int i = 0; i < ROUNDS; ++i) {
            for (int j = 0, len = keyA.length; j < len; ++j) {
                char[] key = keyA[j];
                int keyLen = key.length;
                int hash = SymbolTable.calcHash(key, 0, keyLen);
                String symbol = st.findSymbolIfExists(key, 0, keyLen, hash);
                if (symbol != null) {
                    ++count;
                }
            }
        }
        return count;
    }

    /*
    int testMap(Map set)
    {
        int count = 0;
        String[] keys = mKeywords;
        int ROUNDS = this.Rounds;
        for (int i = 0; i < ROUNDS; ++i) {
            for (int j = 0, len = keys.length; j < len; ++j) {
                String key = keys[j];
                if (set.containsKey(key)) {
                    ++count;
                }
            }
        }
        return count;
    }
    */

    int testHashMap()
    {
        int count = 0;
        String[] keys = mKeywords;
        HashMap map = mHashMap;
        int ROUNDS = this.Rounds;
        for (int i = 0; i < ROUNDS; ++i) {
            for (int j = 0, len = keys.length; j < len; ++j) {
                String key = keys[j];
                String val = (String) map.get(key);
                if (val != null) {
                    ++count;
                }
            }
        }
        return count;
    }

    int testHashMap2()
    {
        int count = 0;
        HashMap map = mHashMap;
        char[][] keyA = mKeyArrays;
        int ROUNDS = this.Rounds;
        for (int i = 0; i < ROUNDS; ++i) {
            for (int j = 0, len = keyA.length; j < len; ++j) {
                char[] key = keyA[j];
                String val = (String) map.get(new String(key));
                if (val != null) {
                    ++count;
                }
            }
        }
        return count;
    }

    int testTreeMap()
    {
        int count = 0;
        String[] keys = mKeywords;
        TreeMap map = mTreeMap;
        int ROUNDS = this.Rounds;
        for (int i = 0; i < ROUNDS; ++i) {
            for (int j = 0, len = keys.length; j < len; ++j) {
                String key = keys[j];
                String val = (String) map.get(key);
                if (val != null) {
                    ++count;
                }
            }
        }
        return count;
    }

    public static void main(String[] args)
    {
        if (args.length < 1) {
            System.err.println("Usage: "+TestWordResolver.class+" word1 [word2] ... [wordN]");
            System.exit(1);
        }
        new TestWordResolver(args).test();
    }
}
