package test;

import java.util.*;

import com.ctc.wstx.util.WordSet;
import com.ctc.wstx.util.SymbolTable;

public class TestWordSet
{
    final int Rounds;

    final String[] mKeywords;
    final char[][] mKeyArrays;

    final HashSet mHashSet;
    final TreeSet mTreeSet;
    final char[] mWordSet;
    final SymbolTable mSymbolTable;

    public TestWordSet(String[] args) {
        mKeywords = args;
        mKeyArrays = new char[args.length][];
        mTreeSet = new TreeSet();
        mSymbolTable = new SymbolTable(true, 8);
        for (int i = 0; i < args.length; ++i) {
            String word = args[i];
            mTreeSet.add(word);
            char[] arr = word.toCharArray();
            mKeyArrays[i] = arr;
            mSymbolTable.findSymbol(word);
        }
        
        mWordSet = WordSet.constructRaw(mTreeSet);
        mHashSet = new HashSet(mTreeSet);

        Rounds = 100000 / args.length;
    }

    public void test()
    {
        int count = 0;

        while (true) {
            String type;
            long now = System.currentTimeMillis();
            int hitCount;

            switch (count % 4) {
            case 0:
                //hitCount = testSet(mHashSet);
                //hitCount = testHashSet();
                hitCount = testHashSet2();
                type = "HashSet";
                break;
            case 1:
                hitCount = testWordSet();
                type = "WordSet";
                break;
            case 2:
                hitCount = testSymbolTable();
                type = "SymbolTable";
                break;
            default:
                //hitCount = testSet(mTreeSet);
                hitCount = testTreeSet();
                type = "TreeSet";
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

    int testWordSet()
    {
        int count = 0;
        char[] set = mWordSet;
        char[][] keyA = mKeyArrays;
        int ROUNDS = this.Rounds;
        for (int i = 0; i < ROUNDS; ++i) {
            for (int j = 0, len = keyA.length; j < len; ++j) {
                char[] key = keyA[j];
                if (WordSet.contains(set, key, 0, key.length)) {
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
    int testSet(Set set)
    {
        int count = 0;
        String[] keys = mKeywords;
        int ROUNDS = this.Rounds;
        for (int i = 0; i < ROUNDS; ++i) {
            for (int j = 0, len = keys.length; j < len; ++j) {
                String key = keys[j];
                if (set.contains(key)) {
                    ++count;
                }
            }
        }
        return count;
    }
    */

    int testHashSet()
    {
        int count = 0;
        String[] keys = mKeywords;
        HashSet set = mHashSet;
        int ROUNDS = this.Rounds;
        for (int i = 0; i < ROUNDS; ++i) {
            for (int j = 0, len = keys.length; j < len; ++j) {
                String key = keys[j];
                if (set.contains(key)) {
                    ++count;
                }
            }
        }
        return count;
    }

    int testHashSet2()
    {
        int count = 0;
        HashSet set = mHashSet;
        char[][] keyA = mKeyArrays;
        int ROUNDS = this.Rounds;
        for (int i = 0; i < ROUNDS; ++i) {
            for (int j = 0, len = keyA.length; j < len; ++j) {
                char[] key = keyA[j];
                if (set.contains(new String(key))) {
                    ++count;
                }
            }
        }
        return count;
    }

    int testTreeSet()
    {
        int count = 0;
        String[] keys = mKeywords;
        TreeSet set = mTreeSet;
        int ROUNDS = this.Rounds;
        for (int i = 0; i < ROUNDS; ++i) {
            for (int j = 0, len = keys.length; j < len; ++j) {
                String key = keys[j];
                if (set.contains(key)) {
                    ++count;
                }
            }
        }
        return count;
    }

    public static void main(String[] args)
    {
        if (args.length < 1) {
            System.err.println("Usage: "+WordSet.class+" word1 [word2] ... [wordN]");
            System.exit(1);
        }
        new TestWordSet(args).test();
    }
}
