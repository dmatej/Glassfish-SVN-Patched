package wstxtest.util;

import junit.framework.TestCase;

import com.ctc.wstx.util.TextAccumulator;

/**
 * Simple unit tests for testing {@link TextAccumulator}. That class
 * is generally used to try to minimize shuffling between char arrays,
 * Strings and StringBuilders -- most common case being that only one
 * instance is passed, before a String is needed.
 */
public class TestTextAccumulator
    extends TestCase
{
    public void testBasic()
    {
        TextAccumulator acc = new TextAccumulator();

        acc.addText("foo");
        assertEquals("foo", acc.getAndClear());

        acc.addText("foo".toCharArray(), 0, 3);
        acc.addText("bar");
        assertEquals("foobar", acc.getAndClear());
    }
}
