package test;

import javax.xml.stream.*;

/**
 * Simple XML warning reporter class to used with test classes.
 */
public class TestReporter
    implements XMLReporter
{
    public void report(String msg, String errorType, Object info, Location location)
    {
        System.err.println("\nWARNING("+errorType+"): "+msg+" [at "+location+"]");
    }
}

