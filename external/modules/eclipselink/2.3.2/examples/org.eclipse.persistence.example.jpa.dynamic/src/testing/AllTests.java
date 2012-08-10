package testing;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses( { ConfigTests.class, QueryExamples.class,
		TransactionExamples.class, PUWithoutXML.class })
public class AllTests {
}
