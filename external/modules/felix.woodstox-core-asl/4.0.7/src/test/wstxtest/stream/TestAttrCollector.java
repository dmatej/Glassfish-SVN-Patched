package wstxtest.stream;

import javax.xml.stream.XMLStreamException;

import com.ctc.wstx.sr.*;
import com.ctc.wstx.stax.WstxInputFactory;
import com.ctc.wstx.util.StringVector;

/**
 * This class contains tests to ensure that the attribute collectors
 * (low-level containers for attribute information during stream reader's
 * life-cycle) work reliably.
 *<p>
 * !!! 28-Nov-2005, TSa: Not yet implemented; need to figure out exactly
 *   how it should be tested.
 */
public class TestAttrCollector
    extends BaseStreamTest
{
    // // // First randomly chosen (but distinct) attribute names etc;
    // // // 8 of each type

    final static String[] sLocalNames = new String[] {
        "attr1", "foo", "bar", "xyz",
        "attr0", "id", "a", "z_prf",
    };
    final static String[] sPrefixes = new String[] {
        "a", "b", "c", "de", "foo",
        "prefix", "ns", "ns2", "weird_o", "xxxx",
    };
    final static String[] sURIs = new String[] {
        "http://foo", "urn", "someuri", "xyz123", "https://www.com",
        "somethingElse", "http:xxx", "urn:1234", "http://www.google.com", "abc",
    };

    public TestAttrCollector() { super(); }

    public void testNsAttrCollector()
        throws XMLStreamException
    {
        /*WstxInputFactory f =*/ getWstxInputFactory();
        //*NsAttributeCollector ac =*/ new NsAttributeCollector(f.getConfig());
        /*StringVector attrsIn =*/ new StringVector(8);

        // !!! TBI
    }

    public void testNonNsAttrCollector()
        throws XMLStreamException
    {
        WstxInputFactory f = getWstxInputFactory();
        /*NonNsAttributeCollector ac =*/ new NonNsAttributeCollector(f.getConfig());

        // !!! TBI
    }
}
