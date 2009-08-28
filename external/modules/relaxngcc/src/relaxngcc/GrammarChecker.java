/*
 * GrammarChecker.java
 *
 * Created on 2002/01/06, 9:41
 */

package relaxngcc;
import javax.xml.parsers.SAXParserFactory;
import com.sun.msv.grammar.Grammar;
import com.sun.msv.reader.util.GrammarLoader;
import com.sun.msv.driver.textui.DebugController;

public class GrammarChecker
{
    //check grammar with MSV
    public static Grammar check(String location, SAXParserFactory f) throws Exception {
        return GrammarLoader.loadSchema(location, new DebugController(false,false), f);
    }
    
}
