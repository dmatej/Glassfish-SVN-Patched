package relaxngcc.parser;

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLFilterImpl;

/**
 * Filters out those elements which do not belong to neither
 * RELAX NG or RelaxNGCC.
 * 
 * <p>
 * We need to have this filter between XMLReader and our parser
 * because our parser is not built to accept foreign elements.
 * 
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class ForeignElementFilter extends XMLFilterImpl {
    
    public ForeignElementFilter( XMLReader parent ) {
        setParent(parent);
    }
    
    /**
     * When we are filtering a sub-tree, this value indicates the depth.
     */
    private int _depth = 0;
    
    public void startElement( String uri, String localName, String qname,
        Attributes atts ) throws SAXException {
            
        if(_depth!=0) {
            _depth++;
        } else {
            if(uri.equals(Const.NGCC_URI) || uri.equals(Const.RELAXNG_URI)) {
                super.startElement(uri,localName,qname,atts);
            } else {
                _depth++;
            }
        }
    }
    
    public void endElement( String uri, String localName, String qname )
        throws SAXException {
            
        if(_depth!=0) {
            _depth--;
        } else {
            super.endElement(uri,localName,qname);
        }
    }
    
    public void characters( char[] buf, int start, int len ) throws SAXException {
        if(_depth==0)
            super.characters(buf,start,len);
    }
    
    public void ignorableWhitespace( char[] buf, int start, int len ) throws SAXException {
        if(_depth==0)
            super.ignorableWhitespace(buf,start,len);
    }
}

