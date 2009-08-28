package relaxngcc.parser;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Stack;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParserFactory;

import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.LocatorImpl;

import relaxngcc.grammar.Grammar;
import relaxngcc.grammar.NameClass;
import relaxngcc.grammar.SimpleNameClass;
import relaxngcc.parser.state.NGCCRuntime;

/**
 * 
 * 
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public abstract class ParserRuntime extends NGCCRuntime {
    
    /** Parses a document with this runtime. */
    public void parse(String source) throws SAXException {
        
        // check the date of this source file
        checkLastModifiedTime(new File(source).lastModified());
        
        // parse
        try {
            XMLReader reader = _SAXFactory.newSAXParser().getXMLReader();
            reader = new ForeignElementFilter(reader);
            reader = new TextSyntaxInternalizer(reader);
            reader.setContentHandler(this);
            reader.parse(source);
        } catch( ParserConfigurationException e ) {
            throw new SAXException(e);
        } catch( IOException e ) {
            throw new SAXException(e);
        }
    }
    
    /** Call-back method that receives the last modified time of a newly parsed file. */
    protected abstract void checkLastModifiedTime( long time );

    /** static SAX parser factory. */
    static private final SAXParserFactory _SAXFactory;
    static {
        _SAXFactory = SAXParserFactory.newInstance();
        _SAXFactory.setNamespaceAware(true);
        _SAXFactory.setValidating(false);
    }
    
    /** Parses a QName into a SimpleNameClass. */
    public NameClass parseSimpleName( String qname, boolean attributeMode ) {
        String uri,local;
        
        int idx = qname.indexOf(':');
        
        if(idx<0) {
            if(attributeMode && !_nsPresent) {
                uri="";
                local=qname;
            } else {
                uri=getTargetNamespace();
                local=qname;
            }
        } else {
            String prefix = qname.substring(0,idx);
            uri = resolveNamespacePrefix(prefix);
            if(uri==null) {
                // TODO: undeclared prefix
                throw new UnsupportedOperationException();
//                uri = "";
            }
            local = qname.substring(idx+1);
        }
        
        return new SimpleNameClass(uri,local);
    }
    
    /** Grammar object that we are currently building. */
    public Grammar grammar;
    
    /** Processes the &lt;include> element. */
    public void processInclude( String href ) throws SAXException {
        // TODO: support entity resolver
        
        // resolve relative href.
        // TODO: we need a fully-fledged URI class.
        try {
            href = new URL( new URL(getLocator().getSystemId()), href ).toExternalForm();
        } catch( MalformedURLException e ) {
            // TODO: error handling?
            throw new SAXException(e);
        }
        
        new IncludeParserRuntime(this).parse(href);
    }
    
    /** Any global-scope &lt;cc:java-import> will be reported here. */
    public abstract void appendGlobalImport( String code );
    
    /** Any global-scope &lt;cc:java-body> will be reported here. */
    public abstract void appendGlobalBody( String code );
    
    
    
    /** Keeps track of values of the ns attribute. */
    protected final Stack _nsStack = new Stack();
    {// register the default binding
        _nsStack.push("");
    }
    
    /** Gets the value of the current "ns". */
    public String getTargetNamespace() {
        return (String)_nsStack.peek();
    }
    
    /** set to true if the ns attribute is present. */
    private boolean _nsPresent;
    
    public Locator createLocator() {
        return new LocatorImpl(super.getLocator());
    }
    
    // override start/endElement to handle the ns attribute
    // TODO: handle datatypeLibrary attribute
    public void startElement( String uri, String local, String qname, Attributes atts )
        throws SAXException {
            
        String ns = atts.getValue("ns");
        _nsPresent = (ns!=null);
        if(ns==null)    ns = getTargetNamespace();
        _nsStack.push(ns);
        
        super.startElement(uri,local,qname,atts);
    }
    
    public void endElement( String uri, String local, String qname )
        throws SAXException {
            
        super.endElement(uri,local,qname);
        _nsStack.pop();
    }
}

