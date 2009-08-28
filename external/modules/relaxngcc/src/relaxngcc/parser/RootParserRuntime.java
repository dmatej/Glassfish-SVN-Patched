package relaxngcc.parser;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import relaxngcc.NGCCGrammar;
import relaxngcc.grammar.Grammar;
import relaxngcc.grammar.NGCCDefineParam;
import relaxngcc.grammar.Pattern;
import relaxngcc.grammar.RefPattern;
import relaxngcc.parser.state.Start;

/**
 * Runtime that parses grammars normally.
 * 
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class RootParserRuntime extends ParserRuntime {

    public RootParserRuntime() {
        setRootHandler(start=new Start(this));
    }
    
    /** The root state object that we use to parse the RELAX NG grammar. */
    private final Start start;
    
    /** The value specified via the cc:package attribute. */
    private String packageName = "";
    
    /** The value specified via the cc:runtime-type attribute. */
    private String runtimeType = null;

    private String globalImportDecls = "";
    public void appendGlobalImport( String code ) {
        globalImportDecls += code;
    }

    private String globalBody = "";
    public void appendGlobalBody( String code ) {
        globalBody += code;
    }

    /**
     * Timestamp of the source grammar file.
     * If other files are included via some mechanism, those are
     * also incorporated.
     */
    private long grammarTimestamp = -1;
    protected void checkLastModifiedTime( long time ) {
        grammarTimestamp = Math.max( grammarTimestamp, time );
    }
    public long getGrammarTimestamp() { return grammarTimestamp; }

    
    /** Gets the parsed result, or null if there was any error. */
    public NGCCGrammar getResult() {
        Grammar grammar;
        Pattern p = start.getResult();
        if(p instanceof RefPattern) {
            grammar = (Grammar)((RefPattern)p).target;
        } else {
            // if the parsed tree doesn't have the enclosing &lt;grammar>, add one.
            grammar = new Grammar(null);
            grammar.setParam(new NGCCDefineParam("RelaxNGCC_Result",null,null,null,null));
            grammar.append(p,null);
        }
        
        if(runtimeType==null) {
            // if none is specified, defaults to the NGCCRuntime in the target package.
            runtimeType = packageName+".NGCCRuntime";
            if(runtimeType.charAt(0)=='.')
                runtimeType = runtimeType.substring(1);
        }
        
        return new NGCCGrammar(
            grammar,packageName,runtimeType,globalImportDecls,globalBody);
    }

    // TODO: handle datatypeLibrary attribute
    public void startElement( String uri, String local, String qname, Attributes atts )
        throws SAXException {
            
        String v;
        v = atts.getValue(Const.NGCC_URI,"package");
        if(v!=null) packageName = v;
        
        v = atts.getValue(Const.NGCC_URI,"runtime-type");
        if(v!=null) runtimeType = v;
        
        
        super.startElement(uri,local,qname,atts);
    }
}

