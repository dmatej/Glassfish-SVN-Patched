package relaxngcc.parser;

import java.io.StringReader;

import org.xml.sax.Attributes;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLFilterImpl;
import relaxngcc.javabody.JavaBodyParser;
import relaxngcc.javabody.ParseException;
import relaxngcc.javabody.Token;
import relaxngcc.javabody.TokenMgrError;
import relaxngcc.runtime.AttributesImpl;

/**
 * Handles the text syntax of RelaxNGCC and "internalizes" them
 * into normal syntax.
 * 
 * @author Kohsuke Kawaguchi (kk@kohsuke.org)
 */
public class TextSyntaxInternalizer extends XMLFilterImpl {
    public TextSyntaxInternalizer( XMLReader parent ) {
        setParent(parent);
    }
    
    // buffered text
    private StringBuffer buf = new StringBuffer();
    
    public void characters( char[] b, int start, int len ) {
        buf.append( b, start, len );
    }
    
    public void ignorableWhitespace( char[] b, int start, int len ) {
        buf.append( b, start, len );
    }
    
    /**
     * If an alias is found in the processText method, the alias
     * name will be set to this field. Otherwise null.
     */
    private String nextAlias;
    
    /** The parser instance will be reused to avoid overhead. */
    private final JavaBodyParser parser = new JavaBodyParser(
        new StringReader(""));
    
    /** empty attribute set. */
    private static final Attributes emptyAttributes = new AttributesImpl();
    
    private void processText() throws SAXException {
        
        // reset
        nextAlias=null;
        
        if(buf.length()==0) return;
        
        String rawText = buf.toString();
        String text = rawText.trim();
        if(text.length()==0) {
            char[] buf = rawText.toCharArray();
            super.characters(buf,0,buf.length);
            resetBuffer();
            return;
        }
        
        // we have some meaningful text. process it.
        
        // first, check the trailing '='
        if(text.charAt(text.length()-1)=='=') {
            // if there is one, then we expect "token="
            
            text = text.substring(0,text.length()-1); // cut '='
            text = text.trim();
            
            int idx=text.length()-1;
            while(idx>=0 && "\t\n ;".indexOf(text.charAt(idx))==-1)
                idx--;
            
            nextAlias = text.substring(idx+1);
            
            text = text.substring(0,idx+1);
        }
        
        // then check the arguments if the first character is '('.
        if(text.length()!=0 && text.charAt(0)=='(') {
	        try {
	            parser.ReInit(new StringReader(text));
	            parser.Arguments(); // see if we can consume (x,y,z)
	            
                // successfully consumed arguments.
	            String args = cutString(text,parser.token);
                
                // trim off the first '(' and last ')'
                args = args.substring(1,args.length()-1);

                parser.Semicolon(); // further consume ';', to make sure that
                                    // the fragment is properly terminated.
                text = text.substring(cutString(text,parser.token).length());

// DBG                System.out.println("\nreporting "+args);
                // emulate this arguments as a SAX event
                
                // TODO error check. This can be added only after certain elements.
                super.startElement(Const.NGCC_URI,"withParams","withParams",emptyAttributes);
                super.characters(args.toCharArray(),0,args.length());
                super.endElement(Const.NGCC_URI,"withParams","withParams");
	        } catch( ParseException e ) {
                System.out.println("\nfailed to parse");
                // if we failed to parse, just ignore it.
                // we are not even sure if this is an error.
                // for example, a text might be something like
                // "((Bob)foo).invoke();"
                // which is a perfectly valid Java code.
                ;
            } catch( TokenMgrError e ) {
                ;
            }
        }
        
        text = text.trim();
        if(text.length()!=0) {
	        // report other characters as <cc:java>.
            super.startElement(Const.NGCC_URI,"java","java",emptyAttributes);
            super.characters(text.toCharArray(),0,text.length());
            super.endElement(Const.NGCC_URI,"java","java");
        }
        
        resetBuffer();
    }
    
    private void resetBuffer() {
        // then reset StringBuffer
        if(buf.length()<1024)   buf.setLength(0);
        else                    buf = new StringBuffer();
    }
    
    public void startElement( String uri, String local, String qname,
        Attributes atts ) throws SAXException {
        
        processText();
        if(nextAlias!=null) {
            // if there is the nextAlias, add that as if it were an attribute.
            
            // TODO: error check. cc:alias can be only added to certain elements,
            // and it's invalid if those elements already have cc:alias.
            AttributesImpl newAtts = new AttributesImpl(atts);
            newAtts.addAttribute(Const.NGCC_URI,"alias","cc:alias",null,nextAlias);
            atts = newAtts;
        }
        super.startElement(uri,local,qname,atts);
    }
    
    public void endElement( String uri, String local, String qname ) throws SAXException {
        
        if(uri.equals(Const.NGCC_URI)
        || (uri.equals(Const.RELAXNG_URI) &&
            (local.equals("param")||local.equals("name")||local.equals("value")))) {
            // don't process text inside NGCC elements.
            // and certain RELAX NG elements can have values
            super.characters(buf.toString().toCharArray(),0,buf.length());
            resetBuffer();
        } else
            processText();
            
        super.endElement(uri,local,qname);
    }

    
    /**
     * Cuts the string from the beginning till the specified token.
     * The returned string includes the specified token.
     */
    private static String cutString( String str, Token t ) {
        int line = t.endLine;
        int col = t.endColumn;
        
        StringBuffer buf = new StringBuffer();
        
        while(line>1) {
            int idx = str.indexOf('\n');
            buf.append(str.substring(0,idx+1));
            str = str.substring(idx+1);
            line--;
        }
        
        buf.append(str.substring(0,col));
        
        return buf.toString();
    }
}

