package javax.xml.namespace;

/**
 * 
 * 
 * @author
 *      Kohsuke Kawaguchi (kk@kohsuke.org)
 * @see
 *      http://java.sun.com/webservices/docs/1.3/api/javax/xml/namespace/QName.html
 */
public class QName {
    private final String uri;
    private final String localName;
    private final String prefix;

    public QName(String localName) {
        this("",localName,"");
    }

    public QName(String uri, String localName) {
        this(uri,localName,"");
    }
    
    public QName(String uri, String localName, String prefix) {
        if(uri==null)   uri="";
        if(localName==null || localName.length()==0) throw new IllegalArgumentException();
        if(prefix==null)    throw new IllegalArgumentException();
        
        this.uri = uri;
        this.localName = localName;
        this.prefix = prefix;
    }
    
    public String getLocalPart() {
        return localName;
    }
    
    public String getNamespaceURI() {
        return uri;
    }
    
    public String getPrefix() {
        return prefix;
    }
    
    public int hashCode() {
        return localName.hashCode() ^ uri.hashCode();
    }
    
    public String toString() {
        if(uri.length()==0)     return localName;
        return '{'+uri+'}'+localName;
    }
    
    public boolean equals(Object _rhs) {
        if(!(_rhs instanceof QName))
            return false;
        
        QName rhs = (QName) _rhs;
        return this.localName.equals(rhs.localName) && this.uri.equals(rhs.uri);
    }
    
    public static QName valueOf(String qNameAsString) {
        if( qNameAsString==null || qNameAsString.length()==0 )
            throw new IllegalArgumentException();
        
        if( qNameAsString.charAt(0)=='{') {
            int idx = qNameAsString.lastIndexOf('}');
            if(idx==-1) throw new IllegalArgumentException();
            return new QName( qNameAsString.substring(1,idx), qNameAsString.substring(idx+1) );
        } else {
            return new QName( qNameAsString );
        }
    }
}
