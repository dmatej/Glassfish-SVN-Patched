package org.iso_relax.jaxp;

import org.iso_relax.verifier.Schema;
import org.iso_relax.verifier.VerifierConfigurationException;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

/**
 * Wraps another {@link SAXParserFactory} and adds validation capability.
 * 
 * @author Daisuke OKAJIMA
 */
public class ValidatingSAXParserFactory extends SAXParserFactory
{
    protected SAXParserFactory _WrappedFactory;
    protected Schema _Schema;

    /**
     * creates a new instance that wraps the default DocumentBuilderFactory
     * @param schema the compiled Schema object. It can not be null.
     */
    public ValidatingSAXParserFactory(Schema schema)
    {
        this(SAXParserFactory.newInstance(), schema);
    }    
    
    /**
     * creates a new instance with an internal SAXParserFactory and Schema.
     * @param wrapped internal SAXParser
     * @param schema  compiled schema. 
     */
    public ValidatingSAXParserFactory(SAXParserFactory wrapped, Schema schema)
    {
        _WrappedFactory = wrapped;
        _Schema = schema;
    }

    /**
     * returns a new SAX parser.
     */
    public SAXParser newSAXParser() throws ParserConfigurationException, SAXException
    {
        try {
              return new ValidatingSAXParser(
                  _WrappedFactory.newSAXParser(),
                  _Schema.newVerifier());
         } catch(VerifierConfigurationException ex) {
             throw new ParserConfigurationException(ex.getMessage());
         }
    }

    /**
     * @see SAXParserFactory#setFeature(String, boolean)
     */
    public void setFeature(String name, boolean value) throws ParserConfigurationException, SAXNotRecognizedException, SAXNotSupportedException
    {
        _WrappedFactory.setFeature(name, value);
    }

    /**
     * @see SAXParserFactory#getFeature(String)
     */
    public boolean getFeature(String name) throws ParserConfigurationException, SAXNotRecognizedException, SAXNotSupportedException
    {
        return _WrappedFactory.getFeature(name);
    }

    public boolean isNamespaceAware() {
        return _WrappedFactory.isNamespaceAware();
    }
    public void setNamespaceAware(boolean awareness) {
        _WrappedFactory.setNamespaceAware(awareness);
    }

    public boolean isValidating() {
        return _WrappedFactory.isValidating();
    }

    public void setValidating(boolean validating) {
        _WrappedFactory.setValidating(validating);
    }
}
