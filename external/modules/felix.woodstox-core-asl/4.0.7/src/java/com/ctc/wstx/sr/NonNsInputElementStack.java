package com.ctc.wstx.sr;

import java.util.*;

import javax.xml.namespace.QName;
import javax.xml.stream.Location;
import javax.xml.stream.XMLStreamException;

import org.codehaus.stax2.ri.EmptyIterator;
import org.codehaus.stax2.validation.XMLValidator;

import com.ctc.wstx.api.ReaderConfig;
import com.ctc.wstx.util.BaseNsContext;
import com.ctc.wstx.util.EmptyNamespaceContext;
import com.ctc.wstx.util.StringVector;
import com.ctc.wstx.util.TextBuilder;

/**
 * Sub-class of {@link InputElementStack} used when operating in
 * non-namespace-aware, non validating mode.
 */
public final class NonNsInputElementStack
    extends InputElementStack
{
    final static String INT_ERR_UNEXPECTED_CALL = "Internal error: method should never be called for non-namespace element stack.";

    /*
    //////////////////////////////////////////////////
    // Configuration
    //////////////////////////////////////////////////
     */

    protected final NonNsAttributeCollector mAttrCollector;

    /*
    //////////////////////////////////////////////////
    // Element stack state information
    //////////////////////////////////////////////////
     */

    /**
     * Array that contains path of open elements from root, one String
     * for each open start element.
     */
    protected String[] mElements;

    /**
     * Number of Strings in {@link #mElements} that are valid.
     */
    protected int mSize;

    /*
    //////////////////////////////////////////////////
    // Simple 1-slot QName cache; used for improving
    // efficiency of code that uses QNames extensively
    // (like StAX Event API implementation)
    //////////////////////////////////////////////////
     */

    protected String mLastLocalName = null;

    protected QName mLastName = null;

    /*
    //////////////////////////////////////////////////
    // Life-cycle (create, update state)
    //////////////////////////////////////////////////
     */

    public NonNsInputElementStack(int initialSize, ReaderConfig cfg)
    {
        super(cfg);
        mSize = 0;
        if (initialSize < 4) {
            initialSize = 4;
        }
        mElements = new String[initialSize];
        mAttrCollector = new NonNsAttributeCollector(cfg);
    }

    protected void setAutomaticDTDValidator(XMLValidator validator, NsDefaultProvider nsDefs)
    {
        addValidator(validator);
    }

    public final void push(String prefix, String localName)
    {
        throw new IllegalStateException(INT_ERR_UNEXPECTED_CALL);
    }

    public final void push(String fullName)
    {
        if (mSize == mElements.length) {
            String[] old = mElements;
            mElements = new String[old.length + 32];
            System.arraycopy(old, 0, mElements, 0, old.length);
        }
        mElements[mSize] = fullName;
        ++mSize;
        mAttrCollector.reset();
    }

    /**
     * @return True if stack still has elements; false if not (root closed)
     */
    public boolean pop()
        throws XMLStreamException
    {
        if (mSize == 0) {
            throw new IllegalStateException("Popping from empty stack.");
        }
        /* Let's allow GCing (not likely to matter, as Strings are very
         * likely interned... but it's a good habit
         */
        mElements[--mSize] = null;
        return (mSize > 0);
    }

    /**
     * Method called to update information about top of the stack, with
     * attribute information passed in. Will resolve namespace references,
     * and update namespace stack with information.
     *
     * @return Validation state that should be effective for the fully
     *   resolved element context
     */
    public int resolveAndValidateElement()
        throws XMLStreamException
    {
        NonNsAttributeCollector ac = mAttrCollector;

        /* Attribute collector can now build its accessor data structs
         * as necessary
         */
        int xmlidIx = ac.resolveValues(mReporter);
        mIdAttrIndex = xmlidIx;

        // Any validator(s)? If not, we are done (except for xml:id check)
        if (mValidator == null) {
            if (xmlidIx >= 0) { // need to normalize xml:id, still?
                normalizeXmlIdAttr(ac, xmlidIx);
            }
            return XMLValidator.CONTENT_ALLOW_ANY_TEXT;
        }

        // Otherwise need to call validator's methods:

        /* First, a call to check if the element itself may be acceptable
         * within structure:
         */
        mValidator.validateElementStart(mElements[mSize-1], null,null);

        // Then attributes, if any:
        int attrLen = ac.getCount();
        if (attrLen > 0) {
            StringVector attrNames = ac.getNameList();
            String[] nameData = attrNames.getInternalArray();
            TextBuilder attrBuilder = ac.getAttrBuilder();
            char[] attrCB = attrBuilder.getCharBuffer();
            for (int i = 0; i < attrLen; ++i) {
                String normValue = mValidator.validateAttribute
                    (nameData[i], null, null, attrCB,
                     attrBuilder.getOffset(i),
                     attrBuilder.getOffset(i+1));
                if (normValue != null) {
                    ac.setNormalizedValue(i, normValue);
                }
            }
        }

        /* And finally let's wrap things up to see what textual content
         * is allowed as child content, if any:
         */
        return mValidator.validateElementAndAttributes();
    }

    public int validateEndElement()
        throws XMLStreamException
    {
        if (mValidator == null) { // should never be null if we get here
            return XMLValidator.CONTENT_ALLOW_ANY_TEXT;
        }
        int index = mSize-1;
        int result = mValidator.validateElementEnd(mElements[index], null, null);
        if (index == 0) { // root closing
            mValidator.validationCompleted(true);
        }
        return result;
    }

    /*
    ///////////////////////////////////////////////////
    // Access to helper objects
    ///////////////////////////////////////////////////
     */

    public final AttributeCollector getAttrCollector() {
        return mAttrCollector;
    }

    /**
     * Method called to construct a non-transient NamespaceContext instance;
     * generally needed when creating events to return from event-based
     * iterators.
     */
    public final BaseNsContext createNonTransientNsContext(Location loc) {
        return EmptyNamespaceContext.getInstance();
    }

    /*
    ///////////////////////////////////////////////////
    // Implementation of NamespaceContext:
    ///////////////////////////////////////////////////
     */

    public final String getNamespaceURI(String prefix) {
        /* No prefixes can be bound in non-namespace mode, however,
         * "no prefix" needs to match "no namespace": latter of which
         * is signified by the empty String
         */
        if (prefix == null || prefix.length() == 0) {
            return "";
        }
        return null;
    }

    public final String getPrefix(String nsURI) {
        return null;
    }

    public final Iterator getPrefixes(String nsURI) {
        return EmptyIterator.getInstance();
    }

    /*
    ///////////////////////////////////////////////////
    // AttributeInfo methods (StAX2)
    ///////////////////////////////////////////////////
     */

    public final int getAttributeCount()
    {
        return mAttrCollector.getCount();
    }

    public final int findAttributeIndex(String nsURI, String localName)
    {
        // Should never pass a NS URI...
        if (nsURI != null && nsURI.length() > 0) {
            return -1;
        }
        return mAttrCollector.findIndex(localName);
    }

    /*
    ///////////////////////////////////////////////////
    // ValidationContext methods
    ///////////////////////////////////////////////////
     */

    public final QName getCurrentElementName()
    {
        if (mSize == 0) {
            return null;
        }
        /* 03-Dec-2004, TSa: Maybe we can just reuse the last QName
         *    object created, if we have same data? (happens if
         *    state hasn't changed, or we got end element for a leaf
         *    element, or repeating leaf elements)
         */
        String ln = mElements[mSize-1];

        /* Since local names are always interned, can just use cheap
         * identity comparison here:
         */
        if (ln == mLastLocalName) {
            return mLastName;
        }
        QName n = new QName(ln);
        mLastLocalName = ln;
        mLastName = n;
        return n;
    }

    public int addDefaultAttribute(String localName, String uri, String prefix,
                                   String value)
    {
        // No real namespace info passed...
        return mAttrCollector.addDefaultAttribute(localName, value);
    }

    /*
    ///////////////////////////////////////////////////
    // Support for NsDefaultProvider
    ///////////////////////////////////////////////////
     */

    public boolean isPrefixLocallyDeclared(String internedPrefix) {
        // should never be called... but let's not care if it is
        return false;
    }

    public void addNsBinding(String prefix, String uri) {
        // should never be called... but let's just ignore it.
    }

    /*
    ///////////////////////////////////////////////////
    // Accessors:
    ///////////////////////////////////////////////////
     */

    public final boolean isNamespaceAware() {
        return false;
    }

    // // // Generic stack information:

    public final int getDepth() { return mSize; }

    public final boolean isEmpty() {
        return mSize == 0;
    }

    // // // Information about element at top of stack:

    public final String getDefaultNsURI() {
        return null;
    }

    public final String getNsURI() {
        return null;
    }

    public final String getPrefix() {
        return null;
    }

    public final String getLocalName() {
        if (mSize == 0) {
            throw new IllegalStateException("Illegal access, empty stack.");
        }
        return mElements[mSize-1];
    }

    public final boolean matches(String prefix, String localName)
    {
        if (mSize == 0) {
            throw new IllegalStateException("Illegal access, empty stack.");
        }
        if (prefix != null && prefix.length() > 0) {
            return false;
        }
        String thisName = mElements[mSize-1];
        return (thisName == localName) || thisName.equals(localName);
    }

    public final String getTopElementDesc() {
        if (mSize == 0) {
            throw new IllegalStateException("Illegal access, empty stack.");
        }
        return mElements[mSize-1];
    }

    // // // Namespace information:

    public final int getTotalNsCount() {
        return 0;
    }

    /**
     * @return Number of active prefix/namespace mappings for current scope,
     *   NOT including mappings from enclosing elements.
     */
    public final int getCurrentNsCount() {
        return 0;
    }

    public final String getLocalNsPrefix(int index) { 
        throwIllegalIndex(index);
        return null;
    }

    public final String getLocalNsURI(int index) { 
        throwIllegalIndex(index);
        return null;
    }

    private static void throwIllegalIndex(int index) {
        throw new IllegalArgumentException("Illegal namespace index "+index
                                           +"; current scope has no namespace declarations.");
    }
}
