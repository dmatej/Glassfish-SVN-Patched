/* Woodstox XML processor
 *
 * Copyright (c) 2004- Tatu Saloranta, tatu.saloranta@iki.fi
 *
 * Licensed under the License specified in file LICENSE, included with
 * the source code.
 * You may not use this file except in compliance with the License.
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.ctc.wstx.sr;

import java.util.Iterator;

import javax.xml.namespace.NamespaceContext;
import javax.xml.namespace.QName;
import javax.xml.stream.Location;
import javax.xml.stream.XMLStreamException;

import org.codehaus.stax2.AttributeInfo;
import org.codehaus.stax2.validation.ValidationContext;
import org.codehaus.stax2.validation.XMLValidator;
import org.codehaus.stax2.validation.XMLValidationProblem;
import org.codehaus.stax2.validation.XMLValidationSchema;
import org.codehaus.stax2.validation.ValidatorPair;

import com.ctc.wstx.api.ReaderConfig;
import com.ctc.wstx.api.WstxInputProperties;
import com.ctc.wstx.cfg.XmlConsts;
import com.ctc.wstx.dtd.DTDValidatorBase; // unfortunate dependency
import com.ctc.wstx.util.*;

/**
 * Shared base class that defines API stream reader uses to communicate
 * with the element stack implementation, independent of whether it's
 * operating in namespace-aware or non-namespace modes.
 * Element stack class is used for storing nesting information about open
 * elements, and for namespace-aware mode, also information about
 * namespaces active (including default namespace), during parsing of
 * XML input.
 *<p>
 * This class also implements {@link NamespaceContext}, since it has all
 * the information necessary, so parser can just return element stack
 * instance as necesary.
 */
public abstract class InputElementStack
    implements AttributeInfo, NamespaceContext, ValidationContext
{
    final static int ID_ATTR_NONE = -1;

    /*
    //////////////////////////////////////////////////
    // Configuration
    //////////////////////////////////////////////////
    */

    protected final ReaderConfig mConfig;

    protected InputProblemReporter mReporter = null;

    /*
    //////////////////////////////////////////////////
    // Element validation (optional), attribute typing
    //////////////////////////////////////////////////
    */

    /**
     * Optional validator object that will get called if set,
     * and that can validate xml content. Note that it is possible
     * that this is set to a proxy object that calls multiple
     * validators in sequence.
     */
    protected XMLValidator mValidator = null;

    /**
     * Index of the attribute with type of ID, if known (most likely
     * due to Xml:id support); -1 if not available, or no ID attribute
     * for current element.
     */
    protected int mIdAttrIndex = ID_ATTR_NONE;

    /*
    //////////////////////////////////////////////////
    // Life-cycle (create, update state)
    //////////////////////////////////////////////////
     */

    protected InputElementStack(ReaderConfig cfg)
    {
        mConfig = cfg;
    }

    protected void connectReporter(InputProblemReporter rep)
    {
        mReporter = rep;
    }

    protected XMLValidator addValidator(XMLValidator vld)
    {
        if (mValidator == null) {
            mValidator = vld;
        } else {
            mValidator = new ValidatorPair(mValidator, vld);
        }
        return vld;
    }

    /**
     * Method called to connect the automatically handled DTD validator
     * (one detected from DOCTYPE, loaded and completely handled by
     * the stream reader without application calling validation methods).
     * Handled separately, since its behaviour is potentially different
     * from that of explicitly added validators.
     */
    protected abstract void setAutomaticDTDValidator(XMLValidator validator, NsDefaultProvider nsDefs);

    /*
    //////////////////////////////////////////////////
    // Start/stop validation
    //////////////////////////////////////////////////
     */

    public XMLValidator validateAgainst(XMLValidationSchema schema)
        throws XMLStreamException
    {
        /* Should we first check if we maybe already have a validator
         * for the schema?
         */
        return addValidator(schema.createValidator(this));
    }



    public XMLValidator stopValidatingAgainst(XMLValidationSchema schema)
        throws XMLStreamException
    {
        XMLValidator[] results = new XMLValidator[2];
        if (ValidatorPair.removeValidator(mValidator, schema, results)) { // found
            XMLValidator found = results[0];
            mValidator = results[1];
            found.validationCompleted(false);
            return found;
        }
        return null;
    }

    public XMLValidator stopValidatingAgainst(XMLValidator validator)
        throws XMLStreamException
    {
        XMLValidator[] results = new XMLValidator[2];
        if (ValidatorPair.removeValidator(mValidator, validator, results)) { // found
            XMLValidator found = results[0];
            mValidator = results[1];
            found.validationCompleted(false);
            return found;
        }
        return null;
    }

    /*
    //////////////////////////////////////////////////
    // Accessors:
    //////////////////////////////////////////////////
     */

    /**
     * This is a method called by the reader to ensure that we have at
     * least one 'real' validator. This is only needed to ensure that
     * validation problems that the reader can detect (illegal textual
     * content) can be reported as validity errors. Since the validator
     * API does not have a good way to cleanly deal with such a possibility,
     * the check is rather fragile, but should work for now: essentially
     * we need at least one validator object that either is not a sub-class
     * of <code>DTDValidatorBase</code> or returns true for
     * <code>reallyValidating</code>.
     *<p>
     * !!! TODO: remove need for this method (and method itself) with
     * Woodstox 4.0, by adding necessary support in Stax2 XMLValidator
     * interface.
     */
    protected boolean reallyValidating()
    {
        if (mValidator == null) { // no validators, no validation
            // (although, should never get called if no validators)
            return false;
        }
        if (!(mValidator instanceof DTDValidatorBase)) {
            // note: happens for validator pair, for one
            return true;
        }
        return ((DTDValidatorBase) mValidator).reallyValidating();
    }

    /**
     * Method called by {@link BasicStreamReader}, to retrieve the
     * attribute collector it needs for some direct access.
     */
    protected abstract AttributeCollector getAttrCollector();

    /**
     * Method called to construct a non-transient NamespaceContext instance;
     * generally needed when creating events to return from event-based
     * iterators.
     */
    public abstract BaseNsContext createNonTransientNsContext(Location loc);

    /**
     * Method called by the stream reader to add new (start) element
     * into the stack in namespace-aware mode; called when a start element
     * is encountered during parsing, but only in ns-aware mode.
     */
    public abstract void push(String prefix, String localName);

    /**
     * Method called by the stream reader to add new (start) element
     * into the stack in non-namespace mode; called when a start element
     * is encountered during parsing, but only in non-namespace mode.
     */
    public abstract void push(String fullName);

    /**
     * Method called by the stream reader to remove the topmost (start)
     * element from the stack;
     * called when an end element is encountered during parsing.
     *
     * @return True if stack has more elements; false if not (that is,
     *    root element closed)
     */
    public abstract boolean pop()
        throws XMLStreamException;

    /**
     * Method called to resolve element and attribute namespaces (in
     * namespace-aware mode), and do optional validation using pluggable
     * validator object.
     *
     * @return Text content validation state that should be effective
     *   for the fully resolved element context
     */
    public abstract int resolveAndValidateElement()
        throws XMLStreamException;

    /**
     * Method called after parsing (but before returning) end element,
     * to allow for pluggable validators to verify correctness of
     * the content model for the closing element.
     *
     * @return Validation state that should be effective for the parent
     *   element state
     */
    public abstract int validateEndElement()
        throws XMLStreamException;

    /*
    ///////////////////////////////////////////////////
    // AttributeInfo methods (StAX2)
    ///////////////////////////////////////////////////
     */

    public abstract int getAttributeCount();

    /**
     * @return Index of the specified attribute, if the current element
     *   has such an attribute (explicit, or one created via default
     *   value expansion); -1 if not.
     */
    public abstract int findAttributeIndex(String nsURI, String localName);

    /**
     * Default implementation just indicates it does not know of such
     * attributes; this because that requires DTD information that only
     * some implementations have.
     */
    public final int getIdAttributeIndex()
    {
        if (mIdAttrIndex >= 0) {
            return mIdAttrIndex;
        }
        return (mValidator == null) ? -1 : mValidator.getIdAttrIndex();
    }

    /**
     * Default implementation just indicates it does not know of such
     * attributes; this because that requires DTD information that only
     * some implementations have.
     */
    public final int getNotationAttributeIndex()
    {
        return (mValidator == null) ? -1 :
            mValidator.getNotationAttrIndex();
    }

    /*
    ///////////////////////////////////////////////////
    // Implementation of NamespaceContext:
    ///////////////////////////////////////////////////
     */

    public abstract String getNamespaceURI(String prefix);

    public abstract String getPrefix(String nsURI);

    public abstract Iterator getPrefixes(String nsURI);

    /*
    ///////////////////////////////////////////////////
    // ValidationContext
    ///////////////////////////////////////////////////
     */

    public final String getXmlVersion()
    {
        return mConfig.isXml11() ? XmlConsts.XML_V_11_STR : XmlConsts.XML_V_10_STR;
    }

    // Part of Stax2, see above:
    //public int getAttributeCount();

    public String getAttributeLocalName(int index) {
        return getAttrCollector().getLocalName(index);
    }

    public String getAttributeNamespace(int index) {
        return getAttrCollector().getURI(index);
    }

    public String getAttributePrefix(int index) {
        return getAttrCollector().getPrefix(index);
    }

    public String getAttributeValue(int index) {
        return getAttrCollector().getValue(index);
    }

    public String getAttributeValue(String nsURI, String localName)
    {
        int ix = findAttributeIndex(nsURI, localName);
        return (ix < 0) ? null : getAttributeValue(ix);
    }

    // Part of Stax2, see above:
    //public int findAttributeIndex(String nsURI, String localName);

    public boolean isNotationDeclared(String name)
    {
        // !!! TBI
        return false;
    }

    public boolean isUnparsedEntityDeclared(String name)
    {
        // !!! TBI
        return false;
    }

    public String getBaseUri()
    {
        // !!! TBI
        return null;
    }

    public abstract QName getCurrentElementName();

    // This was defined above for NamespaceContext
    //public String getNamespaceURI(String prefix);

    public Location getValidationLocation()
    {
        return mReporter.getLocation();
    }

    public void reportProblem(XMLValidationProblem problem)
        throws XMLStreamException
    {
        mReporter.reportValidationProblem(problem);
    }

    /**
     * Method called by actual validator instances when attributes with
     * default values have no explicit values for the element; if so,
     * default value needs to be added as if it was parsed from the
     * element.
     */
    public abstract int addDefaultAttribute(String localName, String uri, String prefix,
                                            String value);
    /*
    ///////////////////////////////////////////////////
    // Support for NsDefaultProvider
    ///////////////////////////////////////////////////
     */

    /**
     * @return True, if the given prefix ("" for default namespace) was
     *   bound/unbound in the current element (one at the top of the
     *   stack); false if not.
     */
    public abstract boolean isPrefixLocallyDeclared(String internedPrefix);

    public abstract void addNsBinding(String prefix, String uri);

    /*
    ///////////////////////////////////////////////////
    // Support for validation:
    ///////////////////////////////////////////////////
     */

    public final void validateText(TextBuffer tb, boolean lastTextSegment)
        throws XMLStreamException
    {
        tb.validateText(mValidator, lastTextSegment);
    }

    public final void validateText(String contents, boolean lastTextSegment)
        throws XMLStreamException
    {
        mValidator.validateText(contents, lastTextSegment);
    }

    /*
    ///////////////////////////////////////////////////
    // Accessors:
    ///////////////////////////////////////////////////
     */

    // // // Generic properties:

    public abstract boolean isNamespaceAware();

    // // // Generic stack information:


    /**
     * @return Number of open elements in the stack; 0 when parser is in
     *  prolog/epilog, 1 inside root element and so on.
     */
    public abstract int getDepth();

    public abstract boolean isEmpty();

    // // // Information about element at top of stack:

    public abstract String getDefaultNsURI();

    public abstract String getNsURI();

    public abstract String getPrefix();

    public abstract String getLocalName();

    public abstract boolean matches(String prefix, String localName);

    public abstract String getTopElementDesc();

    // // // Namespace information:

    public abstract int getTotalNsCount();

    /**
     * @return Number of active prefix/namespace mappings for current scope,
     *   NOT including mappings from enclosing elements.
     */
    public abstract int getCurrentNsCount();

    public abstract String getLocalNsPrefix(int index);

    public abstract String getLocalNsURI(int index);

    // // // DTD-derived attribute information:

    /**
     * @return Schema (DTD, RNG, W3C Schema) based type of the attribute
     *   in specified index
     */
    public final String getAttributeType(int index)
    {
        if (index == mIdAttrIndex && index >= 0) { // second check to ensure -1 is not passed
            return "ID";
        }
        return (mValidator == null) ? WstxInputProperties.UNKNOWN_ATTR_TYPE : 
            mValidator.getAttributeType(index);
    }

    /*
    ///////////////////////////////////////////////////
    // Internal methods:
    ///////////////////////////////////////////////////
     */

    /**
     * Method called to normalize value of an ID attribute, specified
     * using name xml:id, when support for Xml:id specification enabled.
     */
    protected final void normalizeXmlIdAttr(AttributeCollector ac, int ix)
    {
        // StringUtil has a method, but it works on char arrays...
        TextBuilder attrBuilder = ac.getAttrBuilder();
        char[] attrCB = attrBuilder.getCharBuffer();
        String normValue = StringUtil.normalizeSpaces
            (attrCB, attrBuilder.getOffset(ix),
             attrBuilder.getOffset(ix+1));
        if (normValue != null) {
            ac.setNormalizedValue(ix, normValue);
        }
    }
}
