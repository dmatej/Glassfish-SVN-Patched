package com.ctc.wstx.sr;

import java.io.IOException;
import java.util.Arrays;

import javax.xml.XMLConstants;
import javax.xml.namespace.QName;
import javax.xml.stream.XMLStreamException;

import com.ctc.wstx.api.ReaderConfig;
import com.ctc.wstx.cfg.ErrorConsts;
import com.ctc.wstx.compat.QNameCreator;
import com.ctc.wstx.sw.XmlWriter;
import com.ctc.wstx.util.DataUtil;
import com.ctc.wstx.util.StringVector;
import com.ctc.wstx.util.TextBuilder;

/**
 * Attribute collector class used in namespace-aware parsing mode.
 *<p>
 * Some notes about low-level raw hashing map implementation. Hash area
 * is divided into two sections: first (of size 2^N) is somewhat usual
 * set of indexes to actual attribute names (values in these slots are
 * 'entry + 1', since 0 means 'empty'). There is just one int entry
 * per name; actual name comparison is done using secondary tables that
 * contain (interned) name components.
 * Secondary area will be used for spills, and it is initially sized to
 * have N/8 entries; and since there are 2 ints per entry, size will actually
 * be N/4, following right after the primary hash area.
 *<p>
 * Note: only public for testing purposes
 */
public final class NsAttributeCollector
    extends AttributeCollector
{
    /**
     * Initial size for attribute NS URI buffer
     */
    protected final static int ATTR_URI_BUF_SIZE = 16;

    /**
     * Expected typical maximum number of namespace declarations for any
     * elmement;
     * chosen to minimize need to resize, while trying not to waste space.
     */
    final static int EXP_NS_COUNT = 8;
    
    /*
    //////////////////////////////////////////
    // Collected namespace information:
    //////////////////////////////////////////
     */

    /**
     * TextBuilder into which values of namespace URIs are added (including
     * URI for the default namespace, if one defined).
     */
    private final TextBuilder mNamespaceURIs = new TextBuilder(EXP_NS_COUNT);

    /**
     * StringVector in which namespace prefixes are added; can contain a
     * single null to indicate the default namespace entry
     */
    private final StringVector mNsPrefixes = new StringVector(EXP_NS_COUNT);

    /**
     * Flag to indicate whether the default namespace has already been declared
     * for the current element.
     */
    private boolean mDefaultNsDeclared = false;

    /*
    //////////////////////////////////////////
    // Resolved (derived) attribute information:
    //////////////////////////////////////////
     */

    /**
     * Array in which fully resolved attribute namespace URIs are added,
     * when resolved.
     */
    private String[] mAttrURIs = null;

    /*
    ///////////////////////////////////////////////
    // Life-cycle:
    ///////////////////////////////////////////////
     */

    public NsAttributeCollector(ReaderConfig cfg)
    {
        super(cfg);
    }

    /**
     * Method called to allow reusing of collector, usually right before
     * starting collecting attributes for a new start tag.
     *<p>
     * Note: public only so that it can be called by unit tests.
     */
    public void reset()
    {
        mNamespaceURIs.reset();
        mDefaultNsDeclared = false;
        mNsPrefixes.clear(false);

        /* No need to clear attr name, or NS prefix Strings; they are
         * canonicalized and will be referenced by symbol table in any
         * case... so we can save trouble of cleaning them up. This Object
         * will get GC'ed soon enough, after parser itself gets disposed of.
         */
        if (mAttrCount > 0) {
            mAttrNames.clear(false);
            mValueBuffer.reset();
            mAttrCount = 0;
            if (mXmlIdAttrIndex >= 0) {
                mXmlIdAttrIndex = XMLID_IX_NONE;
            }
        }

        /* Note: attribute values will be cleared later on, when validating
         * namespaces. This so that we know how much to clean up; and
         * occasionally can also just avoid clean up (when resizing)
         */
    }

    /**
     * Method called to resolve namespace URIs from attribute prefixes.
     *<p>
     * Note: public only so that it can be called by unit tests.
     *
     * @param rep Reporter to use for reporting well-formedness problems
     * @param ns Namespace prefix/URI mappings active for this element
     *
     * @return Index of xml:id attribute, if any, -1 if not
     */
    public int resolveNamespaces(InputProblemReporter rep, StringVector ns)
        throws XMLStreamException
    {
        int attrCount = mAttrCount;

        /* Let's now set number of 'real' attributes, to allow figuring
         * out number of attributes created via default value expansion
         */
        mNonDefCount = attrCount;

        if (attrCount < 1) {
            // Checked if doing access by FQN:
            mAttrHashSize = mAttrSpillEnd = 0;
            // And let's just bail out, too...
            return mXmlIdAttrIndex;
        }

        // Need to have room for URIs:
        String[] attrURIs = mAttrURIs;
        if (attrURIs == null || attrURIs.length < attrCount) {
            int size = (attrCount < ATTR_URI_BUF_SIZE) ? ATTR_URI_BUF_SIZE : attrCount;
            mAttrURIs = attrURIs = new String[size];
        }
        String[] attrNames = mAttrNames.getInternalArray();
        for (int i = 0; i < attrCount; ++i) {
            String prefix = attrNames[i+i];
            // Attributes do NOT use default namespace:
            if (prefix == null) {
                attrURIs[i] = null;
                // xml:lang etc? fixed mapping
            } else if (prefix == "xml") {
                attrURIs[i] = XMLConstants.XML_NS_URI;
            } else {
                String uri = ns.findLastFromMap(prefix);
                if (uri == null) {
                    rep.throwParseError(ErrorConsts.ERR_NS_UNDECLARED_FOR_ATTR, 
                                        prefix, attrNames[i+i+1]);
                }
                attrURIs[i] = uri;
            }
        }
        // Also, do we need to clear values first?
        if (mAttrValues != null) {
            // If array is too small, let's just discard it now:
            if (mAttrValues.length < attrCount) {
                mAttrValues = null;
            } else {
                // Otherwise, need to clear value entries from last element
                for (int i = 0; i < attrCount; ++i) {
                    mAttrValues[i] = null;
                }
            }
        }

        /* Ok, finally, let's create attribute map, to allow efficient
         * access by prefix+localname combination. Could do it on-demand,
         * but this way we can check for duplicates right away.
         */
        int[] map = mAttrMap;

        /* What's minimum size to contain at most 80% full hash area,
         * plus 1/8 spill area (12.5% spilled entries, two ints each)?
         */
        int hashCount = 4;
        {
            int min = attrCount + (attrCount >> 2); // == 80% fill rate
            /* Need to get 2^N size that can contain all elements, with
             * 80% fill rate
             */
            while (hashCount < min) {
                hashCount += hashCount; // 2x
            }
            // And then add the spill area
            mAttrHashSize = hashCount;
            min = hashCount + (hashCount >> 4); // 12.5 x 2 ints
            if (map == null || map.length < min) {
                map = new int[min];
            } else {
                /* Need to clear old hash entries (if any). But note that
                 * spilled entries we can leave alone -- they are just ints,
                 * and get overwritten if and as needed
                 */
                Arrays.fill(map, 0, hashCount, 0);
            }
        }

        {
            int mask = hashCount-1;
            int spillIndex = hashCount;

            // Ok, array's fine, let's hash 'em in!
            for (int i = 0; i < attrCount; ++i) {
                String uri = attrURIs[i];
                String name = attrNames[i+i+1];
                int hash = name.hashCode();
                if (uri != null) {
                    hash ^= uri.hashCode();
                }
                int index = hash & mask;
                // Hash slot available?
                if (map[index] == 0) {
                    map[index] = i+1; // since 0 is marker
                } else {
                    int currIndex = map[index]-1;
                    /* nope, need to spill; let's extract most of that code to
                     * a separate method for clarity (and maybe it'll be
                     * easier to inline by JVM too)
                     */
                    map = spillAttr(uri, name, map, currIndex, spillIndex,
                                    attrCount, hash, hashCount);
                    if (map == null) {
                        throwDupAttr(rep, currIndex);
                        // never returns here...
                    } else { // let's use else to keep FindBugs happy
                        map[++spillIndex] = i; // no need to specifically avoid 0
                        ++spillIndex;
                    }
                }
            }
            mAttrSpillEnd = spillIndex;
        }
        mAttrMap = map;
        return mXmlIdAttrIndex;
    }

    /*
    ///////////////////////////////////////////////
    // Public accesors (for stream reader)
    ///////////////////////////////////////////////
     */

    /**
     * @return Number of 'new' namespace declarations collected
     */
    protected int getNsCount() {
        return mNamespaceURIs.size();
    }

    public boolean hasDefaultNs() {
        return mDefaultNsDeclared;
    }

    public String getNsPrefix(int index) {
        return mNsPrefixes.getString(index);
    }

    public String getNsURI(int index) {
        return mNamespaceURIs.getEntry(index);
    }

    // // // Direct access to attribute/NS prefixes/localnames/URI

    public String getPrefix(int index) {
        if (index < 0 || index >= mAttrCount) {
            throwIndex(index);
        }
        return mAttrNames.getString(index << 1);
    }

    public String getLocalName(int index) {
        if (index < 0 || index >= mAttrCount) {
            throwIndex(index);
        }
        return mAttrNames.getString((index << 1) + 1);
    }

    public String getURI(int index) {
        if (index < 0 || index >= mAttrCount) {
            throwIndex(index);
        }
        return mAttrURIs[index];
    }

    /**
     * Note: caller must check validity of the index prior to calling this
     * method.
     */
    public QName getQName(int index)
    {
        String prefix = getPrefix(index);
        if (prefix == null) { // QName barfs on null...
            prefix = "";
        }
        return QNameCreator.create(getURI(index), getLocalName(index), prefix);
    }

    public String getValue(String nsURI, String localName)
    {
        // Primary hit?
        int hashSize = mAttrHashSize;
        if (hashSize == 0) { // sanity check, for 'no attributes'
            return null;
        }
        int hash = localName.hashCode();
        boolean hasURI = (nsURI != null) && (nsURI.length() > 0);
        if (hasURI) {
            hash ^= nsURI.hashCode();
        }
        int ix = mAttrMap[hash & (hashSize-1)];
        if (ix == 0) { // nothing in here; no spills either
            return null;
        }
        --ix;

        // Is primary candidate match?
        String thisName = mAttrNames.getString(ix+ix+1);
        /* Equality first, since although equals() checks that too, it's
         * very likely to match (if interning Strings), and we can save
         * a method call.
         */
        if (thisName == localName || thisName.equals(localName)) {
            String thisURI = mAttrURIs[ix];
            if (hasURI) {
                if (nsURI == thisURI || nsURI.equals(thisURI)) {
                    return getValue(ix);
                }
            } else {
                if (thisURI == null || thisURI.length() == 0) {
                    return getValue(ix);
                }
            }
        }

        /* Nope, need to traverse spill list, which has 2 entries for
         * each spilled attribute id; first for hash value, second index.
         */
        for (int i = hashSize, len = mAttrSpillEnd; i < len; i += 2) {
            if (mAttrMap[i] != hash) {
                continue;
            }
            /* Note: spill indexes are not off-by-one, since there's no need
             * to mask 0
             */
            ix = mAttrMap[i+1];
            thisName = mAttrNames.getString(ix+ix+1);
            if (thisName == localName || thisName.equals(localName)) {
                String thisURI = mAttrURIs[ix];
                if (hasURI) {
                    if (nsURI == thisURI || nsURI.equals(thisURI)) {
                        return getValue(ix);
                    }
                } else {
                    if (thisURI == null) {
                        return getValue(ix);
                    }
                }
            }
        }

        return null;
    }

    public int findIndex(String nsURI, String localName)
    {
        /* Note: most of the code is from getValue().. could refactor
         * code, performance is bit of concern (one more method call
         * if index access was separate).
         * See comments on that method, for logics.
         */

        // Primary hit?
        int hashSize = mAttrHashSize;
        if (hashSize == 0) { // sanity check, for 'no attributes'
            return -1;
        }
        int hash = localName.hashCode();
        boolean hasURI = (nsURI != null) && (nsURI.length() > 0);
        if (hasURI) {
            hash ^= nsURI.hashCode();
        }
        int ix = mAttrMap[hash & (hashSize-1)];
        if (ix == 0) { // nothing in here; no spills either
            return -1;
        }
        --ix;

        // Is primary candidate match?
        String thisName = mAttrNames.getString(ix+ix+1);
        if (thisName == localName || thisName.equals(localName)) {
            String thisURI = mAttrURIs[ix];
            if (hasURI) {
                if (nsURI == thisURI || nsURI.equals(thisURI)) {
                    return ix;
                }
            } else {
                if (thisURI == null) {
                    return ix;
                }
            }
        }

        /* Nope, need to traverse spill list, which has 2 entries for
         * each spilled attribute id; first for hash value, second index.
         */
        for (int i = hashSize, len = mAttrSpillEnd; i < len; i += 2) {
            if (mAttrMap[i] != hash) {
                continue;
            }
            /* Note: spill indexes are not off-by-one, since there's no need
             * to mask 0
             */
            ix = mAttrMap[i+1];
            thisName = mAttrNames.getString(ix+ix+1);
            if (thisName == localName || thisName.equals(localName)) {
                String thisURI = mAttrURIs[ix];
                if (hasURI) {
                    if (nsURI == thisURI || nsURI.equals(thisURI)) {
                        return ix;
                    }
                } else {
                    if (thisURI == null) {
                        return ix;
                    }
                }
            }
        }
        return -1; // no such attribute
    }

    /**
     * @return null if the default namespace URI has been already declared
     *   for the current element; TextBuilder to add URI to if not.
     */
    public TextBuilder getDefaultNsBuilder()
    {
        if (mDefaultNsDeclared) {
            return null;
        }
        mDefaultNsDeclared = true;
        mNsPrefixes.addString(null);
        return mNamespaceURIs;
    }

    /**
     * @return null if prefix has been already declared; TextBuilder to
     *   add value to if not.
     */
    public TextBuilder getNsBuilder(String prefix)
    {
        if (mNsPrefixes.containsInterned(prefix)) {
            return null;
        }
        mNsPrefixes.addString(prefix);
        return mNamespaceURIs;
    }

    /**
     * Method called to get TextBuilder instance, into which value
     * String should be built, when starting to read attribute
     * value.
     *<p>
     * Note: It is assumed that all Strings have been canonicalized
     * via default symbol table
     *
     * @param attrPrefix canonicalized attribute prefix
     * @param attrLocalName canonicalized local name of attribute
     */
    public TextBuilder getAttrBuilder(String attrPrefix, String attrLocalName)
    {
        // 'normal' attribute:
        if (mAttrCount == 0) {
            if (mValueBuffer == null) {
                allocBuffers();
            }
            mAttrCount = 1;
        } else {
            ++mAttrCount;
        }
        mAttrNames.addStrings(attrPrefix, attrLocalName);
        // 25-Sep-2006, TSa: Need to keep track of xml:id attribute?
        if (attrPrefix == "xml" && attrLocalName == "id") {
            if (mXmlIdAttrIndex != XMLID_IX_DISABLED) {
                mXmlIdAttrIndex = mAttrCount - 1;
            }
        }
        /* Can't yet create attribute map by name, since we only know
         * name prefix, not necessarily matching URI.
         */ 
        return mValueBuffer;
    }

    /**
     *<p>
     * Note: only called by {@link InputElementStack}
     */
    protected String[] getNsPrefixes() {
        return mNsPrefixes.getInternalArray();
    }

    /**
     *<p>
     * Note: only called by {@link InputElementStack}
     */
    public TextBuilder getNsURIs() {
        return mNamespaceURIs;
    }

    /**
     *<p>
     * Note: only called by {@link InputElementStack}
     */
    protected String[] getAttrURIs() {
        return mAttrURIs;
    }

    /**
     * Method needed by event creating code, to build a non-transient
     * attribute container, to use with XMLEvent objects (specifically
     * implementation of StartElement event).
     */
    public ElemAttrs buildAttrOb()
    {
        int count = mAttrCount;
        if (count == 0) {
            return null;
        }
        /* If we have actual attributes, let's first just create the
         * raw array that has all attribute information:
         */
        String[] raw = new String[count << 2];
        for (int i = 0; i < count; ++i) {
            int ix = (i << 2);
            raw[ix] = mAttrNames.getString(i + i + 1);
            raw[ix+1] = mAttrURIs[i];
            raw[ix+2] = mAttrNames.getString(i + i);
            raw[ix+3] = getValue(i);
        }

        // Do we have a "short" list?
        if (count < LONG_ATTR_LIST_LEN) {
            return new ElemAttrs(raw, mNonDefCount);
        }

        // Ok, nope; we need to also pass the Map information...
        /* 02-Feb-2009, TSa: Must make a copy of the Map array now,
         *   otherwise could get overwritten.
         */
        int amapLen = mAttrMap.length;
        int[] amap = new int[amapLen];
        // TODO: JDK 1.6 has Arrays.copyOf(), should use with Woodstox 6
        System.arraycopy(mAttrMap, 0, amap, 0, amapLen);
        return new ElemAttrs(raw, mNonDefCount,
                             amap, mAttrHashSize, mAttrSpillEnd);
    }

    /*
    ///////////////////////////////////////////////
    // Validation methods:
    ///////////////////////////////////////////////
     */

    /**
     * Method called by validator to insert an attribute that has a default
     * value and wasn't yet included in collector's attribute set.
     *
     * @return Index of the newly added attribute, if added; -1 to indicate
     *    this was a duplicate
     */
    public int addDefaultAttribute(String localName, String uri, String prefix,
                                   String value)
    {
        int attrIndex = mAttrCount;
        if (attrIndex < 1) {
            /* had no explicit attributes... better initialize now, then.
             * Let's just use hash area of 4, and 
             */
            initHashArea();
        }

        /* Ok, first, since we do want to verify that we can not accidentally
         * add duplicates, let's first try to add entry to Map, since that
         * will catch dups.
         */
        int hash = localName.hashCode();
        if (uri.length() > 0) {
            hash ^= uri.hashCode();
        }
        int index = hash & (mAttrHashSize - 1);
        int[] map = mAttrMap;
        if (map[index] == 0) { // whoa, have room...
            map[index] = attrIndex+1; // add 1 to get 1-based index (0 is empty marker)
        } else { // nah, collision...
            int currIndex = map[index]-1; // Index of primary collision entry
            int spillIndex = mAttrSpillEnd;
            map = spillAttr(uri, localName, map, currIndex, spillIndex,
                            attrIndex, hash, mAttrHashSize);
            if (map == null) { // dup!
                return -1; // could return negation (-(index+1)) of the prev index?
            }
            map[++spillIndex] = attrIndex; // no need to specifically avoid 0
            mAttrMap = map;
            mAttrSpillEnd = ++spillIndex;
        }

        // And then, finally, let's add the entry to Lists:
        mAttrNames.addStrings(prefix, localName);
        if (mAttrURIs == null) {
            mAttrURIs = new String[ATTR_URI_BUF_SIZE];
        } else if (mAttrCount >= mAttrURIs.length) {
            mAttrURIs = DataUtil.growArrayBy(mAttrURIs, 8);
        }
        mAttrURIs[attrIndex] = uri;
        if (mAttrValues == null) {
            mAttrValues = new String[attrIndex + 8];
        } else if (attrIndex >= mAttrValues.length) {
            mAttrValues = DataUtil.growArrayBy(mAttrValues, 8);
        }
        mAttrValues[attrIndex] = value;

        return mAttrCount++;
    }

    /**
     * Method that basically serializes the specified (read-in) attribute
     * using Writers provided
     */
    public void writeAttribute(int index, XmlWriter xw)
        throws IOException, XMLStreamException
    {
        // Note: here we assume index checks have been done by caller
        int offset = (index << 1);
        String ln = mAttrNames.getString(offset + 1);
        String prefix = mAttrNames.getString(offset);
        if (prefix == null || prefix.length() == 0) {
            xw.writeAttribute(ln, getValue(index));
        } else {
            xw.writeAttribute(prefix, ln, getValue(index));
        }
    }

    /*
    ///////////////////////////////////////////////
    // Internal methods:
    ///////////////////////////////////////////////
     */

    /**
     * @return Null, if attribute is a duplicate (to indicate error);
     *    map itself, or resized version, otherwise.
     */
    private int[] spillAttr(String uri, String name,
                            int[] map, int currIndex, int spillIndex, int attrCount,
                            int hash, int hashCount)
    {
        // Do we have a dup with primary entry?
        /* Can do equality comp for local name, as they
         * are always canonicalized:
         */
        if (mAttrNames.getString(currIndex+currIndex+1) == name) {
            // URIs may or may not be interned though:
            String currURI = mAttrURIs[currIndex];
            if (currURI == uri || (currURI != null && currURI.equals(uri))) {
                return null;
            }
        }

        /* Is there room to spill into? (need to 2 int spaces; one for hash,
         * the other for index)
         */
        if ((spillIndex + 1)>= map.length) {
            // Let's just add room for 4 spills...
            map = DataUtil.growArrayBy(map, 8);
        }
        // Let's first ensure we aren't adding a dup:
        for (int j = hashCount; j < spillIndex; j += 2) {
            if (map[j] == hash) {
                currIndex = map[j+1];
                if (mAttrNames.getString(currIndex+currIndex+1) == name) {
                    String currURI = mAttrURIs[currIndex];
                    if (currURI == uri || (currURI != null && currURI.equals(uri))) {
                        return null;
                    }
                }
            }
        }
        map[spillIndex] = hash;
        return map;
    }

    /**
     * Method called to ensure hash area will be properly set up in
     * cases where initially no room was needed, but default attribute(s)
     * is being added.
     */
    private void initHashArea()
    {
        /* Let's use small hash area of size 4, and one spill; don't
         * want too big (need to clear up room), nor too small (only
         * collisions)
         */
        mAttrHashSize = mAttrSpillEnd = 4;
        if (mAttrMap == null || mAttrMap.length < mAttrHashSize) {
            mAttrMap = new int[mAttrHashSize+1];
        }
        mAttrMap[0] =  mAttrMap[1] = mAttrMap[2] = mAttrMap[3] = 0;
        allocBuffers();
    }
}
