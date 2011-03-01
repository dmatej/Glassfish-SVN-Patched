package com.ctc.wstx.sr;

import java.io.IOException;

import javax.xml.namespace.QName;
import javax.xml.stream.XMLStreamException;

import com.ctc.wstx.api.ReaderConfig;
import com.ctc.wstx.sw.XmlWriter;
import com.ctc.wstx.util.DataUtil;
import com.ctc.wstx.util.TextBuilder;

/**
 * Attribute collector class used in non-namespace parsing mode; much
 * simpler than the one that has to handle namespaces.
 *<p>
 * Note: only public for testing purposes
 */
public final class NonNsAttributeCollector
    extends AttributeCollector
{
    /*
    ///////////////////////////////////////////////
    // Life-cycle:
    ///////////////////////////////////////////////
     */

    public NonNsAttributeCollector(ReaderConfig cfg)
    {
        super(cfg);
    }

    /**
     * Method called to allow reusing of collector, usually right before
     * starting collecting attributes for a new start tag.
     */
    public void reset()
    {
        if (mAttrCount > 0) { // anything to clean up?
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
     * Method called to by the input element stack when all attributes for
     * the element have been parsed. Now collector can build data structures
     * it needs, if any.
     *
     * @return Index of xml:id attribute, if any, -1 if not
     */
    public int resolveValues(InputProblemReporter rep)
        throws XMLStreamException
    {
        int attrCount = mAttrCount;

        /* Let's now set number of 'real' attributes, to allow figuring
         * out number of attributes created via default value expansion
         */
        mNonDefCount = attrCount;

        if (attrCount < 1) {
            // Checked if doing access by FQN:
            mAttrHashSize = 0;
            // And let's just bail out, too...
            return mXmlIdAttrIndex;
        }
        String[] attrNames = mAttrNames.getInternalArray();

        // Also, we better clean up values now
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

        /* And then let's create attribute map, to allow efficient storage
         * and access by name, without creating full hash map instance.
         * Could do it on-demand,
         * but this way we can check for duplicates right away.
         */
        int[] map = mAttrMap;

        /* What's minimum size, to contain at most 80% full hash area,
         * plus 1/8 spill area (12.5% spilled entries, two ints each)?
         */
        int hashCount = 4;
        {
            int min = attrCount + (attrCount >> 2); // == 80% fill rate
            while (hashCount < min) {
                hashCount += hashCount; // 2x
            }
            // And then the spill area
            mAttrHashSize = hashCount;
            min = hashCount + (hashCount >> 4); // 12.5 x 2 ints
            if (map == null || map.length < min) {
                map = new int[min];
            } else { // need to clear old hash entries:
                for (int i = 0; i < hashCount; ++i) {
                    map[i] = 0;
                }
            }
        }

        {
            int mask = hashCount-1;
            int spillIndex = hashCount;

            // Ok, array's fine, let's hash 'em in!
            for (int i = 0; i < attrCount; ++i) {
                String name = attrNames[i];
                int hash = name.hashCode();
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
                    map = spillAttr(name, map, currIndex, spillIndex, attrCount,
                                    hash, hashCount);
                    if (map == null) {
                        throwDupAttr(rep, currIndex);
                        // won't return, but let's use else so FindBugs won't mind
                    } else {
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

    protected int getNsCount() {
        return 0;
    }

    public String getNsPrefix(int index) {
        return null;
    }

    public String getNsURI(int index) {
        return "";
    }

    // // // Direct access to attribute/NS prefixes/localnames/URI

    public String getPrefix(int index) {
        if (index < 0 || index >= mAttrCount) {
            throwIndex(index);
        }
        return null;
    }

    public String getLocalName(int index) {
        if (index < 0 || index >= mAttrCount) {
            throwIndex(index);
        }
        return mAttrNames.getString(index);
    }

    public String getURI(int index)
    {
        if (index < 0 || index >= mAttrCount) {
            throwIndex(index);
        }
        return "";
    }

    public QName getQName(int index) {
        return new QName(getLocalName(index));
    }

    public String getValue(String nsURI, String localName)
    {
        /* Not allowed to have a namespace, except let's not mind the
         * default (empty) namespace URI
         */
        if (nsURI != null && nsURI.length() > 0) {
            return null;
        }

        // Primary hit?
        int hashSize = mAttrHashSize;
        if (hashSize == 0) { // sanity check, for 'no attributes'
            return null;
        }
        int hash = localName.hashCode();
        int ix = mAttrMap[hash & (hashSize-1)];
        if (ix == 0) { // nothing in here; no spills either
            return null;
        }
        --ix;

        // Is primary candidate match?
        String thisName = mAttrNames.getString(ix);
        /* Equality first, since although equals() checks that too, it's
         * very likely to match (if interning Strings), and we can save
         * a method call.
         */
        if (thisName == localName || thisName.equals(localName)) {
            return getValue(ix);
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
            thisName = mAttrNames.getString(ix);
            if (thisName == localName || thisName.equals(localName)) {
                return getValue(ix);
            }
        }

        return null;
    }

    public int findIndex(String localName)
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
        int ix = mAttrMap[hash & (hashSize-1)];
        if (ix == 0) { // nothing in here; no spills either
            return -1;
        }
        --ix;

        // Is primary candidate match?
        String thisName = mAttrNames.getString(ix);
        if (thisName == localName || thisName.equals(localName)) {
            return ix;
        }

        /* Nope, need to traverse spill list, which has 2 entries for
         * each spilled attribute id; first for hash value, second index.
         */
        for (int i = hashSize, len = mAttrSpillEnd; i < len; i += 2) {
            if (mAttrMap[i] != hash) {
                continue;
            }
            ix = mAttrMap[i+1];
            thisName = mAttrNames.getString(ix);
            if (thisName == localName || thisName.equals(localName)) {
                return ix;
            }
        }
        return -1;
    }

    public TextBuilder getDefaultNsBuilder()
    {
        throwInternal();
        return null;
    }

    /**
     * @return null if prefix has been already declared; TextBuilder to
     *   add value to if not.
     */
    public TextBuilder getNsBuilder(String localName)
    {
        throwInternal();
        return null;
    }

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
        mAttrNames.addString(attrLocalName);
        // 25-Sep-2006, TSa: Need to keep track of xml:id attribute?
        if (attrLocalName == "xml:id") {
            if (mXmlIdAttrIndex != XMLID_IX_DISABLED) {
                mXmlIdAttrIndex = mAttrCount - 1;
            }
        }
        return mValueBuffer;
    }

    public TextBuilder getNsURIs() {
        throwInternal();
        return null;
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
            raw[ix] = mAttrNames.getString(i);
            raw[ix+1] = null; // uri
            raw[ix+2] = null; // prefix
            raw[ix+3] = getValue(i);
        }

        // Do we have a "short" list?
        if (count < LONG_ATTR_LIST_LEN) {
            return new ElemAttrs(raw, mNonDefCount);
        }

        // Ok, nope; we need to also pass the Map information...
        return new ElemAttrs(raw, mNonDefCount,
                             mAttrMap, mAttrHashSize, mAttrSpillEnd);
                             
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
    public int addDefaultAttribute(String localName, String value)
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
        int index = hash & (mAttrHashSize - 1);
        int[] map = mAttrMap;
        if (map[index] == 0) { // whoa, have room...
            map[index] = attrIndex+1; // add 1 to get 1-based index (0 is empty marker)
        } else { // nah, collision...
            int currIndex = map[index]-1; // Index of primary collision entry
            int spillIndex = mAttrSpillEnd;
            map = spillAttr(localName, map, currIndex, spillIndex, attrIndex,
                            hash, mAttrHashSize);
            if (map == null) { // dup!
                return -1; // could return negation (-(index+1)) of the prev index?
            }
            map[++spillIndex] = attrIndex; // no need to specifically avoid 0
            mAttrMap = map;
            mAttrSpillEnd = ++spillIndex;
        }

        // And then, finally, let's add the entry to Lists:
        mAttrNames.addString(localName); // First, the name
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
        String ln = mAttrNames.getString(index);
        xw.writeAttribute(ln, getValue(index));
    }

    /*
    ///////////////////////////////////////////////
    // Internal methods:
    ///////////////////////////////////////////////
     */

    private void throwInternal() {
        throw new IllegalStateException("Internal error: shouldn't call this method.");
    }

    /**
     * @return Null, if attribute is a duplicate (to indicate error);
     *    map itself, or resized version, otherwise.
     */
    private int[] spillAttr(String name,
                            int[] map, int currIndex, int spillIndex, int attrCount,
                            int hash, int hashCount)
    {
        // Do we have a dup with primary entry?
        /* Can do equality comp for local name, as they
         * are always canonicalized:
         */
        if (mAttrNames.getString(currIndex) == name) {
            return null;
        }

        /* Is there room to spill into? (need to have 2 int spaces; one for
         * hash, the other for index)
         */
        if ((spillIndex + 1) >= map.length) {
            // Let's just add room for 4 spills...
            map = DataUtil.growArrayBy(map, 8);
        }
        // Let's first ensure we aren't adding a dup:
        for (int j = hashCount; j < spillIndex; j += 2) {
            if (map[j] == hash) {
                currIndex = map[j+1];
                if (mAttrNames.getString(currIndex) == name) {
                    return null;
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
