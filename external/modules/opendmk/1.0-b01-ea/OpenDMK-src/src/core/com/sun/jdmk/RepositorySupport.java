/*
 * @(#)file      RepositorySupport.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.52
 * @(#)date      07/04/04
 *
 * 
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright (c) 2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * The contents of this file are subject to the terms of either the GNU General
 * Public License Version 2 only ("GPL") or the Common Development and
 * Distribution License("CDDL")(collectively, the "License"). You may not use
 * this file except in compliance with the License. You can obtain a copy of the
 * License at http://opendmk.dev.java.net/legal_notices/licenses.txt or in the 
 * LEGAL_NOTICES folder that accompanied this code. See the License for the 
 * specific language governing permissions and limitations under the License.
 * 
 * When distributing the software, include this License Header Notice in each
 * file and include the License file found at
 *     http://opendmk.dev.java.net/legal_notices/licenses.txt
 * or in the LEGAL_NOTICES folder that accompanied this code.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.
 * 
 * If applicable, add the following below the License Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * 
 *       "Portions Copyrighted [year] [name of copyright owner]"
 * 
 * Contributor(s):
 * 
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding
 * 
 *       "[Contributor] elects to include this software in this distribution
 *        under the [CDDL or GPL Version 2] license."
 * 
 * If you don't indicate a single choice of license, a recipient has the option
 * to distribute your version of this file under either the CDDL or the GPL
 * Version 2, or to extend the choice of license to its licensees as provided
 * above. However, if you add GPL Version 2 code and therefore, elected the
 * GPL Version 2 license, then the option applies only if the new code is made
 * subject to such option by the copyright holder.
 * 
 */
package com.sun.jdmk;


// java import
import java.util.Hashtable;
import java.util.Enumeration;
import java.util.Set;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Vector;

// RI import
import javax.management.* ; 
import com.sun.jdmk.internal.ClassLogger;


/**
 * The RepositorySupport  implements the Repository interface.
 * This repository does not support persistency.
 * @deprecated This class is kept as a utility class, though it is no longer
 *      used. It may not be supported in future releases.
 */
public class RepositorySupport  implements  Repository { 
    
    // Private fields -------------------------------------------->

    /**
     * An object name for query describing the whole set of mbeans.
     * Optimization helper for queries.
     */
    private final static ObjectName _WholeWordQueryObjectName;
    static {
        try {
            _WholeWordQueryObjectName = new ObjectName("*:*");
        } catch (MalformedObjectNameException e) {
            throw new UnsupportedOperationException(e.getMessage());
        }
    }
    
    /**
     * two int utilities to minimize wildmatch method stack frame overhead
     * during recursions.
     */
    private static int _slen;
    private static int _plen;

    /**
     * The structure for storing the objects is very basic .
     * A Hashtable is used for storing the different domains
     * For each domain, a hashtable contains the instances with
     * canonical key property list string as key and named object
     * aggregated from given object name and mbean instance as value.
     */
    private final Hashtable domainTb;
    
    /**
     * Number of elements contained in the Repository
     */
    private int nbElements = 0;
  
    /**
     * Domain name of the server the repository is attached to.
     * It is quicker to store the information in the repository rather
     * than querying the framework each time the info is required.
     */
    private String domain = ServiceName.DOMAIN;
    
    /** The name of this class to be used for tracing */
    private final static String dbgTag = "Repository";

    // Private fields <=============================================
    

    // Private methods --------------------------------------------->

    // TRACES & DEBUG
    //---------------
    private final ClassLogger logger = 
        new ClassLogger(ClassLogger.LOGGER_MBEANSERVER,
                        "RepositorySupport");

    
    /* This class is used to match an ObjectName against a pattern. */
    private final static class ObjectNamePattern {
        private final char[]   domain;
        private final String[] keys;
        private final String[] values;
        private final String   properties;
        private final boolean  isPropertyPattern;
        
        /**
         * The ObjectName pattern against which ObjectNames are matched.
         **/
        public  final  ObjectName pattern;
        
        /**
         * Builds a new ObjectNamePattern object from an ObjectName pattern.
         * @param pattern The ObjectName pattern under examination.
         **/
        public ObjectNamePattern(ObjectName pattern) {
            this(pattern.isPattern(),pattern.getDomain(),
                 pattern.isPropertyPattern(),
                 pattern.getCanonicalKeyPropertyListString(),
                 pattern.getKeyPropertyList(),pattern);
        }

        /**
         * Builds a new ObjectNamePattern object from an ObjectName pattern
         * constituents.
         * @param domainPattern pattern.isPattern().
         * @param domain pattern.getDomain().
         * @param propertyPattern pattern.isPropertyPattern().
         * @param canonicalProps pattern.getCanonicalKeyPropertyListString()
         * @param keyPropertyList pattern.getKeyPropertyList()
         * @param pattern The ObjectName pattern under examination.
         **/
        ObjectNamePattern(boolean domainPattern, String domain, 
                          boolean propertyPattern, String canonicalProps,
                          Hashtable keyPropertyList, ObjectName pattern) {
            final int len = (keyPropertyList==null?0:keyPropertyList.size());
            final Enumeration e = 
                (keyPropertyList==null?null:keyPropertyList.keys());
            this.domain = (domain == null?null:domain.toCharArray());
            this.keys   = new String[len];
            this.values = new String[len];
            for (int i = 0 ; i < len ; i++ ) {
                final String k = (String)e.nextElement();
                keys[i]   = k;
                values[i] = (String)keyPropertyList.get(k);
            }
            this.properties = canonicalProps;
            this.isPropertyPattern = propertyPattern;
            this.pattern = pattern;
        }

        /**
         * Return true if the given ObjectName matches the ObjectName pattern
         * for which this object has been built.
         * WARNING: domain name is not considered here because it is supposed
         *          not to be wildcard when called. PropertyList is also 
         *          supposed not to be zero-length.
         * @param name The ObjectName we want to match against the pattern.
         * @return true if <code>name</code> matches the pattern.
         **/
        public boolean matchKeys(ObjectName name) {
            if (isPropertyPattern) {
                // Every property inside pattern should exist in name 
                for (int i= keys.length -1; i >= 0 ; i--) {

                    // find value in given object name for key at current 
                    // index in receiver
                    String v = name.getKeyProperty(keys[i]);

                    // did we find a value for this key ?
                    if (v == null) return false; 
                    
                    // if this property is ok (same key, same value), 
                    // go to next
                    if (v.equals(values[i])) continue; 
                    return false;
                }
                return true;
            } else {
                if (keys.length != name.getKeyPropertyList().size()) 
                    return false;
                final String p1 = name.getCanonicalKeyPropertyListString();
                final String p2 = properties;
                // if (p1 == null) return (p2 == null);
                // if (p2 == null) return p1.equals("");
                return (p1.equals(p2));
            }
        }
    }

    /**
     * Add all the matching objects from the given hashtable in the 
     * result set for the given ObjectNamePattern
     * Do not check whether the domains match (only check for matching
     * key property lists - see <i>matchKeys()</i>)
     **/
    private final void addAllMatching(final Hashtable moiTb, final Set result,
                                      final ObjectNamePattern pattern) {
        synchronized (moiTb) {
            for (Enumeration e = moiTb.elements(); e.hasMoreElements();) {   
                final NamedObject no = (NamedObject) e.nextElement();
                final ObjectName on = no.getName();

                // if all couples (property, value) are contained 
                if (pattern.matchKeys(on)) result.add(no);
            }
        }
    }
    
    private final void addNewDomMoi(final Object object, final String dom, 
                                    final ObjectName name) {
        final Hashtable moiTb= new Hashtable();
        domainTb.put(dom, moiTb);
        moiTb.put(name.getCanonicalKeyPropertyListString(), 
                  new NamedObject(name, object));
        nbElements++;
    }
    
    /** Match a string against a shell-style pattern.  The only pattern
        characters recognised are <code>?</code>, standing for any one
        character, and <code>*</code>, standing for any string of
        characters, including the empty string.

        @param str the string to match, as a character array.
        @param pat the pattern to match the string against, as a
        character array.

        @return true if and only if the string matches the pattern.
    */
    /* The algorithm is a classical one.  We advance pointers in
       parallel through str and pat.  If we encounter a star in pat,
       we remember its position and continue advancing.  If at any
       stage we get a mismatch between str and pat, we look to see if
       there is a remembered star.  If not, we fail.  If so, we
       retreat pat to just past that star and str to the position
       after the last one we tried, and we let the match advance
       again.

       Even though there is only one remembered star position, the
       algorithm works when there are several stars in the pattern.
       When we encounter the second star, we forget the first one.
       This is OK, because if we get to the second star in A*B*C
       (where A etc are arbitrary strings), we have already seen AXB.
       We're therefore setting up a match of *C against the remainder
       of the string, which will match if that remainder looks like
       YC, so the whole string looks like AXBYC.
    */
    public static boolean wildmatch(char[] str, char[] pat) {
        int stri;     // index in str
        int pati;     // index in pat
        int starstri; // index for backtrack if "*" attempt fails
        int starpati; // index for backtrack if "*" attempt fails, +1
        final int strlen = str.length;
        final int patlen = pat.length;

        stri = pati = 0;
        starstri = starpati = -1;
        
        /* On each pass through this loop, we either advance pati,
           or we backtrack pati and advance starstri.  Since starstri
           is only ever assigned from pati, the loop must terminate.  */
        while (true) {
            if (pati < patlen) {
                final char patc = pat[pati];
                switch (patc) {
                case '?':
                    if (stri == strlen)
                        break;
                    stri++;
                    pati++;
                    continue;
                case '*':
                    pati++;
                    starpati = pati;
                    starstri = stri;
                    continue;
                default:
                    if (stri < strlen && str[stri] == patc) {
                        stri++;
                        pati++;
                        continue;
                    }
                    break;
                }
            } else if (stri == strlen)
                return true;

            // Mismatched, can we backtrack to a "*"?
            if (starpati < 0 || starstri == strlen)
                return false;

            // Retry the match one position later in str
            pati = starpati;
            starstri++;
            stri = starstri;
        }
    }

    /**
     * Retrieves the named object contained in repository
     * from the given objectname.
     */
    private NamedObject retrieveNamedObject(ObjectName name) {

        // No patterns inside reposit
        if (name.isPattern() == true) return null;

        // Extract the domain name.       
        String dom= name.getDomain().intern();

        // Default domain case
        if (dom.length() == 0) {
            dom = domain;
        }

        Object tmp_object = domainTb.get(dom);
        if (tmp_object == null) {
            return null; // No domain containing registered object names
        }

        // If name exists in repository, we will get it now 
        Hashtable moiTb= (Hashtable) tmp_object;
            
        Object o = moiTb.get(name.getCanonicalKeyPropertyListString());
        if (o != null ) {
            return (NamedObject) o;
        }
        else return null;
    }

    // Private methods <=============================================

    // Protected methods --------------------------------------------->
    // Protected methods <=============================================


    // Public methods --------------------------------------------->

    /**
     * The default constructor.
     */
    public RepositorySupport() {
        domainTb= new Hashtable(5);
        
        // Creates an new hastable for the default domain
        domainTb.put(domain.intern(), new Hashtable());
    }   
    
    /**
     * The purpose of this method is to provide a unified way to provide 
     * whatever configuration information is needed by the specific 
     * underlying implementation of the repository.
     *
     * @param configParameters An list containing the configuration 
     *        parameters needed by the specific Repository Service 
     *        implementation.
     */
    public void setConfigParameters(ArrayList configParameters) {
        return;
    } 
  
    /**
     * Indicates whether or not the Repository Service supports filtering. If
     * the Repository Service does not support filtering, the MBean Server
     * will perform filtering.
     *
     * @return  true if filtering is supported, false otherwise.
     */
    public boolean isFiltering() {
        // Let the MBeanServer perform the filtering !     
        return false;
    }
    
    /**
     * Stores an MBean associated with its object name in the repository.
     *
     *@param object MBean to be stored in the repository.
     *@param name MBean object name.
     *
     */
    public void addMBean(final Object object, ObjectName name) 
        throws InstanceAlreadyExistsException {
        
        if (logger.traceOn()) {
            logger.trace("addMBean", "name=" + name);
        }  

        // Extract the domain name. 
        String dom = name.getDomain().intern();     

        // locally record default domain or not
        boolean to_default_domain = (dom.length() == 0) || (dom == domain);
        if (to_default_domain) {
            dom = domain;
        }

        // Validate name for an object     
        if (name.isPattern() == true) {
            throw new RuntimeOperationsException(
             new IllegalArgumentException("Repository: cannot add mbean for pattern name " + name.toString()));
        }

        // Domain cannot be JMImplementation if entry does not exists
        if ( !to_default_domain &&
             dom.equals("JMImplementation") &&
             domainTb.containsKey("JMImplementation")) {

                throw new RuntimeOperationsException(
                  new IllegalArgumentException(
                      "Repository: domain name cannot be JMImplementation"));
            }            

        // If domain not already exists, add it to the hash table
        final Hashtable moiTb= (Hashtable) domainTb.get(dom);
        if (moiTb == null) {
            addNewDomMoi(object, dom, name);
            return;
        }
        else {
            // Add instance if not already present
            String cstr = name.getCanonicalKeyPropertyListString();
            Object elmt= moiTb.get(cstr);
            if (elmt != null) {
                throw new InstanceAlreadyExistsException(name.toString());
            } else {
                nbElements++;
                moiTb.put(cstr, new NamedObject(name, object));
            }
        }
    } 
    
    /**
     * Checks whether an MBean of the name specified is already stored in
     * the repository.
     *
     * @param name name of the MBean to find.
     *
     * @return  true if the MBean is stored in the repository, 
     *          false otherwise.
     *
     */
    public boolean contains(ObjectName name) {
        
        if (logger.traceOn()) {
            logger.trace("contains", "name=" + name);
        }  
        return (retrieveNamedObject(name) != null);
    }
    
    /**
     * Retrieves the MBean of the name specified from the repository. The
     * object name must match exactly.
     *
     * @param name name of the MBean to retrieve.
     *
     * @return  The retrieved MBean if it is contained in the repository, 
     *          null otherwise.
     *
     */
    public Object retrieve(ObjectName name) {
        
        if (logger.traceOn()) {
            logger.trace("retrieve", "name=" + name);
        }

        // Calls internal retrieve method to get the named object
        NamedObject no = retrieveNamedObject(name);
        if (no == null) return null;
        else return no.getObject();

    } 

    
    /**
     * Selects and retrieves the list of MBeans whose names match the specified
     * object name pattern and which match the specified query expression 
     * (optionally).
     *
     * @param pattern The name of the MBean(s) to retrieve - may be a specific 
     * object or a name pattern allowing multiple MBeans to be selected.
     * @param query query expression to apply when selecting objects - this 
     * parameter will be ignored when the Repository Service does not 
     * support filtering.
     *
     * @return  The list of MBeans selected. There may be zero, one or many 
     *          MBeans returned in the set.
     *
     */
    public Set query(ObjectName pattern, QueryExp query) {
        
        ObjectNamePattern on_pattern = null; // intermediate Object name pattern for performance
        final HashSet result = new HashSet();
        
        // The following filter cases are considered :
        // null, "", "*:*"" :  names in all domains
        // ":*" : names in defaultDomain
        // "domain:*" : names in the specified domain
        // "domain:[key=value], *"
        
        // Surely one of the most frequent case ... query on the whole world
        ObjectName name = null;
        if (pattern == null ||
            pattern.getCanonicalName().length() == 0 ||
            pattern.equals(_WholeWordQueryObjectName))
           name = _WholeWordQueryObjectName;
        else name = pattern;
        
        // If pattern is not a pattern, retrieve this mbean !
        if (!name.isPattern()) {       
            try {
                final NamedObject no = retrieveNamedObject(name);
                if (no != null) result.add(no);
            } catch (RuntimeOperationsException e ) { }
            return result;
        }
        
        // all  names in all domains
        if  (name == _WholeWordQueryObjectName) {   
            synchronized(domainTb) {
                for(final Enumeration e = domainTb.elements(); 
                    e.hasMoreElements();) {     
                    final Hashtable moiTb = (Hashtable) e.nextElement();
                    result.addAll(moiTb.values());
                }
            }
            return result;
        }

        String canonical_key_property_list_string = name.getCanonicalKeyPropertyListString();

        // all names in default domain
        //
        // fix 4618986 - take into account the case where the 
        //     property list is not empty.
        //
        if (name.getDomain().length() == 0) {
            final Hashtable moiTb = (Hashtable) domainTb.get(domain);
            if  (canonical_key_property_list_string.length() == 0) {
                result.addAll(moiTb.values());
            } else {
                if (on_pattern == null) 
                    on_pattern = new ObjectNamePattern(name);
                addAllMatching(moiTb,result,on_pattern);
            }
            return result;
        }
        
        // Pattern matching in the domain name (*, ?)
        synchronized (domainTb) {
            char[] dom2Match = name.getDomain().toCharArray();
            String nextDomain;
            char [] theDom;
            for (final Enumeration enumi = domainTb.keys(); enumi.hasMoreElements();) {
                nextDomain = (String) enumi.nextElement();
                theDom = nextDomain.toCharArray();

                if (wildmatch(theDom, dom2Match)) {
                    final Hashtable moiTb = 
                        (Hashtable) domainTb.get(nextDomain);

                    if (canonical_key_property_list_string.length() == 0)
                        result.addAll(moiTb.values());
                    else {
                        if (on_pattern == null) 
                            on_pattern = new ObjectNamePattern(name);
                        addAllMatching(moiTb,result,on_pattern);
                    }
                }
            }
        }
        return result;
    }
    
    /**
     * Removes an MBean from the repository.
     *
     * @param name name of the MBean to remove.
     *
     * @exception InstanceNotFoundException The MBean does not exist in 
     *            the repository.
     */
    public void remove(final ObjectName name) 
        throws InstanceNotFoundException {

        // Debugging stuff
        if (logger.traceOn()) {
            logger.trace("remove", "name=" + name);
        }  

        // Extract domain name.       
        String dom= name.getDomain().intern();

        // Default domain case
        if (dom.length() == 0) dom = domain;

        // Find the domain subtable
        Object tmp_object =  domainTb.get(dom);
        if (tmp_object == null) {
            throw new InstanceNotFoundException(name.toString());
        }

        // Remove the corresponding element
        Hashtable moiTb= (Hashtable) tmp_object;
        if (moiTb.remove(name.getCanonicalKeyPropertyListString()) == null) {
            throw new InstanceNotFoundException(name.toString());
        }

        // We removed it !
        nbElements--;
        
        // No more object for this domain, we remove this domain hashtable
        if (moiTb.isEmpty()) {
            domainTb.remove(dom);

            // set a new default domain table (always present)
            // need to reinstantiate a hashtable because of possible
            // big buckets array size inside table, never cleared, 
            // thus the new !
            if (dom == domain) 
                domainTb.put(domain, new Hashtable());
        }
    } 
    
    /**
     * Gets the number of MBeans stored in the repository.
     *
     * @return  Number of MBeans.
     */
    
    public Integer getCount() {
        return new Integer(nbElements);
    }
    
    /**
     * Gets the name of the domain currently used by default in the 
     * repository.
     *
     * @return  A string giving the name of the default domain name.
     */
    public  String getDefaultDomain() {
        return domain;
    }
    
    /**
     * Sets the name of the domain currently used by default in the
     * repository.
     *
     * @param domain the default domain name.
     */
    public void setDefaultDomain(String domain) { 
        synchronized (domainTb) {
            if ((domain != null) &&
                (!this.domain.equals(domain))) {
                this.domain = domain.intern();
                if (domainTb.get(this.domain) == null) 
                    domainTb.put(domain, new Hashtable());
            }
        }
    }
}



