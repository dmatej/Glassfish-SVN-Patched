/*
 * @(#)file      HtmlPage.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.29
 * @(#)lastedit      07/03/08
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
 *
 */


package com.sun.jdmk.comm;



// java import
//
import java.lang.StringBuffer;
import java.util.StringTokenizer;
import java.util.Set;
import java.util.ArrayList;

// jmx import
//
import javax.management.MBeanServer;
import javax.management.ObjectName;
import javax.management.MalformedObjectNameException;

// jmx RI import
//
import com.sun.jdmk.internal.ClassLogger;



abstract class HtmlPage {

    // --------------------------------------------------------
    // CONSTRUCTORS
    // --------------------------------------------------------
        
    /**
     * Constructs a new HtmlPage.
     */
    public HtmlPage(MBeanServer f, boolean r, boolean w) {
        htmlPage = new StringBuffer();
        mbs = f;
        readPerm = r;
        writePerm = w;
	logger = new ClassLogger(ClassLogger.LOGGER_ADAPTOR_HTML,
				 this.getClass());
    }


    // --------------------------------------------------------
    // PUBLIC METHODS
    // --------------------------------------------------------
    
    public String getPage() {
        return (htmlPage.toString());
    }

    public void setBgColor(String value) {
        if (value != null && !value.equals("")) {
            if (value.startsWith("#")) {
                bgColor = "BGCOLOR=\"" + value + "\"";
            } else {
                if (value.endsWith("gif")) {
                    bgColor = "BACKGROUND=\""+ value + "\"";
                } else {
                    bgColor = value;
                }
            }
        } else {
            bgColor = null;
        }
    }

    /**
     * HTML common
     */
    public abstract void buildPage(String Request);


    // --------------------------------------------------------
    // PROTECTED METHODS
    // --------------------------------------------------------

    protected void add2Page(String s) {
        htmlPage.append(s+HtmlDef.PF);
    }
    
    protected void buildError(String errorStr, String errorDef) {
        htmlPage = new StringBuffer();
        htmlPage.append(buildHead(errorDef));
        htmlPage.append(startBody( null ));
        htmlPage.append("<HR><P>"+HtmlDef.CRLF +
                        "<FONT SIZE=+3 COLOR=red><B>" + errorDef +
                        "</B></FONT><P><HR><P>" + errorStr + HtmlDef.CRLF);
        htmlPage.append("<P><TABLE WIDTH=100%><TR>");      
        htmlPage.append("<TD ALIGN=RIGHT>"+HtmlDef.LISTOFMBEAN+"</TD>");
        htmlPage.append("</TR></TABLE>");
        htmlPage.append(stopBody());
    }

    protected String buildHead(String title) {
        StringBuffer header = new StringBuffer(80);
        header.append( HtmlDef.docType  + HtmlDef.PF);        
        header.append( "<HTML>"  + HtmlDef.PF);
        header.append("<HEAD>"  + HtmlDef.PF);
        header.append("<TITLE>" + title + "</TITLE>" + HtmlDef.PF);
        header.append("</HEAD>" + HtmlDef.PF);
        return header.toString();
    }

    protected String buildHeadMeta(String title, String meta) {
        StringBuffer header = new StringBuffer(80);
        header.append("<HTML>"  + HtmlDef.PF);
        header.append("<HEAD>"  + HtmlDef.PF);
        header.append( "<TITLE>" + title + "</TITLE>" + HtmlDef.PF);
        header.append( meta);
        header.append("</HEAD>" + HtmlDef.PF);
        return header.toString();
    }

    protected String startBody(String otherOption) {
        StringBuffer body = new StringBuffer();
        body.append("<BODY");
        if (bgColor != null) {
            body.append( " " + bgColor + " ");
        }
        if (otherOption != null) {
            body.append(" " + otherOption + " ");
        }
        body.append(">" + HtmlDef.PF);
        return body.toString();
    }

    protected String stopBody() {
        return ("</BODY>" + HtmlDef.PF + "</HTML>" + HtmlDef.PF);
    }


    // [JF] Should use translateNameToHtmlFormat method instead of this one
    //
//     protected String removeQuote(String v) {
//         StringBuffer res = new StringBuffer();
//         int id = 0;
//         int beg = 0;
//         id = v.indexOf("\"");
//         while (id > 0) {
//             res.append( v.substring(beg,id) + "&#034");
//             beg = id + 1;
//             id = v.indexOf("\"", beg);
//         }
//         res.append(v.substring(beg, v.length()));
//         return res.toString();
//     }

    /**
     * URL encoding according to rfc2396.
     * @param string 
     * @return encoded string.
     */
    protected String encodeUrl(String string) {
        byte[] bytes = null;
        try {
            bytes=string.getBytes("ISO-8859-1");
        } catch(java.io.UnsupportedEncodingException e) {
            bytes=string.getBytes();
        }

        int len = bytes.length;
        byte[] encoded = new byte[bytes.length*3];
        int n = 0;
        boolean noEncode = true;

        for (int i=0; i<len; i++) {
            byte b = bytes[i];
            if (b==' ') {
                noEncode = false;
                encoded[n++] = (byte)'+';
            } else if (b>='a' && b<='z' || b>='A' && b<='Z' || b>='0' && b<='9') {
                encoded[n++] = b;
            } else {
                noEncode = false;
                encoded[n++] = (byte)'%';
                byte nibble = (byte) ((b&0xf0)>>4);
                if (nibble>=10) {
                    encoded[n++] = (byte)('A'+nibble-10);
                } else {
                    encoded[n++] = (byte)('0'+nibble);
                }
                nibble = (byte) (b&0xf);
                if (nibble>=10) {
                    encoded[n++] = (byte)('A'+nibble-10);
                } else {
                    encoded[n++] = (byte)('0'+nibble);
                }
            }
        }

        if (noEncode) {
            return string;
        }

        try {
            return new String(encoded, 0, n, "ISO-8859-1");
        } catch(java.io.UnsupportedEncodingException e) {
            return new String(encoded, 0, n);
        }
    }

    /**
     * URL decoding according to rfc2396.
     * @param  encoded URL string to decode
     * @return decoded string.
     */
    protected String decodeUrl(String encoded) {
        int len = encoded.length();
        char[] characters = encoded.toCharArray();
        byte[] bytes = new byte[len];
        int n = 0;
        boolean noDecode = true;

        for (int i=0; i<len; i++) {
            char c = characters[i];
            if (c<0||c>0x7f) {
                throw new IllegalArgumentException("Not encoded");
            }
            byte b = (byte)(0x7f & c);
            if (c=='+') {
                noDecode = false;
                b = (byte)' ';
            } else if ((c=='%') && ((i+2)<len)) {
                noDecode = false;
                b = (byte)(0xff&Integer.parseInt(encoded.substring(i+1,i+3),16));
                i += 2;
            }
            bytes[n++] = b;
        }

        if (noDecode) {
            return encoded;
        }

        try {
            return new String(bytes,0,n,"ISO-8859-1");
        } catch(java.io.UnsupportedEncodingException e) {
            return new String(bytes,0,n);
        }
    }

    protected String toUrlName(String name) {
        return "//" + encodeUrl(name);
    }

    protected String fromUrlName(String name) {
        int pos;
        if ((pos = name.indexOf("/")) < 0)
            return name;
        int next;
        if ((next = name.indexOf("/", pos + 1)) < 0)
            return name;

        name = name.substring(next + 1);

        return decodeUrl(name);
    }

    /**
     * Framework interaction
     */

    /**
     * Indicate whether or not a Java type is an array.
     * Instead of using the reflection API, use the way an array
     * is represented when calling the "toString()" method.
     */
    protected boolean isArrayType(String str) {
        if (str == null) 
            return false; 
        if (str.endsWith("[]"))
            // we are dealing with an array !
            // 
            return true;
        return false;
    }

    protected String usualType(String str) {
        int    lastStep;
        String pathName;
        String objName;
        String postFix = "";

        if (str == null) {
            return (null);
        }
        if ( str.startsWith("[Z"))
            str = "boolean[]";    
        if (  str.startsWith("[C"))
            str = "char[]";
        if ( str.startsWith("[D"))
            str = "double[]";
        if ( str.startsWith("[F"))
            str = "float[]";
        if ( str.startsWith("[I"))
            str = "int[]";
        if ( str.startsWith("[S"))
            str = "short[]";
        if ( str.startsWith("[J"))
            str = "long[]";
        if ( str.startsWith("[B"))
            str = "byte[]";
        if (str.startsWith("[L")) {
            // we are dealing with an array !
            // 
            postFix = "[]";
            str = str.substring(2, str.length() -1);
        } 

        lastStep = str.lastIndexOf('.');

        if (lastStep == -1) {
            return (str + postFix);
        }
        return (str + postFix);
    }

    /**
     * Translates a name string into a HTML safe format.
     *
     * @param name the name string to encode
     * @return the encoded string
     */
    protected String translateNameToHtmlFormat(String name) {

	if ( (name == null) || (name.length() == 0) ) {
	    return name;
	}

        char [] htmlChars = name.toCharArray();
        StringBuffer encodedHtml = new StringBuffer();
        for (int i=0; i<htmlChars.length; i++) {
            switch(htmlChars[i]) {
            case '<':
                encodedHtml.append("&lt;");
                break;
            case '>':
                encodedHtml.append("&gt;");
                break;
            case '&':
                encodedHtml.append("&amp;");
                break;
            case '\'':
                encodedHtml.append("&#39;");
                break;
            case '"':
                encodedHtml.append("&quot;");
                break;
            case '\\':
                encodedHtml.append("&#92;");
                break;
            case (char)133:
                encodedHtml.append("&#133;");
                break;
            default:
                encodedHtml.append(htmlChars[i]);
                break;
            }
        }
        return encodedHtml.toString();
    }

    protected String boolToHtml(String propertyName, String propertyType, String propertyView, boolean rw) {
        StringBuffer html = new StringBuffer("<INPUT TYPE=RADIO NAME=\"");
        //
        // We can CHECKED one
        //
        html.append(propertyName + "+" + propertyType + "\" ");

        if (propertyView != null && propertyView.equals("true") && rw)
            html.append( "CHECKED ");

        html.append("VALUE=\"true\">True ");
        html.append("<INPUT TYPE=RADIO NAME=\"");
        html.append(propertyName + "+" + propertyType + "\" ");

        if (propertyView != null && propertyView.equals("false") && rw) 
            html.append("CHECKED ");

        html.append("VALUE=\"false\">False ");    

        return html.toString();
    }

    protected ObjectName getObjectNameByObjNameStr(String objNameStr)
        throws MalformedObjectNameException {

        ObjectName manipulatedObjName = new ObjectName(objNameStr);
        Set list = mbs.queryNames(manipulatedObjName,null);
        if (list.size() <= 0) {
            buildError("Unable to get MBean ["+objNameStr+"]",
                       HtmlDef.HTTP_ERROR_INSTANCE_NOT_FOUND_ID + " " + HtmlDef.HTTP_ERROR_INSTANCE_NOT_FOUND);
            return null;
        }
        return (ObjectName)(list.toArray())[0];
    }

    // We should only propose the type that we know we can
    // convert from a String.
    // goodType is THE list (blank separated).
    //
    protected boolean checkType(String typeStr) {
	if(typeStr == null)
	    return false;
	
        String  eltStr;
        String  listStr = new String(HtmlDef.goodType);
        int     index   = 0;
        boolean done    = false;

        while (!done) {
            index = listStr.indexOf(' ');
            if (index < 0) {
                //
                // No more after this one
                //
                eltStr = listStr;
                done = true;
            } else {
                eltStr  = listStr.substring(0, index);
                listStr = listStr.substring(index + 1);
            }
            if (typeStr.equals(eltStr) || typeStr.endsWith(eltStr)) {
                return (true);
            }
        }

        return (false); 
    }

    protected void quicksort(String[] v, int lo0, int hi0) {
        int lo = lo0;
        int hi = hi0;
        String mid;

        if (hi0 > lo0) {
            // Arbitrarily establishing partition element as the midpoint of
            // the array.
            mid = v[(lo0 + hi0) / 2];

            // loop through the array until indices cross
            while(lo <= hi) {
                // find the first element that is greater than or equal to
                // the partition element starting from the left Index.
                //
                // Nasty to have to cast here. Would it be quicker
                // to copy the vectors into arrays and sort the arrays?
                while((lo < hi0) && lt(v[lo], mid)) {
                    ++lo;
                }

                // find an element that is smaller than or equal to
                // the partition element starting from the right Index.
                while((hi > lo0) && lt(mid, v[hi])) {
                    --hi;
                }

                // if the indexes have not crossed, swap
                if (lo <= hi) {
                    inverseS(v, lo, hi);
                    ++lo;
                    --hi;
                }
            }

            // If the right index has not reached the left side of array
            // must now sort the left partition.
            if (lo0 < hi) {
                quicksort(v, lo0, hi);
            }

            // If the left index has not reached the right side of array
            // must now sort the right partition.
            if (lo < hi0) {
                quicksort(v, lo, hi0);
            }
        }
    }

    protected boolean lt(String a, String b) {
        if (a==null) return false;
        if (b==null) return true;
        return a.compareTo(b) < 0;
    }

    protected void inverseS(String[] a ,int lo, int hi) {
        String T = a[lo];
        a[lo] = a[hi];
        a[hi] = T;
    }


    // --------------------------------------------------------
    // PROTECTED VARIABLES
    // --------------------------------------------------------

    protected StringBuffer htmlPage  = null;
    protected MBeanServer  mbs       = null;
    protected boolean      readPerm  = false;
    protected boolean      writePerm = false;

    // --------------------------------------------------------
    // PRIVATE VARIABLES
    // --------------------------------------------------------

    private String bgColor = null;
    final ClassLogger logger; 
}
