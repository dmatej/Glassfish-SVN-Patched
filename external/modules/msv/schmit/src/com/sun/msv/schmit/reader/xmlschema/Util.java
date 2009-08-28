/*
 * @(#)$Id: Util.java 1523 2003-03-13 20:23:42Z kk122374 $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package com.sun.msv.schmit.reader.xmlschema;

import com.sun.msv.util.StartTagInfo;

/**
 * 
 * 
 * @author
 *     Kohsuke Kawaguchi (kohsuke.kawaguchi@sun.com)
 */
final class Util {
    static boolean isAnnotationElement( StartTagInfo tag ) {
        return tag.namespaceURI.equals( SchmitXMLSchemaReader.XMLSchemaNamespace )
            &&  tag.localName.equals("annotation");
    }
}
