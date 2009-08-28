/*
 * @(#)$Id: AnnotationParent.java 1523 2003-03-13 20:23:42Z kk122374 $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package com.sun.msv.schmit.reader;

import org.w3c.dom.Node;

/**
 * Receives parsed annotations.
 * 
 * @author
 *     Kohsuke Kawaguchi (kohsuke.kawaguchi@sun.com)
 */
public interface AnnotationParent {
    /**
     * Receives parsed annotations.
     * 
     * @param annotation
     *      Either Element or Attr.
     */
    void onEndAnnotation( Node annotation );
}
