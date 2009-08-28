/*
 * @(#)$Id: ChildlessState.java 1566 2003-06-09 20:37:49Z kk122374 $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package com.sun.msv.reader;

import com.sun.msv.util.StartTagInfo;

/**
 * state that has no children.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
public class ChildlessState extends SimpleState {
    protected final State createChildState( StartTagInfo tag ) {
        return null;
    }
}
    
    
