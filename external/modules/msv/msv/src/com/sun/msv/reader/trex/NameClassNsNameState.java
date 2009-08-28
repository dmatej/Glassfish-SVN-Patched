/*
 * @(#)$Id: NameClassNsNameState.java 1566 2003-06-09 20:37:49Z kk122374 $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package com.sun.msv.reader.trex;

import com.sun.msv.grammar.NameClass;
import com.sun.msv.grammar.NamespaceNameClass;

/**
 * parses &lt;nsName&gt; name class.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
public class NameClassNsNameState extends NameClassWithoutChildState {
    protected NameClass makeNameClass() {
        return new NamespaceNameClass( getPropagatedNamespace() );
    }
}
