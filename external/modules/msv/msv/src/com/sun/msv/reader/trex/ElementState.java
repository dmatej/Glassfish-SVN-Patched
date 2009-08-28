/*
 * @(#)$Id: ElementState.java 1566 2003-06-09 20:37:49Z kk122374 $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package com.sun.msv.reader.trex;

import com.sun.msv.grammar.Expression;
import com.sun.msv.grammar.trex.ElementPattern;

/**
 * parses &lt;element&gt; pattern.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
public class ElementState extends NameClassAndExpressionState {
    protected Expression annealExpression( Expression contentModel ) {
        ElementPattern e = new ElementPattern( nameClass, contentModel );
        reader.setDeclaredLocationOf(e);
        return e;
    }
}
