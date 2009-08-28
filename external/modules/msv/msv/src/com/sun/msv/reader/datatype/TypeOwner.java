/*
 * @(#)$Id: TypeOwner.java 1566 2003-06-09 20:37:49Z kk122374 $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package com.sun.msv.reader.datatype;

import com.sun.msv.grammar.Expression;

/**
 * State can implement this method to be notified by DataType vocabulary
 * about the result of parsing.
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
public interface TypeOwner {
    void onEndChildType( Expression datatype, String typeName );
}
