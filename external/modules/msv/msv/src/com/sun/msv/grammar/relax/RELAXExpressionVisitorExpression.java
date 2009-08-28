/*
 * @(#)$Id: RELAXExpressionVisitorExpression.java 1566 2003-06-09 20:37:49Z kk122374 $
 *
 * Copyright 2001 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * This software is the proprietary information of Sun Microsystems, Inc.  
 * Use is subject to license terms.
 * 
 */
package com.sun.msv.grammar.relax;

import com.sun.msv.grammar.Expression;
import com.sun.msv.grammar.ExpressionVisitorExpression;

/**
 * RELAX version of {@link ExpressionVisitorExpression}
 * 
 * @author <a href="mailto:kohsuke.kawaguchi@eng.sun.com">Kohsuke KAWAGUCHI</a>
 */
public interface RELAXExpressionVisitorExpression extends ExpressionVisitorExpression {
    
    // RELAX visitor can ignore onRef callback.
    Expression onAttPool( AttPoolClause exp );
    Expression onTag( TagClause exp );
    Expression onElementRules( ElementRules exp );
    Expression onHedgeRules( HedgeRules exp );
}
