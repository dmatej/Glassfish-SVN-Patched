/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2012 Oracle and/or its affiliates. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License.  You can
 * obtain a copy of the License at
 * https://glassfish.dev.java.net/public/CDDL+GPL_1_1.html
 * or packager/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at packager/legal/LICENSE.txt.
 *
 * GPL Classpath Exception:
 * Oracle designates this particular file as subject to the "Classpath"
 * exception as provided by Oracle in the GPL Version 2 section of the License
 * file that accompanied this code.
 *
 * Modifications:
 * If applicable, add the following below the License Header, with the fields
 * enclosed by brackets [] replaced by your own identifying information:
 * "Portions Copyright [year] [name of copyright owner]"
 *
 * Contributor(s):
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 */

package org.glassfish.fighterfish.test.app20;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * A simple implementation of the <code>StockQuoteService</code>
 * using cached values of the quotes for a pre-defined set of symbols
 * [the osgi service implementation]
 * 
 * @author Sivakumar Thyagarajan
 * @author Tang Yong
 */
public class SimpleStockQuoteServiceImpl implements StockQuoteService{
    private Map<String, Double> quotes = new HashMap<String, Double>();
    public SimpleStockQuoteServiceImpl(){
        System.out.println("SimpleStockQuoteServiceImpl::Initializing quotes");
        quotes.put("ORCL", new Double("28.02"));
        quotes.put("HPQ", new Double("42.59"));
        quotes.put("IBM", new Double("144.52"));
        quotes.put("MSFT", new Double("25.59"));
    }

    public Double getQuote(String symbol) {
        System.out.println("SimpleStockQuoteServiceImpl::returning quotes for" + symbol);
        return quotes.get(symbol);
    }
    
    
    public Set<String> getSymbols() {
        System.out.println("SimpleStockQuoteServiceImpl::getSymbols");
        return quotes.keySet();
    }

	public Set<String> getNullSymbols() {
		quotes = null;
		return quotes.keySet();
	}

}
