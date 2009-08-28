/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 * 
 * Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.
 * 
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 * 
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 * 
 * Contributor(s):
 * 
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

package org.jvnet.staxex;

import javax.xml.namespace.NamespaceContext;
import java.util.Iterator;

/**
 * Extended {@link NamespaceContext}.
 *
 * @author Kohsuke Kawaguchi
 * @author Paul Sandoz
 */
public interface NamespaceContextEx extends NamespaceContext, Iterable<NamespaceContextEx.Binding> {

    /**
     * Iterates all the in-scope namespace bindings.
     *
     * <p>
     * This method enumerates all the active in-scope namespace bindings.
     * This does not include implicit bindings, such as
     * <tt>"xml"->"http://www.w3.org/XML/1998/namespace"</tt>
     * or <tt>""->""</tt> (the implicit default namespace URI.)
     *
     * <p>
     * The returned iterator may not include the same prefix more than once.
     * For example, the returned iterator may only contain <tt>f=ns2</tt>
     * if the document is as follows and this method is used at the bar element.
     *
     * <pre><xmp>
     * <foo xmlns:f='ns1'>
     *   <bar xmlns:f='ns2'>
     *     ...
     * </xmp></pre>
     *
     * <p>
     * The iteration may be done in no particular order.
     *
     * @return
     *      may return an empty iterator, but never null.
     */
    Iterator<Binding> iterator();

    /**
     * Prefix to namespace URI binding.
     */
    interface Binding {
        /**
         * Gets the prefix.
         *
         * <p>
         * The default namespace URI is represented by using an
         * empty string "", not null.
         *
         * @return
         *      never null. String like "foo", "ns12", or "".
         */
        String getPrefix();

        /**
         * Gets the namespace URI.
         *
         * <p>
         * The empty namespace URI is represented by using
         * an empty string "", not null.
         *
         * @return
         *      never null. String like "http://www.w3.org/XML/1998/namespace",
         *      "urn:oasis:names:specification:docbook:dtd:xml:4.1.2", or "".
         */
        String getNamespaceURI();
    }
}
