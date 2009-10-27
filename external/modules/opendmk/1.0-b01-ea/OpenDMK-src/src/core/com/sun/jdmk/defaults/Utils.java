/*
 * @(#)file      Utils.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.5
 * @(#)lastedit  07/03/08
 * @(#)build     @BUILD_TAG_PLACEHOLDER@
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
 */

package com.sun.jdmk.defaults;

public class Utils {

    /**
     * Init the cause field of a Throwable object.  
     * The cause field is set only if <var>t</var> has an 
     * {@link Throwable#initCause(Throwable)} method (JDK Version >= 1.4) 
     * @param t Throwable on which the cause must be set.
     * @param cause The cause to set on <var>t</var>.
     * @return <var>t</var> with or without the cause field set.
     */
    public static Throwable initCause(Throwable t, Throwable cause) {

        /* Make a best effort to set the cause, but if we don't
           succeed, too bad, you don't get that useful debugging
           information.  We jump through hoops here so that we can
           work on platforms prior to J2SE 1.4 where the
           Throwable.initCause method was introduced.  If we change
           the public interface of JMRuntimeException in a future
           version we can add getCause() so we don't need to do this.  */
        try {
            java.lang.reflect.Method initCause =
                t.getClass().getMethod("initCause",
                                       new Class[] {Throwable.class});
            initCause.invoke(t, new Object[] {cause});
        } catch (Exception e) {
            // too bad, no debugging info
        }
        return t;
    }

    /**
     * Returns the cause field of a Throwable object.  
     * The cause field can be got only if <var>t</var> has an 
     * {@link Throwable#getCause()} method (JDK Version >= 1.4) 
     * @param t Throwable on which the cause must be set.
     * @return the cause if getCause() succeeded and the got value is not
     * null, otherwise return the <var>t</var>.
     */
    public static Throwable getCause(Throwable t) {
        Throwable ret = t;

        try {
            java.lang.reflect.Method getCause =
                t.getClass().getMethod("getCause", (Class[])null);
            ret = (Throwable)getCause.invoke(t, (Object[])null);

        } catch (Exception e) {
            // older than 1.4 ?
        }
        return (ret != null) ? ret: t;
    }

}
