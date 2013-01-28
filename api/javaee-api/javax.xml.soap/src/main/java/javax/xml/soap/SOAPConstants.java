/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright (c) 2004-2012 Oracle and/or its affiliates. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License.  You can
 * obtain a copy of the License at
 * http://glassfish.java.net/public/CDDL+GPL_1_1.html
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

package javax.xml.soap;

import javax.xml.namespace.QName;

/**
 * The definition of constants pertaining to the SOAP protocol. 
 */
public interface SOAPConstants {
    /**
     * Used to create <code>MessageFactory</code> instances that create
     * <code>SOAPMessages</code> whose concrete type is based on the
     * <code>Content-Type</code> MIME header passed to the
     * <code>createMessage</code> method. If no <code>Content-Type</code>
     * header is passed then the <code>createMessage</code> may throw an
     * <code>IllegalArgumentException</code> or, in the case of the no
     * argument version of <code>createMessage</code>, an
     * <code>UnsupportedOperationException</code>.
     *
     * @since  SAAJ 1.3
     */
    public static final String DYNAMIC_SOAP_PROTOCOL = "Dynamic Protocol";

    /**
     * Used to create <code>MessageFactory</code> instances that create
     * <code>SOAPMessages</code> whose behavior supports the SOAP 1.1  specification.
     *
     * @since  SAAJ 1.3
     */
    public static final String SOAP_1_1_PROTOCOL = "SOAP 1.1 Protocol";

    /**
     * Used to create <code>MessageFactory</code> instances that create
     * <code>SOAPMessages</code> whose behavior supports the SOAP 1.2
     * specification
     *
     * @since  SAAJ 1.3
     */
    public static final String SOAP_1_2_PROTOCOL = "SOAP 1.2 Protocol";

    /**
     * The default protocol: SOAP 1.1 for backwards compatibility.
     *
     * @since SAAJ 1.3
     */
    public static final String DEFAULT_SOAP_PROTOCOL = SOAP_1_1_PROTOCOL;

    /**
     * The namespace identifier for the SOAP 1.1 envelope.
     * @since SAAJ 1.3
     */
    public static final String
		URI_NS_SOAP_1_1_ENVELOPE = "http://schemas.xmlsoap.org/soap/envelope/";
    /**
     * The namespace identifier for the SOAP 1.2 envelope.
     * @since SAAJ 1.3
     */
    public static final String
		URI_NS_SOAP_1_2_ENVELOPE = "http://www.w3.org/2003/05/soap-envelope";

    /**
     * The namespace identifier for the SOAP 1.1 envelope, All SOAPElements in this 
     * namespace are defined by the SOAP 1.1 specification.
     */
    public static final String
        URI_NS_SOAP_ENVELOPE = URI_NS_SOAP_1_1_ENVELOPE;

    /**
     * The namespace identifier for the SOAP 1.1 encoding.
     * An attribute named <code>encodingStyle</code> in the
     * <code>URI_NS_SOAP_ENVELOPE</code> namespace and set to the value
     * <code>URI_NS_SOAP_ENCODING</code> can be added to an element to indicate
     * that it is encoded using the rules in section 5 of the SOAP 1.1
     * specification.
     */
    public static final String
        URI_NS_SOAP_ENCODING = "http://schemas.xmlsoap.org/soap/encoding/";

    /**
     * The namespace identifier for the SOAP 1.2 encoding.
     * @since SAAJ 1.3
     */
    public static final String
        URI_NS_SOAP_1_2_ENCODING = "http://www.w3.org/2003/05/soap-encoding";

    /**
     * The media type  of the <code>Content-Type</code> MIME header in SOAP 1.1.
     * @since SAAJ 1.3
     */
    public static final String
        SOAP_1_1_CONTENT_TYPE = "text/xml";

    /**
     * The media type  of the <code>Content-Type</code> MIME header in SOAP 1.2.
     * @since SAAJ 1.3
     */
    public static final String
        SOAP_1_2_CONTENT_TYPE = "application/soap+xml";

    /**
     * The URI identifying the next application processing a SOAP request as the intended
     * actor for a SOAP 1.1 header entry (see section 4.2.2 of the SOAP 1.1 specification).
     * <p>
     * This value can be passed to
     * {@link SOAPHeader#examineMustUnderstandHeaderElements(String)},
     * {@link SOAPHeader#examineHeaderElements(String)} and
     * {@link SOAPHeader#extractHeaderElements(String)}
     */
    public static final String
        URI_SOAP_ACTOR_NEXT = "http://schemas.xmlsoap.org/soap/actor/next";

    /**
     * The URI identifying the next application processing a SOAP request as the intended
     * role for a SOAP 1.2 header entry (see section 2.2 of part 1 of the SOAP 1.2
     * specification).
     * @since SAAJ 1.3
     */
    public static final String
        URI_SOAP_1_2_ROLE_NEXT = URI_NS_SOAP_1_2_ENVELOPE + "/role/next";

    /**
     * The URI specifying the role None in SOAP 1.2.
     * @since SAAJ 1.3
     */
    public static final String
        URI_SOAP_1_2_ROLE_NONE = URI_NS_SOAP_1_2_ENVELOPE + "/role/none";

    /**
     * The URI identifying the ultimate receiver of the SOAP 1.2 message.
     * @since SAAJ 1.3
     */
    public static final String
        URI_SOAP_1_2_ROLE_ULTIMATE_RECEIVER =
            URI_NS_SOAP_1_2_ENVELOPE + "/role/ultimateReceiver";

    /**
     * The default namespace prefix for http://www.w3.org/2003/05/soap-envelope
     * @since SAAJ 1.3
     */
    public static final String SOAP_ENV_PREFIX = "env";

    /**
     * SOAP 1.2 VersionMismatch Fault
     * @since SAAJ 1.3
     */
    public static final QName SOAP_VERSIONMISMATCH_FAULT =
         new QName(URI_NS_SOAP_1_2_ENVELOPE, "VersionMismatch", SOAP_ENV_PREFIX);
                                                                                                                  
    /**
     * SOAP 1.2 MustUnderstand Fault
     * @since SAAJ 1.3
     */
    public static final QName SOAP_MUSTUNDERSTAND_FAULT =
         new QName(URI_NS_SOAP_1_2_ENVELOPE, "MustUnderstand", SOAP_ENV_PREFIX);
                                                                                                                  
    /**
     * SOAP 1.2 DataEncodingUnknown Fault
     * @since SAAJ 1.3
     */
    public static final QName SOAP_DATAENCODINGUNKNOWN_FAULT =
         new QName(URI_NS_SOAP_1_2_ENVELOPE, "DataEncodingUnknown", SOAP_ENV_PREFIX);
                                                                                                                  
    /**
     * SOAP 1.2 Sender Fault
     * @since SAAJ 1.3
     */
    public static final QName SOAP_SENDER_FAULT =
         new QName(URI_NS_SOAP_1_2_ENVELOPE, "Sender", SOAP_ENV_PREFIX);
                                                                                                                  
    /**
     * SOAP 1.2 Receiver Fault
     * @since SAAJ 1.3
     */
    public static final QName SOAP_RECEIVER_FAULT =
         new QName(URI_NS_SOAP_1_2_ENVELOPE, "Receiver", SOAP_ENV_PREFIX);
                                                                                                                  
}
