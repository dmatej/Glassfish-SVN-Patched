/*
 * @(#)file      ObjectTypeDefinition.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.10
 * @(#)date      07/04/04
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
 *
 */


package com.sun.jdmk.tools.mibgen;



import	java.io.*;
import	java.util.*;

/**
 * The class defines an abstract representation of an object type.
 * The allows to use the same code for handling objects defined through
 * the SNMP V1 or SNMPv2 standard.
 *
 */
public abstract class ObjectTypeDefinition extends SimpleNode implements Serializable {
  
  
    public ObjectTypeDefinition(int id) {
        super(id);
    }

    public ObjectTypeDefinition(Parser p, int id) {
        super(p, id);
    }
  
    public ASTOidValue getOidNode() {
        return oidNode;
    }
  
    public String getDescription() {
        return description;
    }
  
    public String getReference() {
        return reference;
    }
  
    public int getAccess() {
        return access;
    }
  
    public int getStatus() {
        return status;
    }
    
    public Node getIndex() {
        return indexNode;
    }
  
    public ASTValue getDefValue() {
        return defValue;
    }
  
    /**
     * Need to implement this method for the DEFVAL in case of an OBJECT IDENTIFIER value.
     */
    public void  resolve(IdentifierHandler symbolsHandler, String key) {
        
        // DEFVAL for OBJECT IDENTIFIER values:
        // Resolve the identifier default value.
        if (defValue != null) {
            defValue.resolve(symbolsHandler, key);
        }
    }
    
    ASTOidValue oidNode= null;
    ASTValue defValue= null;
    Node indexNode= null;
    String reference= null;
    String description= null;
    int status;
    int access;

}
