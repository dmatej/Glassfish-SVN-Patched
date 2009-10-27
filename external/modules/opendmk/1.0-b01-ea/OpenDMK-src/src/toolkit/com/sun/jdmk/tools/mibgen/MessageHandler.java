/*
 * @(#)file      MessageHandler.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   4.8
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



import java.util.ResourceBundle;
import java.util.MissingResourceException;
import java.text.MessageFormat;

/**
 * An handler of localized messages.
 *
 */

public class MessageHandler {

  /**
   * Resource to use
   */
  static private ResourceBundle rb;

  
static {
    try {
      rb = ResourceBundle.getBundle
        ("com.sun.jdmk.tools.mibgen.resources.Messages");
    } catch (MissingResourceException e) {
      System.err.println(e.getMessage());
      System.exit(1);
    }
  }; 

  /**
   * Version of the implementation
   */
   private static final String sccs_id = "@(#)MessageHandler.java 4.8 03/08/07 SMI";

  public static String getMessage(String key) {
    return (String)rb.getString(key);
  }

  public static String getMessage(String key, Object arg){
    String basemsgfmt = (String)rb.getString(key);
    MessageFormat msgfmt = new MessageFormat(basemsgfmt);
    Object msgobj[] = new Object[1];
    if (arg == null) {
      arg = "null"; // mimic java.io.PrintStream.print(String)
    }
    msgobj[0] = arg;
    return msgfmt.format(msgobj);
  }

  public static String getMessage(String key, Object arg1, Object arg2) {
    String basemsgfmt = (String)rb.getString(key);
    MessageFormat msgfmt = new MessageFormat(basemsgfmt);
    Object msgobj[] = new Object[2];
    if (arg1 == null) {
      arg1 = "null";
    }
    if (arg2 == null) {
      arg2 = "null";
    }
    msgobj[0] = arg1;
    msgobj[1] = arg2;
    return msgfmt.format(msgobj);
  }

  public static String getMessage(String key, Object arg1, Object arg2, Object arg3) {
    String basemsgfmt = (String)rb.getString(key);
    MessageFormat msgfmt = new MessageFormat(basemsgfmt);
    Object msgobj[] = new Object[3];
    if (arg1 == null) {
      arg1 = "null";
    }
    if (arg2 == null) {
      arg2 = "null";
    }
    if (arg3 == null) {
      arg3 = "null";
    }
    msgobj[0] = arg1;
    msgobj[1] = arg2;
    msgobj[2] = arg3;
    return msgfmt.format(msgobj);
  }

  public static String getMessage(String key, Object arg[]) {
    String basemsgfmt = (String)rb.getString(key);
    MessageFormat msgfmt = new MessageFormat(basemsgfmt);
    return msgfmt.format(arg);
  }
  
  /**
   * Returns the version of this class.
   */
  public static String getClassVersion () {
    return (sccs_id);
  }

}
