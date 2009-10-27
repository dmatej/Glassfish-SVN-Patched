/*
 * @(#)file      Trace.java
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

import	java.io.*;
import	java.util.*;

/**
 * Used to output mibgen message.
 */
public class Trace implements Serializable {

  private static String error=  MessageHandler.getMessage("compile.error");
  private static String warning=  MessageHandler.getMessage("compile.warning");
  
  private static int nb_error=0;
  private static int nb_warning=0;

  public static void output(OutputStream out, String msg) {
    try {
      int max= msg.length();
      for (int i = 0; i < max ; i++) {
        out.write(msg.charAt(i));
      } 
      
      out.write('\n');
    } catch (IOException e) {
    }
  } 

  public static void setOutput(OutputStream out) {
    stream= out;
  }

  public static void info(String msg) {
    output(stream, msg);
  }

  public static void warning(String msg) {
    nb_warning++;
    output(stream, warning + msg);
  }

  public static void conclude() {
    if (nb_warning != 0) {
      if (nb_warning == 1)
        output(stream, MessageHandler.getMessage("compile.nb.warning", String.valueOf(nb_warning)));
      else
        output(stream, MessageHandler.getMessage("compile.nb.warnings", String.valueOf(nb_warning)));
    }

    if (nb_error != 0) {
      if (nb_error == 1)
        output(stream, MessageHandler.getMessage("compile.nb.error", String.valueOf(nb_error)));
      else
        output(stream, MessageHandler.getMessage("compile.nb.errors", String.valueOf(nb_error)));
    }
  }

  public static void error(String msg) {
    nb_error++;
    output(stream, error + msg);
  }
 
  private static OutputStream stream= System.out;
  /**
   * Version of the implementation
   */
   private static final String sccs_id = "@(#)Trace.java 4.8 03/08/07 SMI";

}
