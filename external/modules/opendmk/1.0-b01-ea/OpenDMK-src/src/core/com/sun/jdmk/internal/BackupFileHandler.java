/*
 * @(#)file      BackupFileHandler.java
 * @(#)author    Sun Microsystems, Inc.
 * @(#)version   1.6
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
package com.sun.jdmk.internal;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;

import com.sun.jdmk.internal.ClassLogger;


/**
 * This class is used when a backup file is needed. It allows to update File in a safer way.
 */
public class BackupFileHandler {

    /**
     * Create a backup. If the extension is null, ".bak" is used.
     *
     */
    public static File createBackupFile(File file, String extension) {
	if(extension == null)
	    extension = ".bak";
	
	String name = file.getName();
	
	File parentFile = file.getParentFile() == null ? 
	    new File("." + File.separator) : file.getParentFile();
	
	File backupFile = new File(parentFile,
				   name + extension);
	if(logger.finestOn())
	    logger.finest("createBackupFile","backup file : " + backupFile +".");
	
	try {
	    backupFile.delete();
	}catch(Exception e) {
	    if(logger.finestOn())
		logger.finest("createBackupFile", e);
	    if(logger.finestOn())
		logger.finest("createBackupFile",
		      "Unable to delete the previous backup file");
	    return null;
	}
	
	if(logger.finestOn())
	    logger.finest("createBackupFile","Previous backup file : " + 
		  backupFile +" deleted.");
	
	try {
	    copyFile(file, backupFile);
	}catch(Exception e) {
	    if(logger.finestOn())
		logger.finest("createBackupFile", e);
	    if(logger.finestOn())
		logger.finest("createBackupFile",
		      "Unable to copy the file in a bak file.");
	    return null;
	}
	
	return  backupFile;
    }

    /**
     * Delete the backup.
     */
    public static void deleteBackupFile(File backupFile) {
	try {
	    backupFile.delete();
	}catch(Exception e) {
	    if(logger.finestOn())
		logger.finest("deleteBackupFile", e);
	    return;
	}
	
	if(logger.finestOn())
	    logger.finest("deleteBackupFile", 
		  "backup file " + backupFile +" deleted.");
    }

    
    private static void copyFile(File in, File out) throws Exception {
	FileInputStream fis  = new FileInputStream(in);
	FileOutputStream fos = new FileOutputStream(out);
	byte[] buf = new byte[1024];
	int i = 0;
	while((i=fis.read(buf))!=-1) {
	    fos.write(buf, 0, i);
	}
	fis.close();
	fos.close();
    }

    
    private static final ClassLogger logger = 
	new ClassLogger(ClassLogger.LOGGER_SNMP,
		        "BackupFileHandler");

}
