/*
 *  Licensed to the Apache Software Foundation (ASF) under one
 *  or more contributor license agreements.  See the NOTICE file
 *  distributed with this work for additional information
 *  regarding copyright ownership.  The ASF licenses this file
 *  to you under the Apache License, Version 2.0 (the
 *  "License"); you may not use this file except in compliance
 *  with the License.  You may obtain a copy of the License at
 * 
 *  http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing,
 *  software distributed under the License is distributed on an
 *  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 *  KIND, either express or implied.  See the License for the
 *  specific language governing permissions and limitations
 *  under the License.
 */
package org.apache.myfaces.trinidad.webapp;

import java.io.IOException;

import org.apache.myfaces.trinidad.model.UploadedFile;

/**
 * Interface responsible for processing file uploads.  An Apache Trinidad
 * application has a single <code>UploadedFileProcessor</code> instance. For
 * more simpler, multiple chained UploadedFileProcessor option please look at
 * {@link  org.apache.myfaces.trinidad.webapp.ChainedUploadedFileProcessor}.
 * UploadedFileProcessor is accessible from the {@link org.apache.myfaces.trinidad.context.RequestContext},
 * but will be invoked automatically by the framework as needed.  Developers
 * can replace the standard processor using the
 * <code>trinidad-config.xml</code> file.
 * <p>
 * To configure file uploads, the default instance supports three context
 * initialization parameters :
 * <ul>
 * <li>org.apache.myfaces.trinidad.UPLOAD_MAX_MEMORY: the maximum amount of memory
 *  that can be used in a single request to store
 *  uploaded files. (Default of 100K)
 * <li>org.apache.myfaces.trinidad.UPLOAD_MAX_DISK_SPACE: the maximum amount of
 *  disk space that can be used in a single request to store
 *  uploaded files.  (Default of 2000K)
 * <li>org.apache.myfaces.trinidad.UPLOAD_TEMP_DIR: the name of a directory
 *   to store temporary files.  (Defaults to the user's temporary directory)
 * </ul>
 * 
 * @see org.apache.myfaces.trinidad.model.UploadedFile
 */
public interface UploadedFileProcessor
{
  /**
   * Initialization parameter for the default
   * <code>UploadedFileProcessor</code> that configures the maximum
   * amount of memory that can be used in a single request to store
   * uploaded files.  Any requirements above this will be stored on disk.
   * The default is 100 kilobytes.
   */
  public static final String MAX_MEMORY_PARAM_NAME = "org.apache.myfaces.trinidad.UPLOAD_MAX_MEMORY";

  /**
   * Initialization parameter for the default
   * <code>UploadedFileProcessor</code> that configures the maximum
   * amount of disk space that can be used in a single request to store
   * uploaded files.  The default is 2000 kilobytes.  Any requests that
   * exceed this size will result in an EOFException being thrown
   * on that request.
   */
  public static final String MAX_DISK_SPACE_PARAM_NAME = "org.apache.myfaces.trinidad.UPLOAD_MAX_DISK_SPACE";

  /**
   * Initialization parameter for the default
   * <code>UploadedFileProcessor</code> that configures the 
   * to the directory where temporary files should be stored while
   * uploading.  This defaults to the the application server's temporary
   * directory, as provided by the "javax.servlet.context.tempdir"
   * property.  If that is not set, the System "java.io.tempdir" property
   * will be used as a backup.
   */
  public static final String TEMP_DIR_PARAM_NAME = "org.apache.myfaces.trinidad.UPLOAD_TEMP_DIR";

  /**
   * Initialize the UploadedFileProcessor with access to the current
   * web application context. 
   * 
   * @param context the current ServletContext or PortletContext
   */
  public void init(Object context);

  /**
   * Process a single uploaded file, moving it from temporary
   *   storage to per-request storage.  An implementation of this
   *   method must process an incoming <code>UploadedFile</code> object
   *   and return a new <code>UploadedFile</code> instance that will
   *   remain valid for the duration of this request.  The incoming
   *   <code>UploadedFile</code>
   *   object is a temporary file object that has a number of 
   *   restrictions:
   *  <ul>
   *    <li>{@link UploadedFile#getInputStream} may only be called once</li>
   *    <li>{@link UploadedFile#getLength} returns -1, since the length is not yet available</li>
   *    <li>{@link UploadedFile#getFilename} has not yet been internationalized;  users should not
   *      rely on its value, but simply use it unmodified in the
   *      outgoing <code>UploadedFile</code></li>
   *  </ul>
   *  <p>
   *  The <code>UploadedFile</code> object returned from this method
   *  must remain valid for the duration of this request.  The framework
   *  guarantees that {@link UploadedFile#dispose}</code> will be called before
   *  the request completes.
   *  </p>
   *  <p>
   *  If any implementation of this method throws an IOException, it is considered that there is a 
   *  error in processing the uploaded file, and the message contained in the IOException is shown 
   *  to the user as a value conversion warning.
   *  If the processing failure is less severe, and if the failure need to be meaningfully reported 
   *  to the end users, the length of the returned UploadedFile should be set to -1, and its 
   *  getOpaqueData() should provide the error details. The object returned by getOpaqueData() 
   *  should implement a toString() that returns a detailed error message. During the JSF life cycle 
   *  later, the input file component would show this message as value conversion warning to the 
   *  user.
   *  @see UploadedFile#getLength()
   *  @see UploadedFile#getOpaqueData()
   *  </p>
   * @param request the current servlet or portlet request
   * @param file a temporary file object
   * @return a new instance of UploadedFile.  It is legal to return null,
   *   in which case the file will not be available later in the request.
   */
  public UploadedFile processFile(
    Object request, UploadedFile file) throws IOException;
}
