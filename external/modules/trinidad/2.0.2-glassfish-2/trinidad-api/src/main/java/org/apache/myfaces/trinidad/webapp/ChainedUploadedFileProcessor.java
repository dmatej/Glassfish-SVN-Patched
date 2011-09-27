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
 * Interface responsible for processing file uploads by multiple processors one
 * after another in a chained fashion. An Apache Trinidad application could have
 * multiple <code>ChainedUploadedFileProcessor</code> instances. A composite UploadedFileProcessor
 * is accessible from the {@link org.apache.myfaces.trinidad.context.RequestContext},
 * but will be invoked automatically by the framework as needed. Developers
 * can implement this interface and chain many of them up together using space
 * separated class names in <code>trinidad-config.xml</code> file under
 * <code>uploaded-file-processor</code> element. The order in which the processors
 * will be instantated and called will be the same as how it appears inside the element.
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
public interface ChainedUploadedFileProcessor extends UploadedFileProcessor
{
  /**
   * Initialize the ChainedUploadedFileProcessor with access to the current
   * web application context. The order of call is same as it
   * appears in trinidad-config.xml
   *
   * @param context the current ServletContext or PortletContext
   */
  public void init(Object context);

  /**
   * Process a single uploaded file.  An implementation of this
   *   method must process an incoming <code>UploadedFile</code> object
   *   and return a new <code>UploadedFile</code> instance that will
   *   remain valid for the duration of this request.  The properties of the incoming
   *   <code>UploadedFile</code> object depends on the implementation detail of
   *   the previous ChainedUploadedFileProcessor in the chain. In general all the
   *   implementations must strive to return a <code>UploadedFile</code> that
   *   should at the least comply to following:
   *  <ul>
   *    <li>{@link UploadedFile#getInputStream} may only be called once</li>
   *    <li>{@link UploadedFile#getLength} returns length as it should be available</li>
   *    <li>{@link UploadedFile#getFilename} may have been internationalized;  users should not
   *      rely on its value, but simply use it unmodified in the
   *      outgoing <code>UploadedFile</code></li>
   *  </ul>
   *  <p>
   *  First ChainedUploadedFileProcessor in the list gets an <code>UploadedFile</code> implementation
   *  from the framework with the above properties and the following few more:
   *  <ul>
   *    <li>{@link UploadedFile#getInputStream} may be called multiple times. Each call gives a new InputStream</li>
   *    <li>{@link UploadedFile#getLength} returns length</li>
   *  </ul>
   *  Due to the above properties, if there was no change made to the underlying stream
   *  significantly by the current ChainedUploadedFileProcessor, this <code>UploadedFile</code> object could
   *  be returned intact for subsequent processing by the framework. This avoids creation of
   *  new <code>UploadedFile</code> for simple ChainedUploadedFileProcessor implementations.
   *  </p>
   *  <p>
   *  The framework guarantees that {@link UploadedFile#dispose}</code> will be called before
   *  the request completes for each UploadedFile returned by every ChainedUploadedFileProcessor.
   *  The order in which dispose() will be called is the revers of the order
   *  they are declared in trinidad-config.xml. If same UploadedFile
   *  was returned by more than one processors, dispose() will be called only once
   *  on it. Any exception that happenes during dispose() call will be logged
   *  as warning and the processing continues with the rest of the UploadedFile(s).
   *  </p>
   *  <p>
   *  If one of chained file processor throws an IOException in this method, it is considered that
   *  there is a error in processing the uploaded file, the chain is broken hence, the file upload
   *  process stops, and the message contained in the IOException is shown to the user as a value 
   *  conversion warning.
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
