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

package org.apache.myfaces.trinidadinternal.renderkit.core.xhtml;

import java.io.IOException;

import java.util.Map;

import javax.faces.application.FacesMessage;
import javax.faces.component.UIComponent;
import javax.faces.context.ExternalContext;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.convert.ConverterException;

import org.apache.myfaces.trinidad.bean.FacesBean;
import org.apache.myfaces.trinidad.component.core.input.CoreInputFile;
import org.apache.myfaces.trinidad.component.core.input.CoreInputText;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.context.RequestContext;
import org.apache.myfaces.trinidad.model.UploadedFile;
import org.apache.myfaces.trinidad.util.MessageFactory;
import org.apache.myfaces.trinidad.webapp.UploadedFileProcessor;
import org.apache.myfaces.trinidadinternal.config.upload.UploadedFiles;


/**
 */
public class SimpleInputFileRenderer extends SimpleInputTextRenderer
{
  public SimpleInputFileRenderer()
  {
    this(CoreInputFile.TYPE);
  }

  public SimpleInputFileRenderer(
    FacesBean.Type type)
  {
    super(type);
  }

  //
  // DECODE BEHAVIOR
  //
  @Override
  protected Object getSubmittedValue(
    FacesContext context,
    UIComponent  component,
    String       clientId)
  {
    // Since we override getSubmittedValue() entirely,
    // detect auto submit manually
    detectAutoSubmit(context, component, clientId);

    Object result = null;
    UploadedFile file = null;

    UploadedFiles files = UploadedFiles.getUploadedFiles(context);
    if (files != null)
    {
      file = files.getUploadedFile(clientId);
    }

    // If we couldn't find a file (e.g. does not exist OR the file is empty),
    // return "FALSE" to indicate that...
    // the file upload *was* available, but didn't upload a file
    // this time.
    if (file == null || file.getLength() == 0)
      result = Boolean.FALSE;
    else
      result = file;

    return result;
  }

  protected String getAutoComplete(
    UIComponent component,
    FacesBean   bean)
  {
    return CoreInputText.AUTO_COMPLETE_ON;
  }

  @Override
  public Object getConvertedValue(
    FacesContext context,
    UIComponent  component,
    Object       submittedValue)
  {
    // Convert FALSE back into null
    if (submittedValue == Boolean.FALSE)
      return null;

    UploadedFile file = (UploadedFile) submittedValue;
    if(file.getLength() == -1)
    {
      // There was a failure while one of the UploadedFileProcessor in the chain processed this file,
      // we expect the details to be in opaqueData
      String errorMessage = file.getOpaqueData().toString();
      FacesMessage fm = MessageFactory.getMessage(context, 
                                                  FacesMessage.SEVERITY_WARN, 
                                                  "org.apache.myfaces.trinidad.UPLOAD_FAILURE", 
                                                  new Object[]{errorMessage}, component); 
      throw new ConverterException(fm);
    }

    FacesBean bean = getFacesBean(component);
    Converter converter = getConverter(component, bean);
    // support converter for the <inputFile> component
    if(converter != null)
    {
      // create a unique key (component class name + filename) and use this
      // key to add the actual uploaded file to the requestMap
      String fileNameKey = component.getClass().getName() + "." + file.getFilename();
      context.getExternalContext().getRequestMap().put(fileNameKey, file);

      // applying the above convention. The String here is just the
      // unique key which the converter has to use to look for the
      // actual uploaded file.
      return converter.getAsObject(context, component, fileNameKey);
    }
    else
    {
      return file;
    }
  }

  //
  // ENCODE BEHAVIOR
  //

  @Override
  protected void encodeAllAsElement(
    FacesContext     context,
    RenderingContext rc,
    UIComponent      component,
    FacesBean        bean
    ) throws IOException
  {
     // call super...
    super.encodeAllAsElement(context, rc, component, bean);

    // now evaluate the EL
    // We need to evaluate it here and store it on the sessionMap because
    // during UploadedFileProcessor.processFile() there is no FacesContext
    RequestContext requestContext = RequestContext.getCurrentInstance();
    Object maxMemory = requestContext.getUploadedFileMaxMemory();
    Object maxDiskSpace = requestContext.getUploadedFileMaxDiskSpace();
    Object tempDir = requestContext.getUploadedFileTempDir();
    ExternalContext external = context.getExternalContext();
    Map<String, Object> sessionMap = external.getSessionMap();
    sessionMap.put(UploadedFileProcessor.MAX_MEMORY_PARAM_NAME, maxMemory);
    sessionMap.put(UploadedFileProcessor.MAX_DISK_SPACE_PARAM_NAME, maxDiskSpace);
    sessionMap.put(UploadedFileProcessor.TEMP_DIR_PARAM_NAME, tempDir);
  }

  /**
   * <inputFile> cannot show a value.
   */
  @Override
  protected String getConvertedString(
    FacesContext context,
    UIComponent  component,
    FacesBean    bean)
  {
    return null;
  }

  @Override
  protected String getDefaultInputType()
  {
    return "file";
  }

  //
  // Overrides disabling all the things you can't do on
  // an inputFile
  //

  @Override
  public boolean isTextArea(
    UIComponent component,
    FacesBean   bean)
  {
    return false;
  }

  @Override
  protected boolean isAutoSubmit(
    UIComponent component,
    FacesBean   bean)
  {
    return false;
  }

  @Override
  protected boolean getSecret(
    UIComponent component,
    FacesBean   bean)
  {
    return false;
  }

  @Override
  protected Number getMaximumLength(
    UIComponent component,
    FacesBean   bean)
  {
    return null;
  }

  @Override
  protected boolean getReadOnly(
    FacesContext context,
    UIComponent component,
    FacesBean   bean)
  {
    return false;
  }

  @Override
  protected String getRootStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|inputFile";
  }

  @Override
  protected String getContentStyleClass(
    UIComponent component,
    FacesBean   bean)
  {
    return "af|inputFile::content";
  }

  @Override
  protected Integer getDefaultColumns(
    RenderingContext rc,
    UIComponent component,
    FacesBean   bean)
  {
    return null;
  }
}
