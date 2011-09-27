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

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.validator.Validator;

import org.apache.myfaces.trinidad.component.UIXEditableValue;
import org.apache.myfaces.trinidad.context.FormData;
import org.apache.myfaces.trinidad.context.RenderingContext;
import org.apache.myfaces.trinidad.convert.ClientConverter;
import org.apache.myfaces.trinidad.logging.TrinidadLogger;
import org.apache.myfaces.trinidad.util.FastMessageFormat;
import org.apache.myfaces.trinidad.util.MessageFactory;
import org.apache.myfaces.trinidad.validator.ClientValidator;
import org.apache.myfaces.trinidadinternal.share.data.ServletRequestParameters;


/**
 * Implementation of FormData from the CoreRenderKit (or,
 * more specifically, from the Trinidad FormRenderer)
 */
public class CoreFormData extends FormData
{

  public CoreFormData(String name)
  {
    _formName = name;
  }

  public int getInputTextCount()
  {
    return _inputTextCount;
  }

  public void incrementInputTextCount()
  {
    _inputTextCount++;
  }

  public boolean hasImmediateComponent()
  {
    return _hasImmediateComponent;
  }

  @Override
  public String getName()
  {
    return _formName;
  }

  @Override
  public void addLabel(
    String targetId,
    String label
    )
  {

    if (targetId == null || label == null)
      return;

    Map<String, String> labelMap = getLabelMap(true);

    labelMap.put(targetId, label);
  }

  public Map<String, String> getLabelMap(
    boolean  createIfNecessary
    )
  {
    if ((_labelMap == null) && createIfNecessary)
    {
      _labelMap = new HashMap<String, String>(31);
    }

    return _labelMap;
  }

  /**
   *
   * @todo get rid of servletRequestParameters reference
   */
  @Override
  public void addNeededValue(String name)
  {
    if (name == null)
      throw new IllegalArgumentException();

    if ( ServletRequestParameters.HAS_COMPOUND_NAME.equals(name) )
    {
      _useCompoundNames = true;
      return;
    }

    List<String> neededValues = getNeededValues(true);
    neededValues.add(name);
  }


  @Override
  public void addRenderedValue(String name)
  {
    Set<String> renderedSet = getRenderedValues(true);
    renderedSet.add(name);
  }

  /**
   * @todo get rid of needing this somehow?
   */
  public boolean useCompoundNames()
  {
    return _useCompoundNames;
  }

  /**
   * @param call a function call.
   * "eval(call)" will be called on the client when resetting.
   */
  public void addResetCall(
    String           clientId,
    String           call
    )
  {

    if (call != null)
    {
      Map<String, String> resetCalls = getResetCalls(true);

      // add the call
      resetCalls.put(clientId, call);
    }
  }


  public Map<String, String> getResetCalls(
    boolean          createIfNecessary
    )
  {

    if (_resetCallsList == null && createIfNecessary)
    {
      _resetCallsList = new HashMap<String, String>();
    }

    return _resetCallsList;
  }


  public List<String> getClientDependencies(
    boolean          createIfNecessary
    )
  {
    if ((_clientDependencies == null) && createIfNecessary)
    {
      _clientDependencies = new ArrayList<String>(10);
    }

    return _clientDependencies;
  }

  public List<String> getNeededValues(
    boolean createIfNecessary
    )
  {
    if ((_neededValues == null) && createIfNecessary)
    {
      _neededValues = new ArrayList<String>(10);
    }

    return _neededValues;
  }


  public Set<String> getRenderedValues(
    boolean createIfNecessary
    )
  {
    if ((_renderedSet == null) && createIfNecessary)
    {
      _renderedSet =  new HashSet<String>(23);
    }

    return _renderedSet;
  }


  public Iterator<String> getValidationIterator()
  {
    Map<String, Integer> validationMap = _getValidationMap(false);
    if ( validationMap == null)
      return null;

    return validationMap.keySet().iterator();
  }


  public Iterator<String> getErrorFormatIterator()
  {
    Map<String, Integer> errorFormatMap = _getErrorFormatMap(false);
    if ( errorFormatMap == null)
      return null;

    return errorFormatMap.keySet().iterator();
  }

  public Map<String, List<ConvertValidate>> getFormValidatorsInfo(
    boolean createIfNecessary
    )
  {
    // create the validators if they don't already exist
    if ((_formValidatorsInfo == null) && createIfNecessary)
    {
      _formValidatorsInfo = new HashMap<String, List<ConvertValidate>>();
    }

    return _formValidatorsInfo;
  }


  public List<String> getValidatedInputList(
    boolean createIfNecessary
    )
  {
    if ((_validatedInputList == null) && createIfNecessary)
    {
      _validatedInputList = new ArrayList<String>();
    }

    return _validatedInputList;
  }

  /**
   * TODO - adding required, converter, validators should be done separately
   * and this method should be killed.
   * TODO - when this API has been fixed up, move the new
   * versions to FormData
   */
  public void addOnSubmitConverterValidators(
    UIComponent         component,
    Converter           converter,
    Iterator<Validator> validators,
    String              clientId,
    boolean             immediate,
    boolean             required,
    String              requiredMessageKey
    ) throws IOException
  {
    if (clientId == null)
    {
      _LOG.warning("NULL_NODE_NAME_CANNOT_ADD_CONVERTER_AND_VALIDATOR");
      return;
    }

    if (immediate)
      _hasImmediateComponent = true;

    CoreFormData.ConvertValidate convertValidateInfo = null;

    // required identifies that required='true' has been set and that a validation
    // error should be displayed when no value is entered in the input field
    if (required)
    {
      convertValidateInfo = _getNewConvertValidate(clientId);

      convertValidateInfo.required = true;

      String reqMsgDetail = _getRequiredDetailMessage(component,
                                                      requiredMessageKey);

      // get the format index of this error format in the registered formats
      Integer formatIndex = _addErrorFormat(reqMsgDetail);
      convertValidateInfo.requiredFormatIndex = formatIndex;

      _addValidatedInput(clientId);
    }

    FacesContext context = FacesContext.getCurrentInstance();
    RenderingContext rc = RenderingContext.getCurrentInstance();

    if (converter != null && converter instanceof ClientConverter)
    {
      if (convertValidateInfo == null)
        convertValidateInfo = _getNewConvertValidate(clientId);

      _addOnSubmitConverter(context,
                            rc,
                            component,
                            ((ClientConverter) converter),
                            convertValidateInfo,
                            clientId);
    }


    if (validators == null)
    {
      //=-=AEW This seems to be OK right now
      //_LOG.warning("NULL_VALIDATORS_ITERATOR", component);
      ;
    }
    else
    {
      while (validators.hasNext())
      {
        Validator validator = validators.next();

        if (validator instanceof ClientValidator)
        {
          if (convertValidateInfo == null)
            convertValidateInfo = _getNewConvertValidate(clientId);

          _addOnSubmitValidator(context,
                                rc,
                                component,
                                ((ClientValidator)validator),
                                convertValidateInfo,
                                clientId);
        }
      }
    }
  }



  //*******************************************************
  // private
  //*******************************************************

  /**
   * Adds converter info.
   */
  private void _addFormConverterInfo(
    String                    converter,
    CoreFormData.ConvertValidate  convertValidate,
    String                    clientId
   )
  {
    if (converter != null && convertValidate != null)
    {
      if (convertValidate.converter == null)
        convertValidate.converter = new Object[2];
      else
        _LOG.warning("DUPLICATE_CONVERTER_ONE_PER_COMPONENT", clientId);


      // add the converter
      convertValidate.converter = (_addValidation(converter));

    }
  }

 /**
   * Adds a form-level validator.
   */
  private void _addFormValidatorInfo(
    String                    validator,
    CoreFormData.ConvertValidate  convertValidate
   )
  {
    if (validator != null && convertValidate != null)
    {
      if (convertValidate.validators == null)
        convertValidate.validators = new ArrayList<Integer>();

      // add the validator
      convertValidate.validators.add(_addValidation(validator));
    }
  }

  // =-= bts make package private
  /**
   * @todo escape script???
   * @todo get rid of the colorpicker hack!
   */
  private void _addOnSubmitConverter(
    FacesContext              context,
    RenderingContext          rc,
    UIComponent               component,
    ClientConverter           submitConverter,
    CoreFormData.ConvertValidate  convertValidate,
    String                    clientId
    ) throws IOException
  {

    if (component == null)
    {
      // HACK HACK - this is needed for colorPicker!
      component = new org.apache.myfaces.trinidad.component.UIXInput();
      component.setId(clientId);

    }

    // write out the lib(s) and script
    String libURI = submitConverter.getClientLibrarySource(context);
    String clientScript = submitConverter.getClientScript(context, component);
    Collection<String> libRefs = submitConverter.getClientImportNames();
    _addClientScripts(context, rc, libURI, clientScript, libRefs, "TrConverter()");

    String converter = submitConverter.getClientConversion(context,
                                                           component);

    if (converter != null)
    {

      _addFormConverterInfo( converter, convertValidate, clientId);
      _addValidatedInput(clientId);
    }
  }


  private void _addClientScripts(
    FacesContext        context,
    RenderingContext    rc,
    String              libURI,
    String              clientScript,
    Collection<String>  libRefs,
    String              defaultLibRef
  )throws IOException
  {
    if (libURI != null)
    {
      XhtmlUtils.writeLibImport(context, rc, libURI);
    }

    if(libRefs != null)
    {
      _writeDependencies(context, rc, libRefs);
    }
    else
    {
      _writeDependencies(context, rc, defaultLibRef);
    }

    if ( clientScript != null)
    {
      List<String> clientDependencies = getClientDependencies(true);
      clientDependencies.add(clientScript);
    }
  }



  /**
   * @todo Is there a way to remove the hack we have introduced, if the
   * component is null. This happens with composites.
   * @todo =-= bts make package private
   */
  private void _addOnSubmitValidator(
    FacesContext              context,
    RenderingContext          rc,
    UIComponent               component,
    ClientValidator           submitValidator,
    CoreFormData.ConvertValidate  convertValidate,
    String                    clientId
    ) throws IOException
  {

    // write out the lib(s) and script
    String libURI = submitValidator.getClientLibrarySource(context);
    String clientScript = submitValidator.getClientScript(context, component);
    Collection<String> libRefs = submitValidator.getClientImportNames();
    _addClientScripts(context, rc, libURI, clientScript, libRefs, "TrValidator()");

    String validator = submitValidator.getClientValidation(context,
                                                           component);

    if (validator != null)
    {
      _addFormValidatorInfo(validator, convertValidate);
      _addValidatedInput( clientId);
    }
  }


  /**
   * Returns the index of the validation in the list of validations, adding
   * the validation String if it doesn't already exist.
   */
  private Integer _addValidation(
    String  validation
    )
  {
    //TODO - not checking for null so map always getting created
    Map<String, Integer> validationMap = _getValidationMap(true);

    Integer validationIndex = validationMap.get(validation);

    if (validationIndex == null)
    {
      // the new element was added to the end of our vector
      validationIndex = validationMap.size();

      // add the new element to our map of strings to indices
      validationMap.put(validation, validationIndex);
    }

    // return the index of this format
    return validationIndex;
  }

  // =-= bts make package private
  private Integer _addErrorFormat(
    String errorFormat
    )
  {
    if (errorFormat != null)
    {
      Map<String, Integer> errorFormatMap = _getErrorFormatMap(true);

      Integer errorFormatIndex = errorFormatMap.get(errorFormat);

      if (errorFormatIndex == null)
      {
        // the new element was added to the end of our vector
        errorFormatIndex = errorFormatMap.size();
        // add the new element to our map of strings to indices
        errorFormatMap.put(errorFormat, errorFormatIndex);
      }

      // return the index of this format
      return errorFormatIndex;
    }

    return null;
  }

  /**
   * @todo should we get rid of this ConvertValidate object somehow? It's
   * used in the formRenderer.
   */
  private ConvertValidate _getNewConvertValidate(
    String           clientId
    )
  {
      // create
      ConvertValidate convertValidateInfo = new ConvertValidate();

      // add to list
      Map<String, List<ConvertValidate>> map = getFormValidatorsInfo(true);
      List<ConvertValidate> convertValidateList = map.get(clientId);
      if (convertValidateList == null)
      {
        convertValidateList = new ArrayList<ConvertValidate>();
        map.put(clientId, convertValidateList);
      }

      convertValidateList.add(convertValidateInfo);
      return convertValidateInfo;
  }



  /**
   * Add a UINode onto the list of UINodes that need to be validated.
   */
  // =-= bts make package private
  private void _addValidatedInput(
    String           clientId
    )
  {

    if (clientId != null)
    {
      getValidatedInputList(true).add(clientId);
    }
  }



  private Map<String, Integer> _getValidationMap(
    boolean          createIfNecessary
    )
  {
    if ((_validationMap == null) && createIfNecessary)
    {
      _validationMap = new LinkedHashMap<String, Integer>(31);
    }

    return _validationMap;
  }

  private Map<String, Integer> _getErrorFormatMap(
    boolean          createIfNecessary
    )
  {
    if ((_errorFormatMap == null) && createIfNecessary)
    {
      _errorFormatMap = new LinkedHashMap<String, Integer>(31);
    }

    return _errorFormatMap;
  }


  //*******************************************************
  // static private
  //*******************************************************
  static private String _getRequiredDetailMessage(
     UIComponent      component,
     String           requiredMessageKey)
  {
    if (component == null)
    {
      return null;
    }

    FacesContext fContext = FacesContext.getCurrentInstance();

    String detail = null;
    // it gets replaced in javaScript
    String label = "{0}";

    Object params[] = {label};

    if (component instanceof UIXEditableValue)
    {
      String requiredMessageDetail =
      ((UIXEditableValue) component).getRequiredMessageDetail();
      if (requiredMessageDetail != null)
      {
        FastMessageFormat format =
        new FastMessageFormat(requiredMessageDetail);
        detail = format.format(params);
      }

    }

    if (detail == null)
    {
      if (requiredMessageKey == null)
        requiredMessageKey = UIXEditableValue.REQUIRED_MESSAGE_ID;
      detail = MessageFactory.getMessage(fContext,
                                         requiredMessageKey,
                                         params).getDetail();
    }

    return detail;
  }

 /**
   * Opportunity for the ClientConverter or Validator to write any of its dependencies
   * to the output.  For HTML, this will typically be imports of
   * JavaScript libraries.
   */
  static private void _writeDependencies(
    FacesContext     context,
    RenderingContext rc,
    String           libReference
    ) throws IOException
  {
    String contentType = context.getResponseWriter().getContentType();
    if ("text/html".equals(contentType) ||
        "application/xhtml+xml".equals(contentType) ||
        (null == contentType))
    {
      XhtmlUtils.addLib(context,
                        rc,
                        libReference);
    }
  }

  /**
    * Opportunity for the ClientConverter or Validator to write any of its dependencies
    * to the output.  For HTML, this will typically be imports of
    * JavaScript libraries.
    */
   static private void _writeDependencies(
     FacesContext        context,
     RenderingContext    rc,
     Collection<String>  libRefs
     ) throws IOException
   {
     String contentType = context.getResponseWriter().getContentType();
     if ("text/html".equals(contentType) ||
         "application/xhtml+xml".equals(contentType) ||
         (null == contentType))
     {
       Object[] libRefArray = libRefs.toArray();
       for (int i = 0; i < libRefArray.length; i++)
       {
         XhtmlUtils.addLib(context, rc, libRefArray[i]);
       }
     }
   }

  private int _inputTextCount = 0;
  private boolean _hasImmediateComponent = false;

  // map of unique validation string to index in map
  // used so that each converter and validator constructor
  // is only written out once
  private Map<String, Integer> _validationMap = null;

  // map of unique error string to index in map
  // used so that each error string is only written out once
  private Map<String, Integer> _errorFormatMap = null;

  // List of ConvertValidate objects
  private Map<String, List<ConvertValidate>> _formValidatorsInfo;

  // javascript needed for client validations
  private List<String> _clientDependencies;


  // List of id's of input controls that need to get validated
  private List<String> _validatedInputList;


  private boolean _useCompoundNames = false;

  // List of empty hidden fields that will be filled during event generation
  private List<String> _neededValues;


  private Set<String> _renderedSet;

  // maps labels to id's
  // need a label map because the label is not always an attribute
  // on the component
  private Map<String, String> _labelMap = null;

  // Map of reset calls
  private Map<String, String> _resetCallsList = null;

  private String _formName = null;

 public static final class ConvertValidate
  {
    public boolean            required = false;
    public Integer            requiredFormatIndex;
    public ArrayList<Integer> validators;
    public Object             converter;
  }

  static private final TrinidadLogger _LOG = TrinidadLogger.createTrinidadLogger(CoreFormData.class);
}
