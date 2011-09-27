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


/**
* TrXMLRequest class is a low-level XML HTTP Request
* wrapper
**/
/**
* Default constructor. Creates an asynchronous XML HTTP request
**/
function TrXMLRequest()
{
  this.isSynchronous = false;
  this.callback = null;
  this._state = TrXMLRequest.UNINITIALIZED;
  this.headers = new Object();
  this.xmlhttp = TrXMLRequest._createXmlHttpRequest();
}

/**
* Request state constants. See getCompletionState()
**/
TrXMLRequest.UNINITIALIZED = 0;
TrXMLRequest.LOADING = 1;
TrXMLRequest.LOADED = 2;
TrXMLRequest.INTERACTIVE = 3;
TrXMLRequest.COMPLETED = 4;

/**
* Specifies whether the request should be synchronous
* Parameters: isSynch - true if request should be synchronous,
* false otherwise
**/
TrXMLRequest.prototype.setSynchronous =
function (isSynch)
{
  this.isSynchronous = isSynch;
};

/**
* Registers request callback for asynchronous requests
* The callback will be called each time the state of the
* request changes (see getCompletionState())
* The callback should have the following siganture:
* <void> function (<TrXMLRequest>request)
**/
TrXMLRequest.prototype.setCallback =
function (_callback)
{
  this.callback = _callback;
};

/**
* Returns request's completion state (see Request state
* constants)
**/
TrXMLRequest.prototype.getCompletionState =
function()
{
  return this._state;
};

/**
 * Returns the HTTP response status.  For example, 200 is OK.
 */
TrXMLRequest.prototype.getStatus =
function()
{
  return this.xmlhttp.status;
}

/**
* Returns the response as an XML document
* Note: this method will block if the the request is asynchronous and
* has not yet been completed
**/
TrXMLRequest.prototype.getResponseXML =
function()
{
  return this.xmlhttp.responseXML;
}

/**
* Returns the response as text
* Note: this method will block if the the request is asynchronous and
* has not yet been completed
**/
TrXMLRequest.prototype.getResponseText =
function()
{
  return this.xmlhttp.responseText;
}

/**
* Sends an XML HTTP request
* Parameters:
* url - destination URL
* content - XML document or string that should be included in the request's body
**/
TrXMLRequest.prototype.send =
function(url, content)
{
  var xmlhttp = this.xmlhttp;
  if (!this.isSynchronous)
  {
    var cb = new Function("arguments.callee.obj.__onReadyStateChange();");
    cb.obj = this;
    xmlhttp.onreadystatechange  = cb;
  }

  var method = content ? "POST" : "GET";
  xmlhttp.open(method, url, !this.isSynchronous);
  for (var name in this.headers)
    xmlhttp.setRequestHeader(name, this.headers[name]);

  // Set some header to indicate the request initiated from
  // the Trinidad XHR request
  // =-ags This needs to be revisited
  xmlhttp.setRequestHeader("Tr-XHR-Message", "true");

  xmlhttp.send(content);
  if (this.isSynchronous)
  {
    this._state = xmlhttp.readyState;
  }
}


TrXMLRequest.prototype.getResponseHeader =
function(name)
{

  return this.xmlhttp.getResponseHeader(name);
}

TrXMLRequest.prototype.getAllResponseHeaders =
function()
{
  return this.xmlhttp.getAllResponseHeaders();
}

TrXMLRequest.prototype.setRequestHeader =
function(name, value)
{
  this.headers[name] = value;
}

TrXMLRequest._createXmlHttpRequest = function()
{
  var xmlhttp;

  if (window.XMLHttpRequest)
  {
    xmlhttp = new XMLHttpRequest();
  }
  else if (window.ActiveXObject)
  {
    try
    {
      xmlhttp = new ActiveXObject("Msxml2.XMLHTTP");
    }
    catch (e)
    {
      try
      {
        xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
      }
      catch (e)
      {
      }
    }
  }
  return xmlhttp;
}


TrXMLRequest.prototype.__onReadyStateChange =
function()
{
  this._state = this.xmlhttp.readyState;
  if (this.callback)
    this.callback(this);
}

TrXMLRequest.prototype.cleanup =function()
{
  this.callback = null
  delete this.xmlhttp;
}

/**
 * Wrapper for the JSF AJAX callback data that provides the same
 * interface as TrXMLRequest to maintain a compatible API to ease
 * the support of both the legacy code as well as code to integrate
 * with JSF 2 AJAX
 */
function TrXMLJsfAjaxRequest(
  event,
  params,
  formId)
{
  this.isSynchronous = false;
  this.callback = null;
  this._event = event;
  this._params = params || new Object();
  this._status = 0;
  this._state = TrXMLRequest.UNINITIALIZED;
  this._formId = formId;
}
TrXMLJsfAjaxRequest.prototype.setCallback = function(value)
{
  this.callback = value;
}
TrXMLJsfAjaxRequest.prototype.getCompletionState = function()
{
  return this._state;
}
TrXMLJsfAjaxRequest.prototype.getStatus = function()
{
  return this._status;
}
TrXMLJsfAjaxRequest.prototype.getResponseXML = function()
{
  return this._responseXML;
}
TrXMLJsfAjaxRequest.prototype.getResponseText = function()
{
  return this._responseText;
}
TrXMLJsfAjaxRequest.prototype.cleanup = function()
{
  if (this._formElements != null)
  {
    for (var name in this._formElements)
    {
      var origValue = this._origFormValues[name];
      this._formElements[name].value = origValue;
    }
  }
  delete this._origFormValues;
  delete this._formElements;

  this.callback = null;
}
TrXMLJsfAjaxRequest.prototype._ajaxCallback = function(
  data
  )
{
  switch (data.status)
  {
    case "begin":
      this._state = TrXMLRequest.LOADING;
      break;
    case "complete":
      this._state = TrXMLRequest.LOADED;
      break;
    case "success":
    default:
      this._state = TrXMLRequest.COMPLETED;
      break;
  }

  if (data.status != "begin")
  {
    this._status = data.responseCode;
    this._responseXML = data.responseXML;
    this._responseText = data.responseText;
  }

  if (this.callback)
  {
    this.callback(this);
  }
}
TrXMLJsfAjaxRequest.prototype.__onerror = function(
  data
  )
{
  this._state = TrXMLRequest.COMPLETED;
  this._status = data.responseCode;
  this._responseXML = data.responseXML;
  this._responseText = data.responseText;
  if (this.callback)
  {
    this.callback(this);
  }
}
TrXMLJsfAjaxRequest.prototype.send = function()
{
  var source = this._params.source ?
    _getElementById(window.document, this._params.source) : null;

  var ajaxCallback = TrUIUtils.createCallback(this, this._ajaxCallback);

  var payload = {
      "onevent": ajaxCallback,
      "onerror": ajaxCallback,
      "Tr-PPR-Message": true // Indicate that this a "legacy" PPR request sent over jsf.ajax
    };

  for (var p in this._params)
  {
    payload[p] = this._params[p];
  }

  // Unlike the Trinidad Request queue _getPostbackContent method, the core JSF ajax request
  // does not overwrite form values with parameter values. Due to the fact that this is expected
  // and Trinidad renderers add form hidden fields to the form (see CoreFormData source), we
  // need to update the form here and remove the values from the params.
  this._origFormValues = {};
  this._formElements = {};
  if (this._formId != null)
  {
    var formElement = document.getElementById(this._formId);
    if (formElement != null)
    {
      var formElements = formElement.elements;
      for (var i = 0; i < formElements.length; ++i)
      {
        var input = formElements[i];
        if (input.name && !input.disabled && !(input.tagName == "INPUT" && input.type == "submit"))
        {
          for (p in payload)
          {
            if (p == input.name)
            {
              var payloadValue = payload[p];
              delete payload[p];

              this._origFormValues[p] = input.value;
              this._formElements[p] = input;
              input.value = payloadValue;
              break;
            }
          }
        }
      }
    }
  }

  jsf.ajax.request(
    source,
    this._event,
    payload);

  // No need for the event anymore, release the resource
  delete this._event;
}
// No-op functions:
TrXMLJsfAjaxRequest.prototype.setSynchronous =
TrXMLJsfAjaxRequest.prototype.setRequestHeader = function() {}
TrXMLJsfAjaxRequest.prototype.getAllResponseHeaders = function()
{
  return new Object();
};
TrXMLJsfAjaxRequest.prototype.getResponseHeader = function()
{
  return null;
};
