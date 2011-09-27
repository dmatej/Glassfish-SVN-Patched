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
 * Simple function for opening a popup
 * @param contentId(String) id of the element to pop
 * @param triggerId(String) optional id of the element that launched the popup
 * @param event(Event) the javascript event object (used to position relative popups)
 * @param triggerType(String) 'click'(default) | 'hover'
 * @param position(String) 'relative'(default) | 'centered'
 * @param modal(boolean)
 * @param width(int) 
 * @param height(int)
 * @param xOffset(int)
 * @param yOffset(int)
 **/
TrPanelPopup.showPopup = function(
  contentId, 
  triggerId, 
  event, 
  triggerType,
  position, 
  modal, 
  width, 
  height, 
  xOffset, 
  yOffset)
{
  if (contentId == null)
    return;
  
  // Get/Initialize a map of visible popups
  var visiblePopups = TrPanelPopup._VISIBLE_POPUPS;
  if (!visiblePopups)
    visiblePopups = TrPanelPopup._VISIBLE_POPUPS = new Object();
  
  // Check if the popup is already visible
  if (visiblePopups[contentId])
    // Popup is already visible
    return;
    
  // Create new popup object and add it to the map of visible popups
  if (triggerType == "hover")
    visiblePopups[contentId] = new TrHoverPopup();
  else
    visiblePopups[contentId] = new TrClickPopup();

  var popup = visiblePopups[contentId];

  var content = document.getElementById(contentId);
  if (!content)
     return;

  popup.setContent(content);
  popup.setTrigger(document.getElementById(triggerId));
  popup.setModal(modal);
  popup.setCentered(position == 'centered');
  popup.setSize(width, height);
  popup.setRelativeOffsetX(xOffset);
  popup.setRelativeOffsetY(yOffset);
  
  popup.showPopup(event);
}

/**
 * Public function for hiding the current popup.
 */  
TrPanelPopup.hidePopup = function(event)
{
  event = window.event || event;
  var visiblePopups = TrPanelPopup._VISIBLE_POPUPS;
  if (!visiblePopups)
    return;

  //loop through element stack and find out which popup the event occured in.
  var currElement = event.target || event.srcElement;
  while (currElement)
  {
    var id = currElement.id;
    if (id)
    {
      var currPopup = visiblePopups[id];
      if (currPopup)
      {
        // We found the popup, so hide it.
        currPopup.hide(event);
        break;
      }
    }
    currElement = currElement.parentNode;
  }
}

/**
 * Class to handle a popup element on a page.
 */
function TrPanelPopup()
{
  //define object properties
  this._content = false;
  this._trigger = false;
  this._centered = false;
  this._modal = false;
  this._visible = false;
}

TrPanelPopup.prototype.getContent = function()
{
  return this._content;
}

TrPanelPopup.prototype.setContent = function(content)
{ 
  this._content = content;
  
  //Initialize the styles for the content
  if (this._content)
  {
    this._content.style.cssText  = "position: absolute; z-index: 5001; top: 0px; left: 0px; visibility:hidden; padding: 0px;";  
  }
}

/**
 * Get the element being used as the trigger
 **/
TrPanelPopup.prototype.getTrigger = function()
{
  return this._trigger;
}

/**
 * Sets the element to be used as the trigger to show the popup.  We
 * use this to ensure further events on the trigger don't cause a re-popup.
 * @param trigger(Element) The element that will trigger the popup.
 **/
TrPanelPopup.prototype.setTrigger = function(trigger)
{
  this._trigger = trigger;
}

/**
 * Sets the popup to be centered on screen when visible
 * @param centered(boolean) true if popup should be centered
 **/
TrPanelPopup.prototype.setCentered = function(centered)
{
  this._centered = centered;
}

/**
 * Returns true if the popup is set to modal.
 **/
TrPanelPopup.prototype.isModal = function()
{
  return this._modal;
}

/**
 * Sets the popup to be modal when visible
 */
TrPanelPopup.prototype.setModal = function(modal)
{
  this._modal = modal;
}

/**
 * Sets X offset to apply if popup is positioned relative to mouse x.
 * @param x(int) The x offset value.
 **/
TrPanelPopup.prototype.setRelativeOffsetX = function(x)
{
  this._relativeOffsetX = parseInt(x, 10);
}

/**
 * Gets X offset to apply if popup is positioned relative to mouse x.
 * @return (int) The x offset value, or zero if unset.
 **/
TrPanelPopup.prototype.getRelativeOffsetX = function()
{
  return (this._relativeOffsetX) ? this._relativeOffsetX: 0;
}

/**
 * Sets Y offset to apply if popup is positioned relative to mouse y.
 * @param y(int) The y offset value.
 **/
TrPanelPopup.prototype.setRelativeOffsetY = function(y)
{
  this._relativeOffsetY = parseInt(y, 10);
}

/**
 * Gets Y offset to apply if popup is positioned relative to mouse y.
 * @return (int) The y offset value, or zero if unset.
 **/
TrPanelPopup.prototype.getRelativeOffsetY = function()
{
  return (this._relativeOffsetY) ? this._relativeOffsetY: 0;
}


/**
 * Returns true if the popup is currently visible.
 **/
TrPanelPopup.prototype.isVisible = function()
{
  return this._visible;
}

/**
 * Holds the return value of the dialog.  Check this property after the 
 * popup has closed.
 **/
TrPanelPopup.prototype.returnValue = undefined;

/**
 * Attach a callback function that will be invoked when the popup
 * has been closed.  The callbackProps and returnValue properties will be
 * passed as parameters (e.g. function myCallback(props, value);).
 **/
TrPanelPopup.prototype.callback = undefined;

/**
 * Attach properties to the popup that will be passed to the callback function
 * (e.g. a component target to populate with the returnValue).
 **/
TrPanelPopup.prototype.callbackProps = undefined;

/**
 * Make the popup visible
 **/
TrPanelPopup.prototype.show = function(event)
{
  //we can't show content that isn't there
  if (!this.getContent())
    return;
 
  //don't pop during ppr - safety check
  if (_pprBlocking)
    return;

  //already visible
  if (this.isVisible())
    return;

  this._calcPosition(event);
  
  if (this.isModal())
    TrPanelPopup._showMask();
  
  TrPanelPopup._showIeIframe();

  this.getContent().style.visibility = "visible"; 
  
  this._visible = true;
}

/**
 * Hide the popup if visible.  Hiding the popup causes the callback
 * handler to be invoked (if configured).
 **/
TrPanelPopup.prototype.hide = function(event)
{
  //we can't hide content that isn't there
  if (!this.getContent())
    return;

  if (this.isModal())
    TrPanelPopup._hideMask();
  
  TrPanelPopup._hideIeIframe();
  
  this.getContent().style.visibility = "hidden";
  //move popup back to top left so it won't affect scroll size if window resized
  this.getContent().style.left = "0px";
  this.getContent().style.top = "0px";
  
  //call the callback function if attached
  if (this.callback)
  {
    try
    {
      this.callback(this.callbackProps, this.returnValue);
    }
    catch(ex)
    {
      alert("Error calling TrPanelPopup callback function:\n" + ex);
    }
  }
  
  this._visible = false;
  
  // Remove the popup from the list of visible popups
  var popups = TrPanelPopup._VISIBLE_POPUPS;
  if (popups)
    delete popups[this.getContent().id];
}

/**
 * Size the popup to a specific width and height
 */
TrPanelPopup.prototype.setSize = function(width, height)
{
  if (width)
  {
    var i = parseInt(width, 10);
    if (i > 0)
      this.getContent().style.width = i + "px";
  }
  if (height)
  {
    var i = parseInt(height, 10);
    if (i > 0)
      this.getContent().style.height = i + "px";
  }
}

// The modal mask - shared by all instances
TrPanelPopup._mask = undefined;

/**
 * Show the popup mask that blocks clicks in modal mode.  Initialize it
 * if not already.
 **/
TrPanelPopup._showMask = function()
{
  //initialise mask only once
  if (!TrPanelPopup._mask)
  {
    //create mask for modal popups
    TrPanelPopup._mask = document.createElement('div');
    TrPanelPopup._mask.name = "TrPanelPopup._BlockingModalDiv";

    //optional: id of blocked area
    TrPanelPopup._mask.id = "af_dialog_blocked-area";

    //set style class for blocked area
    var page = TrPage.getInstance();
    TrPanelPopup._mask.className = page.getStyleClass("af|dialog::blocked-area");

    var cssText = "display:none;position: absolute; z-index: 5000;top: 0px;left: 0px;cursor: not-allowed;";
    if (_agent.isIE && _agent.version == 7)
      //workaround for bug in IE7 : see http://blog.thinkature.com/index.php/2006/12/29/odd-mouse-handling-with-transparent-objects-under-internet-explorer-7/
      cssText = cssText + "background-color: white; filter: alpha(opacity=0);";
    else
      cssText = cssText + "background-color: transparent";
    TrPanelPopup._mask.style.cssText = cssText;
    TrPanelPopup._mask.innerHTML = "&nbsp;";

    //add mask to body
    document.body.appendChild(TrPanelPopup._mask);
  }

  TrPanelPopup._registerMaskEvents();

  //set initial mask size
  TrPanelPopup._setMaskSize();

  TrPanelPopup._mask.style.display = "block";
  
}

TrPanelPopup._registerMaskEvents = function()
{
  //consume all events
  _addEvent(TrPanelPopup._mask, "click", TrPanelPopup._consumeMaskEvent);

  //handle window resize events
  _addEvent(window, "resize", TrPanelPopup._setMaskSize);

  //handle window scroll events
  _addEvent(window, "scroll", TrPanelPopup._setMaskSize);
}

/**
 * Hide the popup mask that blocks clicks in modal mode.
 **/
TrPanelPopup._hideMask = function()
{
  _removeEvent(TrPanelPopup._mask, "click", TrPanelPopup._consumeMaskEvent);
  _removeEvent(window, "resize", TrPanelPopup._setMaskSize);
  _removeEvent(window, "scroll", TrPanelPopup._setMaskSize);
  TrPanelPopup._mask.style.display = "none";
}

/**
 * Check to see if a point lies inside the bounds of an element
 */
TrPanelPopup.prototype._hitTest = function(element, eventLoc)
{
  var b = TrUIUtils._getElementBounds(element);
  
  return b.x <= eventLoc.pageX && (b.x + b.w) >= eventLoc.pageX &&
    b.y <= eventLoc.pageY && (b.y + b.h) >= eventLoc.pageY;
}

/**
 * Reposition an element to ensure that it fits on the screen
 */
TrPanelPopup.prototype._fitOnScreen = function(element, windowSize)
{
  var vis = TrUIUtils._getStyle(element, 'visibility');
  element.style.visibility = 'hidden';
  var b = TrUIUtils._getElementBounds(element);
  var parentLoc = TrUIUtils._getElementLocation(element.offsetParent);
  var posType = TrUIUtils._getStyle(element.offsetParent, 'position');
  var parentOffset;
  if (posType == 'relative' || posType == 'absolute')
  {
    parentOffset = { left: parentLoc.x, top: parentLoc.y };
  }
  else
  {
    parentOffset = { left: 0, top: 0 };
  }
  
  // calculate the client location of the popup (not the page location)
  var clientLoc = { 
    x: b.x - (document.body.scrollLeft || document.documentElement.scrollLeft),
    y: b.y - (document.body.scrollTop || document.documentElement.scrollTop)
  };
  
  // is the popup off the page to the left?
  if (b.x < 0)
  {
    element.style.left = (0 - parentOffset.left) + 'px';
  }
  // is it off the page to the right?
  else if (clientLoc.x + b.w > windowSize.w)
  {
    element.style.left = (element.offsetLeft - (clientLoc.x + b.w - windowSize.w)) + 'px';
  }

  // is the popup off the top of the page?
  if (b.y < 0)
  {
    element.style.top = (0 - parentOffset.top) + 'px';
  }
  // is it past the bottom the page?
  else if (clientLoc.y + b.h > windowSize.h)
  {
    element.style.top = (element.offsetTop - (clientLoc.y + b.h - windowSize.h)) + 'px';
  }
  element.style.visibility = vis;
}

/**
 * Get the page X and Y and the client X and Y of an event
 */
TrPanelPopup.prototype._getEventPosition = function(event)
{
  // all browsers
  var pos = { 
    clientX: event.clientX,
    clientY: event.clientY,
    pageX: event.pageX,
    pageY: event.pageY
  };

  if (pos.pageX == null)
  {
    pos.pageX = event.clientX
      + (document.body.scrollLeft || document.documentElement.scrollLeft);
    pos.pageY = event.clientY
      + (document.body.scrollTop || document.documentElement.scrollTop);
  }

  return pos;
}

/**
 * Function to center an element on the screen
 */
TrPanelPopup.prototype._centerOnScreen = function(element, windowSize)
{
  element.style.position = 'absolute';
  var vis = TrUIUtils._getStyle(element, 'visibility');
  element.style.visibility = 'hidden'; // prevent flickering
  var parentLoc = TrUIUtils._getElementLocation(element.offsetParent);
  var pageLoc = TrUIUtils._getElementBounds(element);
  var posType = TrUIUtils._getStyle(element.offsetParent, 'position');
  var parentOffset;
  if (posType == 'relative' || posType == 'absolute')
  {
    parentOffset = { left: parentLoc.x, top: parentLoc.y };
  }
  else
  {
    parentOffset = { left: 0, top: 0 };
  }
    
  // calculate the client location of the popup (not the page location)
  var clientLoc = { 
    x: pageLoc.x - (document.body.scrollLeft || document.documentElement.scrollLeft),
    y: pageLoc.y - (document.body.scrollTop || document.documentElement.scrollTop)
  };

  element.style.left = Math.max(0,
    (windowSize.w / 2 - element.clientWidth / 2)
    - parentOffset.left
    + (pageLoc.x - clientLoc.x)) + 'px';
  
  element.style.top = Math.max(0,
    (windowSize.h / 2 - element.clientHeight / 2)
    - parentOffset.top
    + (pageLoc.y - clientLoc.y)) + 'px';
  
  element.style.visibility = vis;
}

/**
 * Get the element to add to the dialog to, to ensure dialog
 * positioning
 */
TrPanelPopup.prototype._getOffsetParent = function()
{
  for (var elem = this.getContent(); elem != null;
    elem = elem.parentNode)
  {
    if (elem.tagName && 'form' == elem.tagName.toLowerCase())
    {
      return elem;
    }
  }
  return document.body;
}

/**
 * Position the popup ensuring it doesn't go off-page, and if centered, then 
 * center in the middle of the current window.
 */
TrPanelPopup.prototype._calcPosition = function(event)
{
  var popup = this.getContent();
  event = window.event || event;
  
  var parent = this._getOffsetParent(); 
  // get the window size before the popup may alter it
  var wSize = TrUIUtils._getWindowClientSize();
  
  if (!popup.origParent)
  {
    popup.origParent = popup.parentNode;
  }  
  parent.appendChild(popup);

  if (!this._centered)
  {
    var eventP = this._getEventPosition(event);
    var parLoc = TrUIUtils._getElementLocation(popup.offsetParent);
    var posType = TrUIUtils._getStyle(popup.offsetParent, 'position');
    var parentOffset;
    if (posType == 'relative' || posType == 'absolute')
    {
      parentOffset = { left: parLoc.x, top: parLoc.y };
    }
    else
    {
      parentOffset = { left: 0, top: 0 };
    }

    // set the location to the location of the event on the page
    // adjusted to the parent location on the page
    // adjusted by the relative offset of the popup
    popup.style.left = (eventP.pageX - parentOffset.left + this.getRelativeOffsetX() -
      this._getSideOffset(popup, "Left")) + 'px';
    
    popup.style.top = (eventP.pageY - parentOffset.top + this.getRelativeOffsetY() -
      this._getSideOffset(popup, "Top")) + 'px';
  }
  
  if (this._centered)
  {
    this._centerOnScreen(popup, wSize);
  }
  else
  {
    this._fitOnScreen(popup, wSize);
  }
  
  if (!this.isModal())
  {
    var b = TrUIUtils._getElementBounds(popup);
    TrPanelPopup._resizeIeIframe(b.x, b.y, b.w, b.h);
  }
}

TrPanelPopup.prototype._getSideOffset = function(elem, side)
{
  var arr = [ "border", "padding", "margin" ];
  var val = 0;
  for (var i = 0; i < arr.length; ++i)
  {
    var o = TrUIUtils._getStyle(elem, arr[i] + side);
    o = parseInt(o, 10);
    if (!isNaN(o))
    {
      val += o;
    }
  }
  return val;
}

/**
 * Simple event handler that consumes any clicks when modal popup is shown
 */
TrPanelPopup._consumeMaskEvent = function(event)
{
  return false;
}

/**
 * Sizes/resizes the modal mask if the window size changes
 */
TrPanelPopup._setMaskSize = function()
{
  //only bother if mask is inited
  if (!TrPanelPopup._mask)
    return;

  var wSize = TrUIUtils._getWindowClientSize();
  var scrollWidth = document.documentElement.scrollWidth || document.body.scrollWidth;
  var scrollHeight = document.documentElement.scrollHeight || document.body.scrollHeight;
    
  var w = Math.max(wSize.w, scrollWidth);
  var h = Math.max(wSize.h, scrollHeight);
  
  TrPanelPopup._mask.style.width = w + "px";
  TrPanelPopup._mask.style.height = h + "px";
  
  TrPanelPopup._resizeIeIframe(0, 0, w, h);
}

/**
 * FUNCTIONS BELOW IMPLEMENT CSS/IFRAME WORKAROUND FOR THE INFAMOUS IE 6.x SELECT ZINDEX BUG
 * More info here: http://dotnetjunkies.com/WebLog/jking/archive/2003/07/21/488.aspx
 **/
TrPanelPopup._showIeIframe = function()
{
  if (_agent.isIE && _agent.version < 7)
  {
    TrPanelPopup._initIeIframe();
    TrPanelPopup._maskIframe.style.display = "block";      
  }
}

TrPanelPopup._hideIeIframe = function()
{
  if (_agent.isIE && _agent.version < 7)
  {
    TrPanelPopup._initIeIframe();
    TrPanelPopup._maskIframe.style.display = "none";      
  }
}

TrPanelPopup._resizeIeIframe = function(left, top, width, height)
{
  if (_agent.isIE && _agent.version < 7)
  {
    TrPanelPopup._initIeIframe();
    TrPanelPopup._maskIframe.style.left = left;
    TrPanelPopup._maskIframe.style.top = top;
    TrPanelPopup._maskIframe.style.width = width;
    TrPanelPopup._maskIframe.style.height = height;
  }
}

TrPanelPopup._initIeIframe = function()
{
  if (!TrPanelPopup._maskIframe)
  {
    //create single reusable iframe if not already inited
    TrPanelPopup._maskIframe = document.createElement('iframe');
    TrPanelPopup._maskIframe.name = "TrPanelPopup._ieOnlyZIndexIframe";
    TrPanelPopup._maskIframe.style.cssText = "display: none; left: 0px; position: absolute; top: 0px; z-index: 4999;";
    TrPanelPopup._maskIframe.style.filter = "progid:DXImageTransform.Microsoft.Alpha(style=0,opacity=0)";
    // avoid SSL warnings in IE
    if (_agent.isIE)
    {
      TrPanelPopup._maskIframe.src = "javascript:false;";
    }
    document.body.appendChild(TrPanelPopup._maskIframe);
  }
}








function TrHoverPopup()
{
  TrPanelPopup.call(this);

  // Setup callback function for hiding the popup
  this._hoverCallbackFunction = TrUIUtils.createCallback(this, this.hidePopup);
}

// TrHoverPopup inherits from TrPanelPopup
TrHoverPopup.prototype = new TrPanelPopup();

TrHoverPopup.prototype.showPopup = function(event)
{
  // Setup event listener for mouse leaving trigger or content elements
  //_addEvent(this.getTrigger(), "mouseout", this._hoverCallbackFunction);
  //_addEvent(this.getContent(), "mouseout", this._hoverCallbackFunction);
  
  // mouse out on the elements didn't always get called, so use mouse move
  _addEvent(document.body, "mousemove", this._hoverCallbackFunction);
  
  this.show(event);
}

TrHoverPopup.prototype.hidePopup = function(event)
{
  event = window.event || event;
  
  var popup = this.getContent();
  var trigger = this.getTrigger();
  
  var eventPos = this._getEventPosition(event);
  
  // see if event is in the popup or the trigger bounds
  if ((this._hitTest(popup, eventPos) ||
    this._hitTest(trigger, eventPos)))
  {
    return;
  }
  
  // Cancel event listeners
  // mouse out on the elements didn't always get called, so use mouse move
  //_removeEvent(this.getTrigger(), "mouseout", this._hoverCallbackFunction);
  //_removeEvent(this.getContent(), "mouseout", this._hoverCallbackFunction);
  _removeEvent(document.body, "mousemove", this._hoverCallbackFunction);

  this.hide(event);
  
  if (popup.origParent)
  {
    popup.origParent.appendChild(popup);
  }
}

TrHoverPopup.prototype.isModal = function()
{
  // Prevent modal for hover popups
  return false;
}




function TrClickPopup()
{
  TrPanelPopup.call(this);

  // Setup callback function for hiding the popup
  this._clickCallbackFunction = TrUIUtils.createCallback(this, this.hidePopup);
}

// TrHoverPopup inherits from TrPanelPopup
TrClickPopup.prototype = new TrPanelPopup();

TrClickPopup.prototype.showPopup = function(event)
{
  if (!this.isModal())
    // Setup event listener for clicking off the popup
    _addEvent(document, "click", this._clickCallbackFunction);
    
  this.show(event);
}

TrClickPopup.prototype.hidePopup = function(event)
{
  //loop through element stack where event occurred
  event = window.event || event;
  var currElement = event.target || event.srcElement;
  while (currElement)
  {
    //if clicked on trigger or popup  
    if (currElement == this.getContent() || 
        currElement == this.getTrigger())
    {
      break;
    }
    currElement = currElement.parentNode;
  }
  
  if (!currElement)
  {
    // Cancel event listeners
    _removeEvent(document, "click", this._clickCallbackFunction);

    //if click was on something other than the popupContainer
    this.hide(event);
    
    if (this.getContent().origParent)
    {
      this.getContent().origParent.appendChild(this.getContent());
    }
  }
}


