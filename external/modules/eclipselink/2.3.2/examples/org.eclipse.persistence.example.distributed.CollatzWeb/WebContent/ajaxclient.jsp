<%@ page language="java" contentType="text/html; charset=ISO-8859-1" pageEncoding="ISO-8859-1"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
        <meta http-equiv="Content-Type" content="text/html; charset=ISO-8859-1">
        <meta http-equiv="Content-Style-Type" content="text/css">
        <meta http-equiv="expires" content="Wed, 26 Feb 1997 08:21:57 GMT">
        <link rel="stylesheet" type="text/css" href="styles.css">        

<title>Collatz Java EE Active Client</title>
<% int processingUnits = 6; %>
<script language="JavaScript" type="text/javascript">
 function FactoryXMLHttpRequest() {
	   if(window.XMLHttpRequest) {
		   // Mozilla
		   return new XMLHttpRequest();
	   } else if(window.ActiveXObject) {
		   try {
		    // Internet Explorer only
            return new ActiveXObject("Microsoft.XMLHTTP");
		   } catch (e) {
		   }
       }
       // Verify Chrome, Firefox, Opera and Apple
       throw new Error("Could not get an AJAX XMLHttpRequest Object");
 }

 function Ajax() {
	 this._xmlHttp = new FactoryXMLHttpRequest();
 }

 // This code is loosely based on 
 // p.22 of Ajax Patterns and Best Practices by Christian Gross (2006)
 // http://books.google.com/books?id=qNzBbUWhGM0C&printsec=frontcover&dq=ajax+patterns+and&cd=2#v=onepage&q&f=false
 // and http://www.w3schools.com/js/js_timing.asp
 // htmlDOMElementId must be an integer
 function AjaxUpdateEvent(htmlDOMelementId, status, statusText, responseText, responseXML) {
	 // This line updates the DOM <span id="[0-9]+"/>
	 document.getElementById(htmlDOMelementId).innerHTML = responseText;
	 // flash data cell only if data has changed
	 var color = "orange";
	 if(flicker[htmlDOMelementId] < 1) {
		 flicker[htmlDOMelementId] = 1;
		 color = "white";
	 } else {
		 flicker[htmlDOMelementId] = 0;
	 }
     if(responseText != lastValue[htmlDOMelementId]) {
         document.getElementById(htmlDOMelementId).style.color = color;
         lastValue[htmlDOMelementId] = responseText;
     }
	 
 }

 // display flags
 var flicker = new Array();
 var lastValue = new Array();
 // Timer variables
 var timers = new Array();
 var timerStateArray = new Array();
 var ajax = new Array();
 // initialize all arrays
 for (i=0;i<<%=processingUnits%>;i=i+1) {
     timerStateArray[i] = 0;
     ajax[i] = new Ajax();
     ajax[i].complete = AjaxUpdateEvent;
     flicker[i] = 0;
 }
 
 function Ajax_call(url, htmlDOMelementId) {
	 var instance = this;
	 this._xmlHttp.open('GET', url, true);
	 // inner anonymous function
	 this._xmlHttp.onreadystatechange = function() {
		 switch(instance._xmlHttp.readyState) {
		 case 1:
			 instance.loading();
			 break;
		 case 2:
			 instance.loaded();
			 break;
		 case 3:
			 instance.interactive();
			 break;
		 case 4:
			 // pass parameters
			 instance.complete(
					 htmlDOMelementId,
					 instance._xmlHttp.status,
					 instance._xmlHttp.statusText,
					 instance._xmlHttp.responseText,
					 instance._xmlHttp.responseXML);
			 break;
		 }
	 };
	 this._xmlHttp.send(null);
 }

 function Ajax_loading(){ }
 function Ajax_loaded(){ }
 function Ajax_interactive(){ }
 function Ajax_complete(htmlDOMelementId, status, statusText, responseText, responseHTML){ }

 // create static class functions
 Ajax.prototype.loading = Ajax_loading;
 Ajax.prototype.loaded = Ajax_loaded;
 Ajax.prototype.interactive = Ajax_interactive;
 Ajax.prototype.complete = Ajax_complete;
 Ajax.prototype.call = Ajax_call;

 // Base case: Switch the timer flag and call the main loop
 function doTimer(url,cell,speed,htmlDOMelementId) {
     if(!timerStateArray[cell]) {
    	  timerStateArray[cell] = 1;
          timedCall(url,cell,speed,htmlDOMelementId);
     }
 }
 // Main timing loop calls itself
 function timedCall(url,cell,speed,htmlDOMelementId) {
     ajax[cell].call(url,htmlDOMelementId);
     if(timerStateArray[cell]) {
           timers[cell] = setTimeout(function() { timedCall(url,cell,speed,htmlDOMelementId); }, speed); // no speed = max speed
     }
 }
 
 
</script>
</head>
<body text="#ffffff" bgcolor="#303030" link="#33D033" vlink="#D030D0" alink="#D03000">
<!-- @&rnd=<%=String.valueOf(Math.random())%>')"-->
 <table border="0">
        <%
        for(int i=0;i<processingUnits;i++) {
        out.println("<tr bgcolor=\"#2d1d4f\">");
        out.println("<td><span id=\"" + i + "\" class=\"refdesc-d\">" + i + "</span>");
        out.println("</td>");
        out.println("<td><span id=\"b" + i + "\">");
        out.println(" <button ");
        out.println("onMouseOver=\"ajax[" + i + "].call('/collatz/FrontController?action=getStatistic&cell=" + i + "','" + i + "')\"  ");
        out.println("onclick=\"doTimer('/collatz/FrontController?action=getStatistic&cell=" + i + "'," + i + ",400,'" + i + "')\">on</button>");
        out.println(" <button ");
        out.println("onclick=\"timerStateArray[" + i + "]=0\">off</button>");
        out.println("</span>");
        out.println("</td>");
        out.println("</tr>");
        }        
        %>
 </table>
 <br/><br/><br/><br/><br/>
 <p class="ref2">Make sure that "Tools | Internet Options | Browsing History - is set to "Every time I visit the webpage" instead of the default "Automatically" so the XMLHttpRequest call will reach the server on subsequent calls.</p>
</body>
</html>