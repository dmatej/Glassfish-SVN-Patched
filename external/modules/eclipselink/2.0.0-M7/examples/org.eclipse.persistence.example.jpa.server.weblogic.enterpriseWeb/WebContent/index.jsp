<%@ page language="java" contentType="text/html; charset=ISO-8859-1"
    pageEncoding="ISO-8859-1"%>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<!-- 
/*******************************************************************************
 * Copyright (c) 1998, 2008 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:     
 *     10/02/2008-1.1M1 Michael O'Brien 
 *       - 249541: Initial example tutorial submission for WebLogic 10 EAR
 *       - all 3 Eclipse projects required EAR, EJB and Web
 *       http://wiki.eclipse.org/EclipseLink/Examples/JPA/WebLogic_Web_Tutorial
 ******************************************************************************/
-->
<html>
    <head>
        <meta http-equiv="Content-Style-Type" content="text/css">
        <meta http-equiv="expires" content="Wed, 26 Feb 1997 08:21:57 GMT">
        <title>Enterprise JPA Example</title>
        <link rel="stylesheet" type="text/css" href="styles.css">

    </head>
    <body onload="javascript:self.window.location.assign('/enterprise/FrontController?action=demo&rnd=<%=String.valueOf(Math.random())%>');"
        text="#ffffff" bgcolor="#303030" link="#33D033" vlink="#D030D0" alink="#D03000">
    Loading FrontController servlet...
    </body>
</html>