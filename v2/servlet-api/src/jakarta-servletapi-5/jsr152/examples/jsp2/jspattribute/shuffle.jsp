<%--
  DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.

  Copyright 1997-2007 Sun Microsystems, Inc. All rights reserved.

  Portions Copyright Apache Software Foundation.

  The contents of this file are subject to the terms of either the GNU
  General Public License Version 2 only ("GPL") or the Common Development
  and Distribution License("CDDL") (collectively, the "License").  You
  may not use this file except in compliance with the License. You can obtain
  a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
  or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
  language governing permissions and limitations under the License.

  When distributing the software, include this License Header Notice in each
  file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
  Sun designates this particular file as subject to the "Classpath" exception
  as provided by Sun in the GPL Version 2 section of the License file that
  accompanied this code.  If applicable, add the following below the License
  Header, with the fields enclosed by brackets [] replaced by your own
  identifying information: "Portions Copyrighted [year]
  [name of copyright owner]"

  Contributor(s):

  If you wish your version of this file to be governed by only the CDDL or
  only the GPL Version 2, indicate your decision by adding "[Contributor]
  elects to include this software in this distribution under the [CDDL or GPL
  Version 2] license."  If you don't indicate a single choice of license, a
  recipient has the option to distribute your version of this file under
  either the CDDL, the GPL Version 2 or to extend the choice of license to
  its licensees as provided above.  However, if you add GPL Version 2 code
  and therefore, elected the GPL Version 2 license, then the option applies
  only if the new code is made subject to such option by the copyright
  holder.
--%>

<%@ taglib prefix="my" uri="http://jakarta.apache.org/tomcat/jsp2-example-taglib"%>

<html>
  <head>
    <title>JSP 2.0 Examples - Shuffle Example</title>
  </head>
  <body>
    <h1>JSP 2.0 Examples - Shuffle Example</h1>
    <hr>
    <p>Try reloading the page a few times.  Both the rows and the columns
    are shuffled and appear different each time.</p>
    <p>Here's how the code works.  The SimpleTag handler called 
    &lt;my:shuffle&gt; accepts three attributes.  Each attribute is a 
    JSP Fragment, meaning it is a fragment of JSP code that can be
    dynamically executed by the shuffle tag handler on demand.  The 
    shuffle tag handler executes the three fragments in a random order.
    To shuffle both the rows and the columns, the shuffle tag is used
    with itself as a parameter.</p>
    <hr>
    <blockquote>
     <font color="#ffffff">
      <table>
        <my:shuffle>
          <jsp:attribute name="fragment1">
            <tr>
              <my:shuffle>
                <jsp:attribute name="fragment1">
                  <my:tile color="#ff0000" label="A"/>
                </jsp:attribute>
                <jsp:attribute name="fragment2">
                  <my:tile color="#00ff00" label="B"/>
                </jsp:attribute>
                <jsp:attribute name="fragment3">
                  <my:tile color="#0000ff" label="C"/>
                </jsp:attribute>
              </my:shuffle>
            </tr>
          </jsp:attribute>
          <jsp:attribute name="fragment2">
            <tr>
              <my:shuffle>
                <jsp:attribute name="fragment1">
                  <my:tile color="#ff0000" label="1"/>
                </jsp:attribute>
                <jsp:attribute name="fragment2">
                  <my:tile color="#00ff00" label="2"/>
                </jsp:attribute>
                <jsp:attribute name="fragment3">
                  <my:tile color="#0000ff" label="3"/>
                </jsp:attribute>
              </my:shuffle>
            </tr>
          </jsp:attribute>
          <jsp:attribute name="fragment3">
            <tr>
              <my:shuffle>
                <jsp:attribute name="fragment1">
                  <my:tile color="#ff0000" label="!"/>
                </jsp:attribute>
                <jsp:attribute name="fragment2">
                  <my:tile color="#00ff00" label="@"/>
                </jsp:attribute>
                <jsp:attribute name="fragment3">
                  <my:tile color="#0000ff" label="#"/>
                </jsp:attribute>
              </my:shuffle>
            </tr>
          </jsp:attribute>
        </my:shuffle>
      </table>
     </font>
    </blockquote>
  </body>
</html>
