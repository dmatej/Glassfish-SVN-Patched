/*
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS HEADER.
 *
 * Copyright 2009 Sun Microsystems, Inc. All rights reserved.
 *
 * The contents of this file are subject to the terms of either the GNU
 * General Public License Version 2 only ("GPL") or the Common Development
 * and Distribution License("CDDL") (collectively, the "License").  You
 * may not use this file except in compliance with the License. You can obtain
 * a copy of the License at https://glassfish.dev.java.net/public/CDDL+GPL.html
 * or glassfish/bootstrap/legal/LICENSE.txt.  See the License for the specific
 * language governing permissions and limitations under the License.
 *
 * When distributing the software, include this License Header Notice in each
 * file and include the License file at glassfish/bootstrap/legal/LICENSE.txt.
 * Sun designates this particular file as subject to the "Classpath" exception
 * as provided by Sun in the GPL Version 2 section of the License file that
 * accompanied this code.  If applicable, add the following below the License
 * Header, with the fields enclosed by brackets [] replaced by your own
 * identifying information: "Portions Copyrighted [year]
 * [name of copyright owner]"
 *
 * Contributor(s):
 *
 * If you wish your version of this file to be governed by only the CDDL or
 * only the GPL Version 2, indicate your decision by adding "[Contributor]
 * elects to include this software in this distribution under the [CDDL or GPL
 * Version 2] license."  If you don't indicate a single choice of license, a
 * recipient has the option to distribute your version of this file under
 * either the CDDL, the GPL Version 2 or to extend the choice of license to
 * its licensees as provided above.  However, if you add GPL Version 2 code
 * and therefore, elected the GPL Version 2 license, then the option applies
 * only if the new code is made subject to such option by the copyright
 * holder.
 */

/*
 * $Id$
 */
 
var toplevel = this;

if (toplevel["arguments"] != undefined) {
    if (arguments.length != 2) {
        throw new Error("invalid number of arguments (2 expected, got " + arguments.length + ")");
    }

    var phase = arguments[0];
    var baseDir = arguments[1] + "/";
    var sourceDir = baseDir + "src/";
    var testDir = baseDir + "test/";
    var buildDir = baseDir + "build/"
}
else if (toplevel["project"] != undefined) {
    var baseDir = project.getProperty("basedir") + "/";
    var sourceDir = project.getProperty("src.dir") + "/";
    var testDir = project.getProperty("test.dir") + "/";
    var buildDir = project.getProperty("build.dir") + "/";
    var phase = project.getProperty("phase");    
}
else {
    throw new Error("script was not invoked correctly")
}
var debug = false;

//////////////// generating

// pass 1 - expand #include directives
//          this will become legacy as soon as we replace #include with XInclude
function passOne(inputFilename) {
    printDebug("PREPROCESSING " + inputFilename);

    return readExpandingIncludes(sourceDir + inputFilename);

    function readExpandingIncludes(aFilename) {
        var pattern = java.util.regex.Pattern.compile('^#include\\s+"(.*)"');
        function readOne(aFilename, builder, alreadyRead) {
            var reader = new java.io.BufferedReader(new java.io.FileReader(aFilename));
            var line;
            while ((line = reader.readLine()) != null) {
                var matcher = pattern.matcher(line);
                if (matcher.matches()) {
                    var includedFileName = matcher.group(1);
                    if (alreadyRead[includedFileName]) {
                        throw Error("circular include detected in file: " + aFilename + " attempting to include: " + includedFileName);
                    }
                    alreadyRead[includedFileName] = true;
                    readOne(sourceDir + includedFileName, builder, alreadyRead);
                }
                else {
                    builder.append(line);
                    builder.append("\n");
                }
            }
            reader.close();
        }
        var builder = new java.lang.StringBuilder();
        readOne(aFilename, builder, {});
        return builder.toString();
    }
}

function stripComments(aFilename) {
    printDebug("STRIPPING COMMENTS FROM: " + aFilename);
    var reader = new java.io.BufferedReader(new java.io.FileReader(aFilename));
    var line;
    var builder = new java.lang.StringBuilder();
    while ((line = reader.readLine()) != null) {
        builder.append(line);
        builder.append("\n");
    }
    var contents = String(builder.toString());
    return contents.replace(/\/\*((.|\n)*)\*\//mg, "");
}

// pass 2 - parse the results as XML, format properly, emit xsd file
function passTwo(outputFilename, reader) {
    printDebug("GENERATING " + outputFilename);

    var writer = new java.io.OutputStreamWriter(new java.io.FileOutputStream(outputFilename), "utf-8");
    try {
        function HandlerImpl(writer) {
            this.w = new java.io.PrintWriter(writer);
        };
        HandlerImpl.prototype.INDENT = 2;
        HandlerImpl.prototype.DIVIDER = "\n<!-- **************************************************** -->\n";
        HandlerImpl.prototype.doindent = function(i) {
            while (i-- > 0) {
                this.w.print(" ");
            }
        }
        HandlerImpl.prototype.startDocument = function() {
            this.level = 0;
            this.indent = 0;
            this.inDocumentation = false;
            this.justStarted = false;
            this.gotComplexType = false;
            this.w.println('<?xml version=\"1.0\" encoding=\"UTF-8"\?>');
        };
        HandlerImpl.prototype.endDocument = function() {
            if (this.level != 0) {
                throw new Error("should not happen")
            }
            this.w.flush();
        };
        HandlerImpl.prototype.startElement = function(uri, localName, qName, attributes) {
            if (this.justStarted) {
                this.completeStartTag();
            }
        
            if (this.level == 1 && qName == "xsd:element" || qName == "xsd:complexType") {
                this.w.println(this.DIVIDER);
            }

            if (this.level == 1 && this.gotComplexType && qName == "xsd:element") {
                this.styleWarning("xsd:element declaration must precede, not follow any xsd:complexType declarations");
            }
            if (this.level == 1 && !this.gotComplexType && qName == "xsd:complexType") {
                this.gotComplexType = true;
            }
        
            this.level++;
            this.doindent(this.indent);
            this.w.print("<" + qName);
            if (attributes.length == 0) {
                this.indent += this.INDENT;
            }
            else {
                this.w.print(" ");
                this.indent += qName.length() + this.INDENT;
                for (var i = 0; i < attributes.length; ++i) {
                    if (i > 0) {
                        this.w.println();
                        this.doindent(this.indent);
                    }
                    this.w.print(attributes.getQName(i));
                    this.w.print('="');
                    this.w.print(escapeAttributeValue(attributes.getValue(i)));
                    this.w.print('"');
                }
                this.indent -= qName.length();
            }
            this.justStarted = true;
            this.inDocumentation = (qName == "xsd:documentation");
            if (this.inDocumentation) {
                this.collectedDocstring = "";
            }
        };
        HandlerImpl.prototype.endElement = function(uri, localName, qName) {
            if (this.inDocumentation) {
                var needsCDATA = containsAngleBrackets(this.collectedDocstring);
                var lines = this.collectedDocstring.split("\n");
                if (lines.length > 0) {
                    if (needsCDATA) {
			            this.doindent(this.indent);
                        this.w.print("<![CDATA[[");
                    }
                    this.w.println();
                }
                var lastLineWasEmpty = true;
                for (var i = 0; i < lines.length; ++i) {
                    var line = lines[i];
                    line = line.replace("\t", "    ");
                    for (var j = 0; j < line.length; ++j) {
                        if (line.charAt(j) != " ") {
                            break;
                        }
                    }
                    line = line.substring(j);
                    if (line == "") {
                        if (lastLineWasEmpty) {
                            continue;
                        }
                        else {
                            lastLineWasEmpty = true;
                        }
                    }
                    else {
                        lastLineWasEmpty = false;
                    }
                    this.doindent(this.indent);
                    this.w.println(line);
                }
            }
            if (needsCDATA) {
	            this.doindent(this.indent);
                this.w.println("]]>");
            }
            this.level--;
            this.indent -= this.INDENT;
            if (this.justStarted) {
                this.w.println("/>");
                this.justStarted = false;
            }
            else {
                this.doindent(this.indent);
                this.w.println("</" + qName + ">");
            }
            if (this.level == 1) {
                this.w.println();
            }
            this.inDocumentation = false;
        };
        HandlerImpl.prototype.completeStartTag = function() {
            this.w.println(">");
            this.justStarted = false;        
        };
        HandlerImpl.prototype.characters = function(ch, start, length) {
            if (this.justStarted) {
                this.completeStartTag();
            }
            var s = new java.lang.String(ch, start, length);
            if (this.inDocumentation) {
                this.collectedDocstring += s;
            }
            else if (!isWhitespace(s)) {
                this.styleWarning("found non-whitespace character content outside of xsd:documentation elements");
            }
        }
        HandlerImpl.prototype.ignorableWhitespace = function(ch, start, length) {
            if (this.justStarted) {
                this.completeStartTag();
            }
            var s = new java.lang.String(ch, start, length);
            if (this.inDocumentation) {
                this.collectedDocstring += s;
            }
        }
        HandlerImpl.prototype.resolveEntity = function(publicId, systemId) {
            // printDebug("RESOLVING RESOURCE (" + publicId + "," + systemId + ")");
            var prefix = "file://" + baseDir;
            if (systemId.startsWith(prefix)) {
                var mapsTo = new java.lang.String(sourceDir + systemId.substring(prefix.length));
                if (mapsTo.endsWith(".inc")) {
                    return new Packages.org.xml.sax.InputSource(new java.io.StringReader(stripComments(mapsTo)));
                }
                else {
                    var newUrl = "file://" + sourceDir + systemId.substring(prefix.length);
                    return new Packages.org.xml.sax.InputSource(newUrl);
                }
            }
            else {
                return null;
            }
        }
        HandlerImpl.prototype.setDocumentLocator = function(locator) {
            this.locator = locator;
        }
        HandlerImpl.prototype.styleWarning = function(s) {
            print("style warning: " + s + " in line " + this.locator.getLineNumber());
        }

        var handler = new JavaAdapter(Packages.org.xml.sax.helpers.DefaultHandler, new HandlerImpl(writer));
        var spf = Packages.javax.xml.parsers.SAXParserFactory.newInstance();
        spf.setNamespaceAware(true);
        spf.setXIncludeAware(true);
        spf.setFeature("http://xml.org/sax/features/namespace-prefixes", true);
        var sp = spf.newSAXParser();
        var is = new Packages.org.xml.sax.InputSource(reader);
        sp.parse(is, handler);
    }
    finally {
        writer.close();
    }
}

function passThree(schemaFilename) {
    printDebug("VALIDATING " + schemaFilename);

    function LSInputImpl() {}
    LSInputImpl.prototype.getBaseURI = function() {
        return this._baseURI;
    }
    LSInputImpl.prototype.getByteStream = function() {
        return this._byteStream;
    }
    LSInputImpl.prototype.getCertifiedText = function() {
        return this._certifiedText;
    }
    LSInputImpl.prototype.getCharacterStream = function() {
        return this._characterStream;
    }
    LSInputImpl.prototype.getEncoding = function() {
        return this._encoding;
    }
    LSInputImpl.prototype.getPublicId = function() {
        return this._publicId;
    }
    LSInputImpl.prototype.getStringData = function() {
        return this._stringData;
    }
    LSInputImpl.prototype.getSystemId = function() {
        return this._systemId;
    }
    LSInputImpl.prototype.setBaseURI = function(v) {
        this._baseURI = v;
    }
    LSInputImpl.prototype.setByteStream = function(v) {
        this._byteStream = v;
    }
    LSInputImpl.prototype.setCertifiedText = function(v) {
        this._certifiedText = v;
    }
    LSInputImpl.prototype.setCharacterStream = function(v) {
        this._characterStream = v;
    }
    LSInputImpl.prototype.setEncoding = function(v) {
        this._encoding = v;
    }
    LSInputImpl.prototype.setPublicId = function(v) {
        this._publicId = v;
    }
    LSInputImpl.prototype.setStringData = function(v) {
        this._stringData = v;
    }
    LSInputImpl.prototype.setSystemId = function(v) {
        this._systemId = v;
    }

    function ResolverImpl() {}
        
    ResolverImpl.prototype.resolveResource = function(type, namespaceURI, publicId, systemId, baseURI)  {
        // printDebug("RESOLVING RESOURCE (" + publicId + "," + systemId + ")");
        var lsInput = new JavaAdapter(Packages.org.w3c.dom.ls.LSInput, new LSInputImpl());
        var resolvedFilename;
        if (systemId == "http://www.w3.org/2001/xml.xsd") {
            resolvedFilename = baseDir + "lib/external/xml.xsd";
        }
        else if (systemId == "XMLSchema.dtd") {
            resolvedFilename = baseDir + "lib/external/XMLSchema.dtd";
        }
        else if (systemId == "XMLSchema.xsd") {
            resolvedFilename = baseDir + "lib/external/XMLSchema.xsd";
        }
        else if (systemId == "datatypes.dtd") {
            resolvedFilename = baseDir + "lib/external/datatypes.dtd";
        }
        else {
            resolvedFilename = buildDir + systemId;            
        }
        var file = new java.io.File(resolvedFilename);
        if (!file.exists) {
            // printDebug("FILE DOES NOT EXIST !!! ("+ resolvedFilename + ")");            
        }
        try {
            lsInput.setCharacterStream(new java.io.FileReader(resolvedFilename));
            // printDebug("CREATED FILE READER FOR ("+ resolvedFilename + ")");
            return lsInput;
        }
        catch(e) {
            throw new Error("attempt to include a non-existing descriptor: " + systemId);
        }
    }
    
    function ErrorHandlerImpl() {}
    ErrorHandlerImpl.prototype.error = function(spe) {
        print("SAX ERROR: " + spe.message);        
        spe.printStackTrace();
    }
    ErrorHandlerImpl.prototype.fatalError = function(spe) {
        print("SAX FATAL ERROR: " + spe.message);
        spe.printStackTrace();
    }
    ErrorHandlerImpl.prototype.warning = function(spe) {}
    
    function HandlerImpl() {}
    HandlerImpl.prototype.error = function(spe) {
        print("SAX ERROR: " + spe.message);
    }
    HandlerImpl.prototype.fatalError = function(spe) {
        print("SAX FATAL ERROR: " + spe.message);
    }
    HandlerImpl.prototype.resolveEntity = resolveEntityUsingBuildDirectory;
    
    var handler = new JavaAdapter(Packages.org.xml.sax.helpers.DefaultHandler, new HandlerImpl());
    var spf = Packages.javax.xml.parsers.SAXParserFactory.newInstance();
    spf.setNamespaceAware(true);
    spf.setValidating(true);
    /*
    var ssf = Packages.javax.xml.validation.SchemaFactory.newInstance("http://www.w3.org/2001/XMLSchema");
    var resolver = new JavaAdapter(Packages.org.w3c.dom.ls.LSResourceResolver, new ResolverImpl());
    ssf.setResourceResolver(resolver);
    var errorHandler = new JavaAdapter(Packages.org.xml.sax.ErrorHandler, new ErrorHandlerImpl());
    ssf.setErrorHandler(errorHandler);
    // var xsdSchema = ssf.newSchema(new java.io.File(baseDir + "lib/external/XMLSchema.xsd"));
    // var xsdSchema = ssf.newSchema(new java.io.File(schemaFilename));
    var schemas = java.lang.reflect.Array.newInstance(Packages.javax.xml.transform.Source, 2);
    schemas[0] = new Packages.javax.xml.transform.stream.StreamSource(new java.io.File(schemaFilename));
    schemas[1] = new Packages.javax.xml.transform.stream.StreamSource(new java.io.File(baseDir + "build/javaee_web_services_client_1_2.xsd"));
    var xsdSchema = ssf.newSchema(schemas);
    spf.setSchema(xsdSchema);
    */
    spf.setFeature("http://xml.org/sax/features/namespace-prefixes", true);
    var sp = spf.newSAXParser();
    sp.setProperty("http://java.sun.com/xml/jaxp/properties/schemaLanguage", "http://www.w3.org/2001/XMLSchema");
    sp.setProperty("http://java.sun.com/xml/jaxp/properties/schemaSource", baseDir + "lib/external/XMLSchema.xsd");
    var is = new Packages.org.xml.sax.InputSource(new java.io.FileReader(schemaFilename));
    sp.parse(is, handler);
}

function isWhitespace(s) {
    for (var i = 0; i < s.length(); ++i) {
        if (!java.lang.Character.isWhitespace(s.charCodeAt(i))) {
            return false;
        }
    }
    return true;
}

function containsAngleBrackets(s) {
    return s.indexOf("<") != -1 || s.indexOf(">") != -1;
}

function escapeAttributeValue(s) {
    return s.replace("&", "&#38;").replace("<", "&#60;").replace(">", "&#62;").replace('"', "&#34;");
}

function printDebug(s) {
    if (debug) {
        java.lang.System.err.println(s);
    }
}

function printWarning(s) {
    print(s);
}

function resolveEntityUsingBuildDirectory(publicId, systemId, fallbackDir) {
    // printDebug("RESOLVING RESOURCE (" + publicId + "," + systemId + ")");
    var resolvedUrl = systemId;
    var prefix_javaee = "http://java.sun.com/xml/ns/javaee/";
    var prefix_j2ee = "http://java.sun.com/xml/ns/j2ee/";
    var prefix_ibm = "http://www.ibm.com/webservices/xsd/";
    
    if (systemId.startsWith("file://") && (fallbackDir != undefined)) {
        // no-op
        var name = systemId.substring(7);
        var file = new java.io.File(name);
        if (!file.exists()) {
            name = fallbackDir + name.substring(name.lastIndexOf("/") + 1);
            file = new java.io.File(name);
            if (file.exists()) {
                resolvedUrl = "file://" + name;
            }
        }
    }
    else if (systemId == "http://www.w3.org/2001/xml.xsd") {
        resolvedUrl = "file://" + baseDir + "lib/external/xml.xsd";
    }
    else if (systemId.startsWith(prefix_javaee)) {
        resolvedUrl = "file://" + buildDir + systemId.substring(prefix_javaee.length);
    }
    else if (systemId.startsWith(prefix_j2ee)) {
        resolvedUrl = "file://" + buildDir + systemId.substring(prefix_j2ee.length);
    }
    else if (systemId.startsWith(prefix_ibm)) {
        resolvedUrl = "file://" + buildDir + systemId.substring(prefix_ibm.length);
    }
    if (resolvedUrl != systemId) {
        //printDebug("RESOLVED TO " + resolvedUrl);
    }
    if (resolvedUrl.substring(0, 5) != "file:") {
        throw new Error("failed to resolve entity to file: " + systemId);
    }
    return new Packages.org.xml.sax.InputSource(resolvedUrl);
}

function processOne(filename) {
    print("processing " + filename);
    // var fullContents = passOne(filename);
    var outputFilename = buildDir + filename.replace(".xsds", ".xsd");
    // passTwo(outputFilename, new java.io.StringReader(fullContents));
    passTwo(outputFilename, new java.io.FileReader(sourceDir + filename));
    passThree(outputFilename);
    printDebug("---");
}

function forEachDo(list, fn) {
    for (var i in list) {
        fn(list[i]);
    }    
}

function processAll(filenameList) {
    forEachDo(filenameList, processOne);
}

//////////////// testing

function extractTestInfo(filename) {
    var testInfo = {expectedErrors:[]};

    function HandlerImpl() {}
    HandlerImpl.prototype.processingInstruction = function(target, data) {
        // printDebug("PI " + target + " " + data);
        if (target == "validateAgainst") {
            if (testInfo.schema) {
                throw new Error("found more than one validateAgainst processing instruction");
            }
            data = data.trim();
            var schemaFilename = buildDir + data;
            if ((new java.io.File(schemaFilename)).isFile()) {
                testInfo.schema = schemaFilename;
            }
            else {
                throw new Error("validateAgainst processing instruction points to nonexistent schema: " + data);
            }
        }
        else if (target == "expectError") {
            data = data.trim();
            testInfo.expectedErrors.push(data);
        }
        else {
            throw new Error("unrecognized processing instruction: " + target + " " + data);
        }
    }
    
    var handler = new JavaAdapter(Packages.org.xml.sax.helpers.DefaultHandler, new HandlerImpl());
    var spf = Packages.javax.xml.parsers.SAXParserFactory.newInstance();
    spf.setNamespaceAware(true);
    spf.setFeature("http://xml.org/sax/features/namespace-prefixes", true);
    var sp = spf.newSAXParser();
    var reader = new java.io.FileReader(testDir + "/" + filename);
    try {
        var is = new Packages.org.xml.sax.InputSource(reader);
        sp.parse(is, handler);
    }
    finally {
        reader.close();
    }
    return testInfo;
}

function validate(filename, testInfo) {
    function HandlerImpl() {
        this.errorCount = 0;
        this.remainingErrors = [];
        for (var i in testInfo.expectedErrors) {
            this.remainingErrors.push(testInfo.expectedErrors[i]);
        }
    }
    HandlerImpl.prototype.error = function(spe) {
        var i = spe.message.indexOf(":");
        if (i == -1) { throw new Error("*** ERROR *** " + spe.message); }
        var errorId = spe.message.substring(0, i);
        var error = spe.getLineNumber() + ":" + errorId;
        if (this.remainingErrors.length > 0) {
            var expectedError = this.remainingErrors.shift();
            if (error != expectedError) {
                print("was expecting error: " + expectedError);                
                print("instead got the following:");
                print("sax error: " + spe.getLineNumber() + ":" + spe.message);
            }
        }
        else {
            print("sax error: " + spe.getLineNumber() + ":" + spe.message);
            this.errorCount++;
        }
    }
    HandlerImpl.prototype.fatalError = function(spe) {
        print("sax fatal error: line " + spe.getLineNumber() + ": " + spe.message);
        this.errorCount++;
    }
    HandlerImpl.prototype.resolveEntity = function(pid, sid) { return resolveEntityUsingBuildDirectory(pid, sid, testDir); }
    
    var handler = new JavaAdapter(Packages.org.xml.sax.helpers.DefaultHandler, new HandlerImpl());
    var spf = Packages.javax.xml.parsers.SAXParserFactory.newInstance();
    spf.setNamespaceAware(true);
    spf.setFeature("http://xml.org/sax/features/namespace-prefixes", true);
    spf.setValidating(true);
    var sp = spf.newSAXParser();
    sp.setProperty("http://java.sun.com/xml/jaxp/properties/schemaLanguage", "http://www.w3.org/2001/XMLSchema");
    sp.setProperty("http://java.sun.com/xml/jaxp/properties/schemaSource", testInfo.schema);
    var reader = new java.io.FileReader(testDir + "/" + filename);
    try {
        var is = new Packages.org.xml.sax.InputSource(reader);
        sp.parse(is, handler);
        if (handler.errorCount > 0) {
            print("found " + handler.errorCount + " error(s)");
        }
    }
    finally {
        reader.close();
    } 
}

function testOne(filename) {
    print("testing " + filename);
    var testInfo = extractTestInfo(filename);
    validate(filename, testInfo);
    printDebug("----");
}

function testAll(filenameList) {
    forEachDo(filenameList, testOne);
}

function findAllFilesWithExtension(directory, extension) {
    var dir = new java.io.File(directory);
    var results = [];
    if (dir.isDirectory()) {
        var files = dir.listFiles(new java.io.FilenameFilter({accept: function(d,n) { return n.endsWith(extension)}}));
        for (var i = 0; i < files.length; ++i) {
            results.push(files[i].getName());
        }
    }
    return results;
}

//////////////// main

function checkAntVersion() {
    try {
        var klass = java.lang.Class.forName("org.apache.tools.ant.taskdefs.condition.AntVersion");
        return true;
    }
    catch (e) {
        return false;
    }
}

if (!checkAntVersion()) {
    throw new Error("you need Apache Ant 1.7.0 or later to build");
}

if (phase == "generate") {
    var sourceList = findAllFilesWithExtension(sourceDir, ".xsds");
    processAll(sourceList);
}
else if (phase =="test") {
    var testList = findAllFilesWithExtension(testDir, ".xml");
    testAll(testList);
}
else {
    throw Error("invalid phase: " + phase);
}