/*******************************************************************************
 * Copyright (c) 2011 Oracle. All rights reserved.
 * This program and the accompanying materials are made available under the 
 * terms of the Eclipse Public License v1.0 and Eclipse Distribution License v. 1.0 
 * which accompanies this distribution. 
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at 
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *     Mike Norman - DBWS-running-under-OSGi Proof-of-concept
 ******************************************************************************/
package simpletable;

//javase imports
import java.io.StringReader;
import java.io.StringWriter;
import org.w3c.dom.Document;

//java eXtension imports
import javax.xml.namespace.QName;
import javax.xml.soap.MessageFactory;
import javax.xml.soap.SOAPException;
import javax.xml.soap.SOAPMessage;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMResult;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.ws.Dispatch;
import javax.xml.ws.Service;
import static javax.xml.ws.soap.SOAPBinding.SOAP11HTTP_BINDING;

//EclipseLink imports
import org.eclipse.persistence.platform.xml.XMLComparer;
import org.eclipse.persistence.platform.xml.XMLParser;
import org.eclipse.persistence.platform.xml.XMLPlatform;
import org.eclipse.persistence.platform.xml.XMLPlatformFactory;

//test imports
import static simpletable.Constants.ENDPOINT_ADDRESS;
import static simpletable.Constants.TEST_NAMESPACE;
import static simpletable.Constants.TEST_PORT;
import static simpletable.Constants.TEST_SERVICE;
import static simpletable.Constants.TEST_SERVICE_NAMESPACE;

public class Client {

    static XMLComparer comparer = new XMLComparer();
    static XMLPlatform xmlPlatform = XMLPlatformFactory.getInstance().getXMLPlatform();
    static XMLParser xmlParser = xmlPlatform.newXMLParser();

    static final String SOAP_FINDBYPK_REQUEST =
        "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\">" +
          "<SOAP-ENV:Body>" +
            "<findByPrimaryKey_simpletableType xmlns=\"urn:simpletableService\">" +
              "<id>1</id>" +
            "</findByPrimaryKey_simpletableType>" +
          "</SOAP-ENV:Body>" +
        "</SOAP-ENV:Envelope>";
    static final String SOAP_FINDALL_REQUEST =
        "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\">" +
          "<SOAP-ENV:Body>" +
            "<findAll_simpletableType xmlns=\"urn:simpletableService\" xmlns:urn=\"urn:simpletable\"/>" +
          "</SOAP-ENV:Body>" +
        "</SOAP-ENV:Envelope>";
    static final String SOAP_UPDATE_REQUEST =
        "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\">" +
          "<SOAP-ENV:Body>" +
            "<update_simpletableType xmlns=\"urn:simpletableService\" xmlns:urn=\"urn:simpletable\">" +
              "<theInstance>" +
                "<urn:simpletableType>" +
                  "<urn:id>1</urn:id>" +
                  "<urn:name>mike norman</urn:name>" +
                  "<urn:since>2010-12-25</urn:since>" +
                "</urn:simpletableType>" +
              "</theInstance>" +
            "</update_simpletableType>" +
          "</SOAP-ENV:Body>" +
        "</SOAP-ENV:Envelope>";
    static final String SOAP_CREATE_REQUEST =
        "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\">" +
          "<SOAP-ENV:Body>" +
            "<create_simpletableType xmlns=\"urn:simpletableService\" xmlns:urn=\"urn:simpletable\">" +
              "<theInstance>" +
                "<urn:simpletableType>" +
                  "<urn:id>4</urn:id>" +
                  "<urn:name>test</urn:name>" +
                  "<urn:since>2009-03-27</urn:since>" +
                "</urn:simpletableType>" +
              "</theInstance>" +
            "</create_simpletableType>" +
          "</SOAP-ENV:Body>" +
        "</SOAP-ENV:Envelope>";
    static final String SOAP_DELETE_REQUEST =
        "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\">" +
          "<SOAP-ENV:Body>" +
            "<delete_simpletableType xmlns=\"urn:simpletableService\" xmlns:urn=\"urn:simpletable\">" +
              "<id>4</id>" +
            "</delete_simpletableType>" +
          "</SOAP-ENV:Body>" +
        "</SOAP-ENV:Envelope>";
    static final String SOAP_UPDATE2_REQUEST =
        "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\">" +
          "<SOAP-ENV:Body>" +
            "<update_simpletableType xmlns=\"urn:simpletableService\" xmlns:urn=\"urn:simpletable\">" +
              "<theInstance>" +
                "<urn:simpletableType>" +
                  "<urn:id>1</urn:id>" +
                  "<urn:name>mike</urn:name>" +
                  "<urn:since>2001-12-25</urn:since>" +
                "</urn:simpletableType>" +
              "</theInstance>" +
            "</update_simpletableType>" +
          "</SOAP-ENV:Body>" +
        "</SOAP-ENV:Envelope>";

    public static void main(String[] args) throws SOAPException, TransformerException {

        QName qname = new QName(TEST_SERVICE_NAMESPACE, TEST_PORT);
        Service service = Service.create(new QName(TEST_NAMESPACE, TEST_SERVICE));
        service.addPort(qname, SOAP11HTTP_BINDING, ENDPOINT_ADDRESS);
        Dispatch<SOAPMessage> sourceDispatch = service.createDispatch(qname, SOAPMessage.class,
            Service.Mode.MESSAGE);

        SOAPMessage request = createSOAPMessage(SOAP_FINDBYPK_REQUEST);
        SOAPMessage result = sourceDispatch.invoke(request);
        if (result != null) {
            Source src = result.getSOAPPart().getContent();
            DOMResult domResult = new DOMResult();
            getTransformer().transform(src, domResult);
            Document resultDoc = (Document)domResult.getNode();
            System.out.println(documentToString(resultDoc));
            Document controlDoc = xmlParser.parse(new StringReader(SOAP_FINDBYPK_RESPONSE));
            boolean nodeEqual = comparer.isNodeEqual(controlDoc, resultDoc);
            if (!nodeEqual) {
                System.out.println("findByPrimaryKey_simpletableType response not same as control document");
            }
        }

        request = createSOAPMessage(SOAP_FINDALL_REQUEST);
        result = sourceDispatch.invoke(request);
        if (result != null) {
            Source src = result.getSOAPPart().getContent();
            DOMResult domResult = new DOMResult();
            getTransformer().transform(src, domResult);
            Document resultDoc = (Document)domResult.getNode();
            Document controlDoc = xmlParser.parse(new StringReader(SOAP_FINDALL_RESPONSE));
            boolean nodeEqual = comparer.isNodeEqual(controlDoc, resultDoc);
            if (!nodeEqual) {
                System.out.println("findAll_simpletableType response not same as control document");
            }
        }

        request = createSOAPMessage(SOAP_UPDATE_REQUEST);
        result = sourceDispatch.invoke(request);
        if (result != null) {
            String localName = result.getSOAPBody().getFirstChild().getLocalName();
            if (!SOAP_UPDATE_RESPONSE_ELEMENTNAME.equals(localName)) {
                System.out.println(SOAP_UPDATE_RESPONSE_ELEMENTNAME + " incorrect");
            }
        }

        request = createSOAPMessage(SOAP_FINDBYPK_REQUEST);
        result = sourceDispatch.invoke(request);
        if (result != null) {
            Source src = result.getSOAPPart().getContent();
            DOMResult domResult = new DOMResult();
            getTransformer().transform(src, domResult);
            Document resultDoc = (Document)domResult.getNode();
            Document controlDoc = xmlParser.parse(new StringReader(SOAP_FINDBYPK_AFTERUPDATE_RESPONSE));
            boolean nodeEqual = comparer.isNodeEqual(controlDoc, resultDoc);
            if (!nodeEqual) {
                System.out.println("findByPrimaryKey_simpletableType response (after update) not same as control document");
            }
        }

        request = createSOAPMessage(SOAP_CREATE_REQUEST);
        result = sourceDispatch.invoke(request);
        if (result != null) {
            String localName = result.getSOAPBody().getFirstChild().getLocalName();
            if (!SOAP_CREATE_RESPONSE_ELEMENTNAME.equals(localName)) {
                System.out.println(SOAP_CREATE_RESPONSE_ELEMENTNAME + " incorrect");
            }
        }

        request = createSOAPMessage(SOAP_DELETE_REQUEST);
        result = sourceDispatch.invoke(request);
        if (result != null) {
            String localName = result.getSOAPBody().getFirstChild().getLocalName();
            if (!SOAP_DELETE_RESPONSE_ELEMENTNAME.equals(localName)) {
                System.out.println(SOAP_DELETE_RESPONSE_ELEMENTNAME + " incorrect");
            }
        }

        request = createSOAPMessage(SOAP_UPDATE2_REQUEST);
        result = sourceDispatch.invoke(request);
        if (result != null) {
            String localName = result.getSOAPBody().getFirstChild().getLocalName();
            if (!SOAP_UPDATE_RESPONSE_ELEMENTNAME.equals(localName)) {
                System.out.println(SOAP_UPDATE_RESPONSE_ELEMENTNAME + " incorrect");
            }
        }

    }
    static final String SOAP_FINDBYPK_RESPONSE =
        "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\">" +
            "<SOAP-ENV:Header/>" +
            "<SOAP-ENV:Body>" +
                "<srvc:findByPrimaryKey_simpletableTypeResponse xmlns=\"urn:simpletable\" xmlns:srvc=\"urn:simpletableService\">" +
                    "<srvc:result>" +
                        "<simpletableType xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
                            "<id>1</id>" +
                            "<name>mike</name>" +
                            "<since>2001-12-25</since>" +
                        "</simpletableType>" +
                    "</srvc:result>" +
                "</srvc:findByPrimaryKey_simpletableTypeResponse>" +
            "</SOAP-ENV:Body>" +
        "</SOAP-ENV:Envelope>";
    static final String SOAP_FINDALL_RESPONSE =
        "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\">" +
            "<SOAP-ENV:Header/>" +
            "<SOAP-ENV:Body>" +
                "<srvc:findAll_simpletableTypeResponse xmlns=\"urn:simpletable\" xmlns:srvc=\"urn:simpletableService\">" +
                    "<srvc:result>" +
                        "<simpletableType xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
                            "<id>1</id>" +
                            "<name>mike</name>" +
                            "<since>2001-12-25</since>" +
                        "</simpletableType>" +
                        "<simpletableType xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
                            "<id>2</id>" +
                            "<name>blaise</name>" +
                            "<since>2001-12-25</since>" +
                        "</simpletableType>" +
                        "<simpletableType xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
                            "<id>3</id>" +
                            "<name>rick</name>" +
                            "<since>2001-12-25</since>" +
                        "</simpletableType>" +
                    "</srvc:result>" +
                "</srvc:findAll_simpletableTypeResponse>" +
            "</SOAP-ENV:Body>" +
        "</SOAP-ENV:Envelope>";
    static final String SOAP_UPDATE_RESPONSE_ELEMENTNAME =
        "update_simpletableTypeResponse";
    static final String SOAP_FINDBYPK_AFTERUPDATE_RESPONSE =
        "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\">" +
            "<SOAP-ENV:Header/>" +
            "<SOAP-ENV:Body>" +
                "<srvc:findByPrimaryKey_simpletableTypeResponse xmlns=\"urn:simpletable\" xmlns:srvc=\"urn:simpletableService\">" +
                    "<srvc:result>" +
                        "<simpletableType xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">" +
                            "<id>1</id>" +
                            "<name>mike norman</name>" +
                            "<since>2010-12-25</since>" +
                        "</simpletableType>" +
                    "</srvc:result>" +
                "</srvc:findByPrimaryKey_simpletableTypeResponse>" +
            "</SOAP-ENV:Body>" +
        "</SOAP-ENV:Envelope>";
    static final String SOAP_CREATE_RESPONSE_ELEMENTNAME =
        "create_simpletableTypeResponse";
    static final String SOAP_DELETE_RESPONSE_ELEMENTNAME =
        "delete_simpletableTypeResponse";

    static SOAPMessage createSOAPMessage(String message) {
        try {
            MessageFactory factory = MessageFactory.newInstance();
            SOAPMessage soapMessage = factory.createMessage();
            soapMessage.getSOAPPart().setContent(
                (Source)new StreamSource(new StringReader(message)));
            soapMessage.saveChanges();
            return soapMessage;
        }
        catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }
    static Transformer getTransformer() {
        Transformer transformer = null;
        try {
            TransformerFactory tf = TransformerFactory.newInstance();
            transformer = tf.newTransformer();
        }
        catch (TransformerConfigurationException e) {
            /* extremely rare, safe to ignore */
        }
        return transformer;
    }
    static String documentToString(Document doc) {
        DOMSource domSource = new DOMSource(doc);
        StringWriter stringWriter = new StringWriter();
        StreamResult result = new StreamResult(stringWriter);
        try {
            Transformer transformer = TransformerFactory.newInstance().newTransformer();
            transformer.setOutputProperty("indent", "yes");
            transformer.transform(domSource, result);
            return stringWriter.toString();
        } catch (Exception e) {
            // e.printStackTrace();
            return "<empty/>";
        }
    }
}