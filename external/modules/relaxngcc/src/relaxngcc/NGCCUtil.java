/*
 * NGCCUtil.java
 *
 * Created on 2001/08/11, 10:32
 */

package relaxngcc;
import java.io.File;

public class NGCCUtil
{
    public static String XSDTypeToJavaType(String xsd) {
        if(xsd.equals("float")) return "Float";
        if(xsd.equals("double")) return "Double";
        if(xsd.equals("boolean")) return "Boolean";
        if(xsd.equals("byte")) return "Byte";
        if(xsd.equals("short")) return "Short";
        if(xsd.equals("int")) return "Integer";
        if(xsd.equals("long")) return "Long";
        if(xsd.equals("unsignedByte")) return "Short";
        if(xsd.equals("unsignedShort")) return "Integer";
        if(xsd.equals("unsignedInt")) return "Long";
        if(xsd.equals("unsignedLong")) return "BigInteger";
        if(xsd.equals("integer") || xsd.endsWith("Integer")) return "BigInteger";
        if(xsd.equals("base64Binary") || xsd.equals("hexBinary")) return "byte[]";
        if(xsd.equals("date") || xsd.equals("time")
           || xsd.equals("dateTime") || xsd.equals("gYear") || xsd.equals("gYearMonth")
           || xsd.equals("gMonth") || xsd.equals("gMonthDay") || xsd.equals("gDay")) return "GregorianCalendar";
        return "String"; //if none of previous types match
    }
}
