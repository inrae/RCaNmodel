package fr.cm.rcan;

import com.github.rcaller.rstuff.ROutputParser;

import java.io.File;
import java.io.IOException;
import java.io.StringWriter;

import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import fr.cm.xml.RCommandXML;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

public class RCanCallerDecodeResult {

    static String getByTag(Node node, String tag) {
        Element eElement = (Element) node;
        Node dNode = eElement.getElementsByTagName(tag).item(0);
        if (dNode != null) {
            return (dNode.getTextContent());
        }
        return ("");
    }

    static String fmt(String st, int s){
        String b = "                                                       ";
        int l = st.length();
        return(b.substring(0,s+4-l)+st);
    }

    public static String decodeParser(RCommandXML rCommandXML, ROutputParser parser) {
        String result = "Problem in getting result";
        try {
            // String XMLResult = parser.getXMLFileAsString();
            File xmlFile = parser.getXMLFile();
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document doc = db.parse(xmlFile);
            doc.getDocumentElement().normalize();
            // xmlPrint(doc);
            if(rCommandXML.isTable()) {
                NodeList nodeList = doc.getElementsByTagName("variable");
                int nv = nodeList.getLength();
                int nit = 0;
                String[] names = new String[nv];
                String[][] valeurs = new String[nv][1000];
                for (int v = 0; v < nv; v++) {
                    for (int i = 0; i < 1000; i++) {
                        valeurs[v][i] = "-";
                    }
                }
                int[] sz = new int[3];
                for (int v = 0; v < nv; v++) {
                    sz[v] = 5;
                    Node node = nodeList.item(v);
                    if (node.getNodeType() == Node.ELEMENT_NODE) {
                        Element elem = (Element) node;
                        String nameV = node.getAttributes().getNamedItem("name").getNodeValue();
                        names[v] = nameV;
                        NodeList xmlValeurs = elem.getElementsByTagName("v");
                        int ni = Math.min(1000, xmlValeurs.getLength());
                        nit = Math.max(nit, ni);
                        for (int i = 0; i < ni; i++) {
                            String va = xmlValeurs.item(i).getTextContent();
                            sz[v] = Math.max(sz[v], va.length());
                            valeurs[v][i] = va;
                        }
                    }
                }
                StringBuilder sb = new StringBuilder("");
                for (int v = 0; v < nv; v++) {
                    sb.append(fmt(names[v], sz[v]));
                }
                sb.append("\n");
                for (int i = 0; i < nit; i++) {
                    for (int v = 0; v < nv; v++) {
                        sb.append(fmt(valeurs[v][i], sz[v]));
                    }
                    sb.append("\n");
                }
                result =  (sb.toString());
            }
            else {
                result="";
            }
        } catch (ParserConfigurationException parserConfigurationException) {
            parserConfigurationException.printStackTrace();
        } catch (IOException ioException) {
            ioException.printStackTrace();
        } catch (SAXException saxException) {
            saxException.printStackTrace();
        }  catch (Exception e) {
            e.printStackTrace();
         }
        return(result);
    }

    public static void xmlPrint(Document doc) {
        try {
            StringWriter stringWriter = new StringWriter();
            StreamResult xmlOutput = new StreamResult(stringWriter);
            TransformerFactory tf = TransformerFactory.newInstance();
            Transformer transformer = tf.newTransformer();
            transformer.setOutputProperty(OutputKeys.METHOD, "xml");
            transformer.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4");
            transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no");
            transformer.setOutputProperty(OutputKeys.INDENT, "yes");
            transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
            transformer.transform(new DOMSource(doc), xmlOutput);
            System.out.println(xmlOutput.getWriter().toString());
        }
        catch (Exception e) {
            Logger.getLogger(RCanCallerDecodeResult.class.getName()).log(Level.SEVERE, e.getMessage());
            e.printStackTrace();
        }
    }
    // ------------------------------------------------------------------------

}
