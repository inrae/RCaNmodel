package fr.cm.xmlFiles;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

public class HelpListXML {

    static List<HelpXML> listOfHelpXML;

    public static void init(){
        listOfHelpXML = new ArrayList<>();
        String fileName = "Helps.xml";
        try {
            InputStream inst = HelpListXML.class.getClassLoader().getResourceAsStream(fileName);
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document doc = db.parse(new InputSource(inst));
            doc.getDocumentElement().normalize();
            NodeList nodeList = doc.getElementsByTagName("scripts");
            for (int itr = 0; itr < nodeList.getLength(); itr++) {
                Node node = nodeList.item(itr);
                if (node.getNodeType() == Node.ELEMENT_NODE) {
                    String id = getByTag(node,"id");
                    String textmenu = getByTag(node,"menu");
                    String submenu = getByTag(node,"submenu");
                    String text = getByTag(node,"text");
                    String image = getByTag(node,"image");
                    HelpXML helPXML = new HelpXML(id, textmenu, submenu, text, image);
                    listOfHelpXML.add(helPXML);
                }
            }
        }
        catch (Exception e) {
            e.printStackTrace();
        }
    }

    static String getByTag(Node node, String tag) {
        Element eElement = (Element) node;
        Node dNode = eElement.getElementsByTagName(tag).item(0);
        if (dNode != null) {
            return (dNode.getTextContent());
        }
        return ("");
    }

    public static HelpXML getHelpByTextMenu(String menu) {
        for(HelpXML HelpXML : listOfHelpXML)
            if (HelpXML.getTextMenu().equals(menu)) {
                return (HelpXML);
            }
        return (null);
    }
    public static List<HelpXML> getListOfHelpXML() {
        return listOfHelpXML;
    }
}

