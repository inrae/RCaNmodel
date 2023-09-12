package fr.cm.xmlFiles;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;

import fr.cm.rCaller.RCaNScript;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;

public class RCommandListXML {

    static List<RCaNScript> listOfRCaNScriptXML;
    public static void init(){
        listOfRCaNScriptXML = new ArrayList<>();

        // String fileName = "project/Project.txt";
        // InputStream inst = MetaInformation.class.getClassLoader().getResourceAsStream(fileName);

        String fileName = "rscripts/RCommands.xml";
        try {
            // URL inst = XMLCommandList.class.getClassLoader().getResource(fileName);
            InputStream inst = RCommandListXML.class.getClassLoader().getResourceAsStream(fileName);
            // File file = new File(inst.toURI());
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document doc = db.parse(new InputSource(inst));
            doc.getDocumentElement().normalize();
            NodeList nodeList = doc.getElementsByTagName("rcommand");
            for (int itr = 0; itr < nodeList.getLength(); itr++) {
                Node node = nodeList.item(itr);
                if (node.getNodeType() == Node.ELEMENT_NODE) {
                    String name = getByTag(node,"name");
                    String textmenu = getByTag(node,"textmenu");
                    String menu = getByTag(node,"menu");
                    String rcompute = getByTag(node,"rcompute");
                    String rplots = getByTag(node,"rplots");
                    String help = getByTag(node,"help");
                    String condition = getByTag(node,"condition");
                    String parameter = getByTag(node,"parameter");
                    String table = getByTag(node,"table");
                    RCaNScript rCaNScriptXML = new RCaNScript(name, textmenu, menu, rcompute,
                            rplots, help, condition,  parameter, table);
                    listOfRCaNScriptXML.add(rCaNScriptXML);
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

    static boolean getBooleanByTag(Node node, String tag) {
        if(getByTag(node, "tag").equals("true")) return(true);
        return(false);
    }

    public static RCaNScript getRCommandByMenu(String menu) {
        for(RCaNScript rCaNScriptXML : listOfRCaNScriptXML)
            if (rCaNScriptXML.getTextMenu().equals(menu)) {
                return (rCaNScriptXML);
            }
        return (null);
    }

    public static List<RCaNScript> getListOfRCommandXML() {
        return listOfRCaNScriptXML;
    }
}

