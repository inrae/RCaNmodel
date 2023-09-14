package fr.cm.xmlFiles;

import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.DocumentBuilder;

import fr.cm.Main.MainApplication;
import fr.cm.rCaller.RCaNScript;
import javafx.stage.FileChooser;
import org.w3c.dom.Document;
import org.w3c.dom.NodeList;
import org.w3c.dom.Node;
import org.w3c.dom.Element;
import org.xml.sax.InputSource;

public class RCommandListXML {

    static List<RCaNScript> listOfRCaNScripts;
    public static void init(){
        listOfRCaNScripts = new ArrayList<>();

        String fileName = "scripts/RCommands.xml";
        StringBuilder sb = new StringBuilder("Scripts R");
        try {
            InputStream inst = RCommandListXML.class.getClassLoader().getResourceAsStream(fileName);
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            DocumentBuilder db = dbf.newDocumentBuilder();
            Document doc = db.parse(new InputSource(inst));
            doc.getDocumentElement().normalize();
            NodeList nodeList = doc.getElementsByTagName("rcommand");
            for (int itr = 0; itr < nodeList.getLength(); itr++) {
                Node node = nodeList.item(itr);
                if (node.getNodeType() == Node.ELEMENT_NODE) {
                    String name = getByTag(node,"name");
                    sb.append(name.trim()+ " \n");
                    String textmenu = getByTag(node,"textmenu");
                    String menu = getByTag(node,"menu");
                    String rcompute = getByTag(node,"rcompute");
                    String rplots = getByTag(node,"rplots");
                    String help = getByTag(node, "scripts");
                    String condition = getByTag(node,"condition");
                    String parameter = getByTag(node,"parameter");
                    String table = getByTag(node,"table");
                    RCaNScript rCaNScriptXML = new RCaNScript(name, textmenu, menu, rcompute,
                            rplots, help, condition,  parameter, table);
                    listOfRCaNScripts.add(rCaNScriptXML);
                }
            }
         }
        catch (Exception e) {
            sb.append("Erreur dans la lecture des ressource \n");
            sb.append(e.getStackTrace().toString());
            e.printStackTrace();
        }
        // saveCommands(sb.toString());
    }

    static void saveCommands(String commands){
        try {
            FileChooser fileChooser = new FileChooser();
            fileChooser.setTitle("Name of log file : ");
            String fileName = fileChooser.showSaveDialog(MainApplication.stage).getAbsolutePath();
            FileWriter fileWriter = new FileWriter(fileName);
            fileWriter.write(commands);
            fileWriter.close();
        } catch (IOException e) {
            System.out.println("An error occurred.");
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
        for(RCaNScript rCaNScriptXML : listOfRCaNScripts)
            if (rCaNScriptXML.getTextMenu().equals(menu)) {
                return (rCaNScriptXML);
            }
        return (null);
    }

    public static List<RCaNScript> getListOfRCommandXML() {

        return listOfRCaNScripts;
    }
}

