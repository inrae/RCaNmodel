/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.dialogs;

import fr.cm.xml.HelpXML;
import fr.cm.xml.RCommandXML;
import javafx.scene.control.Alert;
import javafx.scene.control.TextArea;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import org.apache.commons.io.IOUtils;

import java.io.*;
import java.nio.charset.StandardCharsets;


/**
 * @author christianmullon
 */
public class HelpDialog extends Alert{

    public HelpDialog(String content, String enTete, String title, String type, String image, double w, double h) {
        super(Alert.AlertType.INFORMATION);
        setAlerte(content,  enTete,  title,  type,  w,  h);
    }

    public HelpDialog(HelpXML helpXMl) {
        super(Alert.AlertType.INFORMATION);
        String content = helpXMl.getText();
        String enTete = helpXMl.getTextMenu();
        String title = helpXMl.getTextMenu();
        String type = "Information";
        String image = helpXMl.getImage();
        setAlerte(content,  enTete,  title,  type,  image,  600, 400);
    }

    public HelpDialog(RCommandXML rCommandXMl) {
        super(Alert.AlertType.INFORMATION);
        String content = rCommandXMl.getHelp();
        String enTete = rCommandXMl.getTextMenu();
        String title = rCommandXMl.getTextMenu();
        String type = "Information";
        String image = "No";
        setAlerte(content,  enTete,  title,  type,  image,  600, 400);
    }

    private  void setAlerte(String content, String enTete, String title, String type, String image, double w, double h) {
        switch (type){
            case "information" :
                break;
            case "wait" :
                break;
            case "warning" :
                setAlertType(AlertType.WARNING);
                break;
        }
        setTitle(title);
        setHeaderText(" ");

        boolean withImage = false;
        ImageView imageView = new ImageView();
        try {
            Image imageHelp = new Image("/img/" + image);
            double iw = imageHelp.getWidth();
            double ih = imageHelp.getHeight();
            double ratio = Math.min(500/ih, 500/iw);
            imageView.setImage(imageHelp);
            imageView.setFitHeight(ratio * ih);
            imageView.setFitWidth(ratio * iw);

            setGraphic(imageView);
            withImage = true;
        }
        catch(Exception e){ }
        TextArea area = new TextArea(content);
        area.setWrapText(true);
        area.setEditable(false);
        getDialogPane().setContent(area);
        if(withImage){
            setResizable(true);
            getDialogPane().setPrefSize(w, 2 * h);
        }
        else {
            setResizable(true);
            getDialogPane().setPrefSize(w, h);
        }

        if(type.contains("wait")){
            showAndWait();
        }
        else{
            showAndWait();
        }
    }

    private  void setAlerte(String content, String enTete, String title, String type, double w, double h) {
        switch (type){
            case "information" :
                break;
            case "wait" :
                break;
            case "warning" :
                setAlertType(AlertType.WARNING);
                break;
        }
        setTitle(title);
        setHeaderText(enTete);
        TextArea area = new TextArea(content);
        area.setWrapText(true);
        area.setEditable(false);
        getDialogPane().setContent(area);
        setResizable(true);
        getDialogPane().setPrefSize(w, h);
        if(type.contains("wait")){
            showAndWait();
        }
        else{
            showAndWait();
        }
    }

    public static void information(String contents, String enTete) {
        new HelpDialog( contents, enTete, "Information", "information", "no", 700.0, 900.0);
    }

    public static void warning(String fileTxt, String enTete) {
        new HelpDialog("warning", fileTxt, enTete, "warning", "no", 400.0, 400.0);
    }

    private String importContents(String dir, String variable) {
        String fileName = dir + "/" + variable + ".txt";
        InputStream inst = getClass().getClassLoader().getResourceAsStream(fileName);
        String contents = "Missing comment";
        try {
            contents = IOUtils.toString(inst, StandardCharsets.UTF_8.name());
        } catch (IOException e) {
            e.printStackTrace();
        }
        return (contents);
    }

}

