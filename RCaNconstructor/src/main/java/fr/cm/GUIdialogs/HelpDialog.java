/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.GUIdialogs;

import fr.cm.RCaNMain.Context;
import fr.cm.parameters.ColorsAndFormats;

import fr.cm.xmlFiles.HelpXML;
import fr.cm.xmlFiles.RCommandXML;
import javafx.scene.control.Alert;
import javafx.scene.control.TextArea;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;

import java.io.*;
import java.nio.charset.StandardCharsets;


/**
 * @author christianmullon
 */
public class HelpDialog extends Alert {

    // --------------------------------------------
    public HelpDialog(String content,
                      String enTete,
                      String title, String type,
                      double w,
                      double h) {
        super(Alert.AlertType.INFORMATION);
        this.getDialogPane().setStyle(ColorsAndFormats.font);
        setAlerte(content, enTete, title, type, w, h);
        this.showAndWait();
    }

    // --------------------------------------------
    public HelpDialog(HelpXML helpXMl) {
        super(Alert.AlertType.INFORMATION);
        String content = helpXMl.getText();
        String enTete = helpXMl.getTextMenu();
        String title = helpXMl.getTextMenu();
        String type = "Information";
        String image = helpXMl.getImage();
        this.getDialogPane().setStyle(ColorsAndFormats.font);
        setAlerte(content, enTete, title, type, 600, 400);
        setAlerteImage(image, 600, 400);
        this.showAndWait();
    }

    // --------------------------------------------
    public HelpDialog(RCommandXML rCommandXMl) {
        super(Alert.AlertType.INFORMATION);
        String content = rCommandXMl.getHelp();
        String enTete = rCommandXMl.getTextMenu();
        String title = rCommandXMl.getTextMenu();
        String type = "Information";
        String image = "No";
        this.getDialogPane().setStyle(ColorsAndFormats.font);
        setAlerte(content, enTete, title, type, 600, 400);
        setAlerteImage(image, 600, 400);
        this.showAndWait();
    }

    // --------------------------------------------
    private void setAlerteImage(String image, double w, double h) {

        boolean withImage = false;
        ImageView imageView = new ImageView();
        try {
            Image imageHelp = new Image("/img/" + image);
            double iw = imageHelp.getWidth();
            double ih = imageHelp.getHeight();
            double ratio = Math.min(500 / ih, 500 / iw);
            imageView.setImage(imageHelp);
            imageView.setFitHeight(ratio * ih);
            imageView.setFitWidth(ratio * iw);
            setGraphic(imageView);
            withImage = true;
        } catch (Exception e) {
        }
        if (withImage) {
            setResizable(true);
            getDialogPane().setPrefSize(w, 2 * h);
        } else {
            setResizable(true);
            getDialogPane().setPrefSize(w, h);
        }
    }

    // --------------------------------------------
    private void setAlerte(String content, String enTete, String title, String type, double w, double h) {
        switch (type) {
            case "information":
                setAlertType(AlertType.INFORMATION);
                break;
            case "wait":
                break;
            case "warning":
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
    }

    // --------------------------------------------
    public static void information(String contents, String enTete) {
        new HelpDialog(contents, enTete, "Information", "information", 700.0, 900.0);
    }

    // --------------------------------------------
    public static void warning(String contents, String enTete) {
        if(Context.isWarnings()){
            new HelpDialog(contents, enTete, "Information", "information", 600.0, 400.0);
        }
    }

    public static void warning(String debut, String enTete, Exception ex) {
        String errorCode = "No error";
        if(ex !=null)  {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        ex.printStackTrace(pw);
            errorCode = sw.toString();
        }
        String contents = debut + "\n" + errorCode;
        new HelpDialog(contents, enTete, "Information","information",600.0,400.0);
    }
    // --------------------------------------------
    // --------------------------------------------


}

