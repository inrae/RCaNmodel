package fr.cm.rCaller;

import fr.cm.Main.Context;
import fr.cm.Main.Logg;
import fr.cm.Main.MainApplication;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Pos;
import javafx.scene.control.*;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.FontPosture;
import javafx.scene.text.FontWeight;
import javafx.stage.FileChooser;
import javafx.stage.Window;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.*;
import java.nio.file.Files;
import java.nio.file.StandardOpenOption;

public class RCaNDialogOutput extends Dialog {

    static File filePlot;
    static String output;
    boolean typeOutput = true;
    public RCaNDialogOutput(RCaNScript rCaNScriptXML, String output){
        this.output = output;
        double width = Math.min(800.0, 0.6 * Context.getWindowWidth());
        double height =  Math.min(500.0, 0.6 * Context.getWindowHeight());
        // --------------------------------------
        final Window window = getDialogPane().getScene().getWindow();
        window.setOnCloseRequest(event -> window.hide());
        setTitle(rCaNScriptXML.getTextMenu());
        VBox  vbox = RCaNBox(output);
        getDialogPane().setMinSize(width, height);
        getDialogPane().setContent(vbox);
        getDialogPane().getButtonTypes().add(ButtonType.CLOSE);
        this.showAndWait();
    }


    public RCaNDialogOutput(RCaNScript rCaNScriptXML, File filePlot){
        this.filePlot = filePlot;
        double width = Math.min(800.0, 0.8 * Context.getWindowWidth());
        double height =  Math.min(500.0, 0.8 * Context.getWindowHeight());
        // --------------------------------------
        final Window window = getDialogPane().getScene().getWindow();
        window.setOnCloseRequest(event -> window.hide());
        setTitle(rCaNScriptXML.getTextMenu());
        VBox  vbox = RCaNBox(filePlot);
        getDialogPane().setMinSize(width, height);
        getDialogPane().setContent(vbox);
        getDialogPane().getButtonTypes().add(ButtonType.CLOSE);
        this.showAndWait();
    }
    // ------------------------------------------------------------------------
    static VBox RCaNBox(String txt){
        Label outputR = new Label(txt);
        outputR.setFont(Font.font("Monospaced", FontWeight.BOLD, FontPosture.REGULAR, 20));
        ScrollPane scrollOutputR;
        scrollOutputR = new ScrollPane(outputR);
        double cw = 0.5 * Context.getWindowWidth();
        double ch = 0.5 * Context.getWindowHeight();
        scrollOutputR.setPrefSize(cw, ch);

        Button buttonSave = new Button("Save Result");
        buttonSave.setOnAction(buttonSaveText);
        VBox vBox = new VBox();
        vBox.getChildren().addAll(scrollOutputR, buttonSave);
        return (vBox);
    }


    // ------------------------------------------------------------------------
    static VBox RCaNBox(File filePlot)  {
        VBox vBox = new VBox();
        ImageView viewR = new ImageView();
        try {
            FileInputStream inputstream = new FileInputStream(filePlot);
            Image imageR = new Image(inputstream);
            double iw = imageR.getWidth();
            double ih = imageR.getHeight();
            double cw = 0.9 * Context.getWindowWidth();
            double ch = 0.9 * Context.getWindowHeight();
            double rw = cw;
            double rh = cw * ih / iw;
            if (ch * iw < cw * ih) {
                rh = ch;
                rw = ch * iw / ih;
            }
            viewR.setImage(imageR);
            viewR.setFitHeight(rh);
            viewR.setFitWidth(rw);
            Button buttonSave = new Button("Save Image");
            buttonSave.setOnAction(buttonSaveImage);
            vBox.getChildren().addAll(viewR, buttonSave);
        } catch (FileNotFoundException e) {
            throw new RuntimeException(e);
        }
        return (vBox);
     }
    // ------------------------------------------------------------------------
    static final EventHandler<ActionEvent> buttonSaveImage = e -> handleImage(e);
    private static void handleImage(ActionEvent e) {
        FileChooser fileChooser = new FileChooser();
        fileChooser.setTitle("Name of file : ");
        fileChooser.setInitialDirectory(new File(Context.getDirName()));
        String fileName = fileChooser.showSaveDialog(MainApplication.stage).getAbsolutePath();
        if (fileName != null) {
            if(fileName.length()>0) {
                if (!fileName.contains(".png")) {
                    fileName = fileName + ".png";
                }
                try {
                    BufferedImage bufferedImage = ImageIO.read(filePlot);
                    ImageIO.write(bufferedImage, "png", new File(fileName));
                    Logg.addLog("Saved " + fileName);
                } catch (IOException ioException) {
                    Logg.addLog("Issue saving file " + fileName);
                    ioException.printStackTrace();
                }
            }
        }
    }

    static final EventHandler<ActionEvent> buttonSaveText = e -> handleText(e);
    private static void handleText(ActionEvent e) {
        FileChooser fileChooser = new FileChooser();
        fileChooser.setTitle("Name of file : ");
        fileChooser.setInitialDirectory(new File(Context.getDirName()));
        String fileName = fileChooser.showSaveDialog(MainApplication.stage).getAbsolutePath();
        if (fileName != null) {
            if(fileName.length()>0) {
                if (!fileName.contains(".txt")) {
                    fileName = fileName + ".txt";
                }
                try {
                    File file = new File(fileName);
                    FileWriter writer = new FileWriter(file);
                    writer.write(output);
                    writer.flush();
                    writer.close();
                    Logg.addLog("Saved " + fileName);
                } catch (IOException ioException) {
                    Logg.addLog("Issue saving file " + fileName);
                    ioException.printStackTrace();
                }
            }
        }
    }

// ------------------------------------------------------------------------

}
