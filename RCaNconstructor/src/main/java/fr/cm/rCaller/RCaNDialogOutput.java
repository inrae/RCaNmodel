package fr.cm.rCaller;

import fr.cm.Main.Context;
import fr.cm.Main.MainApplication;
import fr.cm.xmlFiles.RCommandXML;
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
import javafx.stage.FileChooser;
import javafx.stage.Window;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

public class RCaNDialogOutput extends Dialog {

    boolean typeOutput = true;
    public RCaNDialogOutput(RCommandXML rCommandXML, String txt){
        double width = Math.min(800.0, 0.8 * Context.getWindowWidth());
        double height =  Math.min(500.0, 0.8 * Context.getWindowHeight());

        // --------------------------------------
        final Window window = getDialogPane().getScene().getWindow();
        window.setOnCloseRequest(event -> window.hide());
        setTitle(rCommandXML.getTextMenu());

        HBox  hbox = RCaNBox(txt);

        getDialogPane().setMinSize(width, height);
        getDialogPane().setContent(hbox);
        getDialogPane().getButtonTypes().add(ButtonType.CLOSE);
        this.showAndWait();

    }
    public RCaNDialogOutput(RCommandXML rCommandXML, Image imageR){
        double width = Math.min(800.0, 0.8 * Context.getWindowWidth());
        double height =  Math.min(500.0, 0.8 * Context.getWindowHeight());

        // --------------------------------------
        final Window window = getDialogPane().getScene().getWindow();
        window.setOnCloseRequest(event -> window.hide());
        setTitle(rCommandXML.getTextMenu());

        HBox  hbox = RCaNBox(imageR);
        getDialogPane().setMinSize(width, height);
        getDialogPane().setContent(hbox);
        getDialogPane().getButtonTypes().add(ButtonType.CLOSE);

        this.showAndWait();
    }
    static File filePlot;
    // ------------------------------------------------------------------------
    static HBox RCaNBox(String txt){
        HBox hbox = new HBox();
        Label outputR = new Label(txt);
        outputR.setTextFill(Color.BLACK);
        outputR.setFont(new Font("Courier", 20));
        ScrollPane scrollOutputR;
        scrollOutputR = new ScrollPane(outputR);
        double cw = 0.9 * Context.getWindowWidth();
        double ch = 0.9 * Context.getWindowHeight();
        scrollOutputR.setPrefSize(cw, ch);
        hbox.setAlignment(Pos.CENTER);
        hbox.getChildren().add(scrollOutputR);
        return(hbox);
    }

    // ------------------------------------------------------------------------
    static HBox RCaNBox(Image imageR)  {
            RCaNDialogOutput.filePlot = filePlot;
            ImageView viewR = new ImageView();
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
            HBox hbox = new HBox();
            Button buttonSave = new Button("Save Image");
            buttonSave.setOnAction(buttonSaveListener);
            VBox vBox = new VBox();
            vBox.getChildren().addAll(viewR, buttonSave);
            hbox.setAlignment(Pos.CENTER);
            hbox.getChildren().add(vBox);
            return (hbox);
     }
    // ------------------------------------------------------------------------
    static final EventHandler<ActionEvent> buttonSaveListener = e -> handle(e);
    private static void handle(ActionEvent e) {
        FileChooser fileChooser = new FileChooser();
        fileChooser.setTitle("Name of file : ");
        fileChooser.setInitialDirectory(new File(Context.getDirName()));
        String nextImageName = fileChooser.showSaveDialog(MainApplication.stage).getAbsolutePath();
        if (nextImageName != null) {
            if(nextImageName.length()>0) {
                if (!nextImageName.contains(".png")) {
                    nextImageName = nextImageName + ".png";
                }
                try {
                    BufferedImage bufferedImage = ImageIO.read(filePlot);
                    ImageIO.write(bufferedImage, "png", new File(nextImageName));
                    System.out.println(" saved " + nextImageName);
                } catch (IOException ioException) {
                    System.out.println(" pb saving file " + nextImageName);
                    ioException.printStackTrace();
                }

            }
        }
    }

// ------------------------------------------------------------------------

}
