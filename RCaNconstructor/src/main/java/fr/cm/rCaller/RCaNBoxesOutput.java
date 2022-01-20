package fr.cm.rCaller;

import fr.cm.RCaNMain.Context;
import fr.cm.RCaNMain.MainApplication;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.stage.FileChooser;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

public class RCaNBoxesOutput {

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
            RCaNBoxesOutput.filePlot = filePlot;
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
        fileChooser.setInitialDirectory(Context.getWorkingDirectory());
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
