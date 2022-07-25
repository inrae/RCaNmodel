package fr.cm.parameters;

import javafx.geometry.Insets;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;

public class ColorsAndFormats {

    public static final String font = "-fx-font: normal 16px 'sans-serif' ";

    public static final Color background = Color.rgb(100,200,200);
    
    public static final Color inside = Color.rgb(100,150,202);

    public static final Color componentInside = Color.rgb(150,250,150);

    public static final Color componentOutside = Color.rgb(250,155,155);

    public static final Color linkTrophic = Color.rgb(220,180,18);

    public static final Color linkNonTrophic = Color.rgb(180,220,18);

    public static final Color names = Color.DARKBLUE;

    public static final Font titleFont = Font.font("Verdana", FontWeight.BOLD, 16);

    public static void setVBoxCharacteristics(VBox vbox){
        vbox.setSpacing(10);
        vbox.setPadding(new Insets(10, 10, 10, 10));
        vbox.setLayoutX(100);
        vbox.setLayoutY(50);
    }

    public static void setHBoxCharacteristics(HBox hbox){
        hbox.setSpacing(10);
        hbox.setPadding(new Insets(10, 10, 10, 10));
        hbox.setLayoutX(100);
        hbox.setLayoutY(50);
    }

    public static void setGridCharacteristics(GridPane grid){
        grid.setHgap(10);
        grid.setVgap(10);
        grid.setPadding(new Insets(10, 10, 10, 10));
     }
}
