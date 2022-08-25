package fr.cm.preferences;

import javafx.geometry.Insets;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;

public class ColorsAndFormats {
// ---------------------------------

    public static final String font = "-fx-font: normal 16px 'sans-serif' ";

    public static final Color background = Color.rgb(135,206,250);
    
    public static final Color inside = Color.rgb(30,144,255
    );

    public static final Color componentInside = Color.rgb(127,255,0
    );

    public static final Color componentOutside = Color.rgb(0,139,139);

    public static final Color linkTrophic = Color.rgb(238,232,170);

    public static final Color linkNonTrophic = Color.rgb(218,165,32);

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
