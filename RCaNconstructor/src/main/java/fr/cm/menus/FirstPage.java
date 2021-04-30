package fr.cm.menus;

import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.Pane;


public class FirstPage extends Pane {
    public FirstPage(){

        ImageView imageView = new ImageView();

        double he = Context.getHeight();
        double wi = Context.getWidth();
        try {
            Image image = new Image("/img/CaN2020.png");
            double iw = image.getWidth();
            double ih = image.getHeight();
            double ratio = Math.min(he/ih, wi/iw);
            imageView.setImage(image);
            imageView.setFitHeight(ratio * ih);
            imageView.setFitWidth(ratio * iw);
            this.getChildren().addAll(imageView);
        }
         catch(Exception e){
            System.out.println(e.toString());
        }

    }
}
