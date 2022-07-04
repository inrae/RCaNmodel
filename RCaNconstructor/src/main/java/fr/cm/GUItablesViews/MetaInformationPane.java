/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.GUItablesViews;

import fr.cm.canObjects.MetaInformation;
import fr.cm.canObjects.MetaElement;

import fr.cm.RCaNMain.Context;
import fr.cm.canObjects.ProjectListsManager;
import javafx.geometry.Insets;
import javafx.scene.control.*;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;

import java.util.List;

/**
 *
 * @author christianmullon
 */
public class MetaInformationPane extends Pane {

    double width = Context.getWindowWidth();
    double height =  Context.getWindowHeight();
    final TextArea[] metaTextArea;

    public MetaInformationPane() {

        ScrollPane scrollPane = new ScrollPane();

        MetaInformation metaInformation = ProjectListsManager.getMetaInformation();
        List<MetaElement> elements = metaInformation.getElements();
        int nc = elements.size();
        metaTextArea = new TextArea[nc];
        VBox vbox = new VBox();
        vbox.setSpacing(10);
        vbox.setPadding(new Insets(10, 10, 10, 10));
        for (MetaElement metaElement : elements) {
            HBox hbox = new HBox();
            hbox.setSpacing(10);
            hbox.setPadding(new Insets(10, 10, 10, 10));
            Label metaName = new Label(metaElement.getMetaName());
            TextArea metaInput = new TextArea(metaElement.getMetaContent());
            Label metaHint = new Label(metaElement.getMetaHint());

            metaName.setPrefWidth(0.15*width);
            metaInput.setPrefWidth(0.35*width);
            metaHint.setPrefWidth(0.2*width);
            metaInput.setMinHeight(150);

            metaName.setWrapText(true);
            hbox.getChildren().add(metaName);
            if (metaElement.isMetaType()) {
                metaInput.setWrapText(true);
                metaInput.textProperty().addListener((observable, oldValue, newValue) -> save(metaElement, newValue));
                hbox.getChildren().add(metaInput);
                metaHint.setWrapText(true);
                hbox.getChildren().add(metaHint);
            }
            vbox.getChildren().add(hbox);
        }

        // Button confirm = new Button("Confirm changes");

        scrollPane.setContent(vbox);
        scrollPane.setFitToHeight(true);
        scrollPane.setFitToWidth(true);
        scrollPane.setPrefViewportWidth(0.8*width);
        scrollPane.setPrefViewportHeight(0.8*height);

        VBox wbox = new VBox();
        wbox.setSpacing(10);
        wbox.setPadding(new Insets(10, 10, 10, 10));
        wbox.getChildren().addAll(scrollPane);
        wbox.setLayoutX(100);
        wbox.setLayoutY(50);

        this.setPrefSize(width, height);
        this.getChildren().addAll(wbox);
    }

    private void save(MetaElement metaElement, String newValue){
        metaElement.setMetaContent(newValue);
    }

}
