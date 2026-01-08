package fr.cm.rCaller;

import fr.cm.Main.Context;
import javafx.collections.FXCollections;
import javafx.geometry.Insets;
import javafx.scene.control.*;
import javafx.scene.layout.GridPane;
import javafx.stage.Window;

import java.util.Optional;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 * @author christianmullon
 */
public class RCaNDialogGetParametersForSampling extends Dialog<ButtonType> {

    final Window window;

    public RCaNDialogGetParametersForSampling(RCaNScript rCaNScript) {
        window = this.getDialogPane().getScene().getWindow();
        window.setOnCloseRequest(event -> window.hide());

        double width = Math.min(400.0, 0.5 * Context.getWindowWidth());
        double height =  Math.min(400.0, 0.5 * Context.getWindowHeight());

        GridPane gridPane = new GridPane();
        gridPane.setPrefSize(width,height);
        gridPane.setHgap(10);
        gridPane.setVgap(10);
        gridPane.setPadding(new Insets(10, 10, 10, 10));

        ListView cSizeSample = new ListView(FXCollections.observableArrayList(new String[]{"100", "1000","10000","100000"}));
        ListView cNChain = new ListView(FXCollections.observableArrayList(new String[]{"1", "2","3","4","5"}));
        ListView cThin = new ListView(FXCollections.observableArrayList(new String[]{"1", "5","10","20", "50"}));

        cSizeSample.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
        cNChain.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
        cThin.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);

        cSizeSample.getSelectionModel().select(1);
        cNChain.getSelectionModel().select(1);
        cThin.getSelectionModel().select(4);

        gridPane.add(new Label("Size sample.txt"), 0, 0);
        gridPane.add(new Label("Chains"), 1, 0);
        gridPane.add(new Label("Thin"), 2, 0);
        gridPane.add(cSizeSample, 0, 1);
        gridPane.add(cNChain, 1, 1);
        gridPane.add(cThin, 2, 1);

        this.getDialogPane().setContent(gridPane);
        ButtonType buttonTypeOk = new ButtonType("OK", ButtonBar.ButtonData.OK_DONE);
        ButtonType buttonTypeCancel = new ButtonType("Cancel", ButtonBar.ButtonData.OK_DONE);
        this.getDialogPane().getButtonTypes().add(buttonTypeOk);
        this.getDialogPane().getButtonTypes().add(buttonTypeCancel);

        Optional<ButtonType> result = this.showAndWait();
        if (result.isPresent()) {
            if (result.get() == buttonTypeOk) {
                rCaNScript.setSamplingParameters(
                        (String)cSizeSample.getSelectionModel().getSelectedItem(),
                        (String)cNChain.getSelectionModel().getSelectedItem(),
                        (String)cThin.getSelectionModel().getSelectedItem()
                );
            }
        }
    }
}
