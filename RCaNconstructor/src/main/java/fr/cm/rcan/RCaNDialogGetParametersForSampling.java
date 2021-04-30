package fr.cm.rcan;

import fr.cm.menus.Context;
import fr.cm.xml.RCommandXML;
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

    public RCaNDialogGetParametersForSampling(RCommandXML rCommandXML) {
        window = this.getDialogPane().getScene().getWindow();
        window.setOnCloseRequest(event -> window.hide());

        GridPane gridPane = new GridPane();
        gridPane.setPrefSize(0.8 * Context.getWidth(),0.8 * Context.getHeight());
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
                RCaNCommon.setParameterSizeSample((String)cSizeSample.getSelectionModel().getSelectedItem());
                RCaNCommon.setParameterNChain((String)cNChain.getSelectionModel().getSelectedItem());
                RCaNCommon.setParameterThin((String)cThin.getSelectionModel().getSelectedItem());
            }
        }
    }
}
