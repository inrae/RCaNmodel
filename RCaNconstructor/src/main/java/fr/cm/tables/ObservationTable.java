/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.tables;

import fr.cm.canObjects.ListsManager;
import fr.cm.canObjects.Observation;
import fr.cm.menus.Context;
import fr.cm.dialogs.HelpDialog;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.geometry.Insets;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;

import java.util.ArrayList;
import java.util.List;

/**
 * @author christianmullon
 */
public class ObservationTable extends Pane {

    ObservableList<Observation> list;
    List<Observation> observations;

    final TableView<Observation> observationTable;
    final VBox vbox = new VBox();

    int no;
    int ny;

    double width = 0.9 * Context.getWidth();
    double height = 0.9 * Context.getHeight();

    public ObservationTable() {
        observationTable = new TableView<>();
        observationTable.setEditable(false);

        makeList();
        makeTable();
        // -----------------------------------------------------------------------------------------------
        observationTable.setPrefWidth(0.9 * width);
        observationTable.setPrefHeight(0.8 * height);

        // -----------------------------------------------------------------------------------------------

        vbox.setSpacing(20);
        vbox.setPadding(new Insets(10, 0, 0, 10));
        vbox.getChildren().addAll(observationTable);
        vbox.setLayoutX(100);
        vbox.setLayoutY(50);

        this.getChildren().addAll(vbox);
        this.setMinWidth(width);
        this.setHeight(height);
    }

    private void makeList() {
        observations = new ArrayList<>();
        no = ListsManager.getListOfObservations().size();
        int minYear = Context.getMinYear();
        ny = Context.getNbYears();
        for (int y = 0; y < ny; y++) {
            String name = "" + (minYear + y);
            double[] values = new double[no];
            for (int o = 0; o < no; o++) {
                values[o] = ListsManager.getListOfObservations().get(o).getValues(y);
            }
            Observation observation = new Observation(name, values, true);
            observations.add(observation);
        }
    }

    private void makeTable() {
        // Premiere colonne avec les annees
        TableColumn<Observation, String> name = new TableColumn<>("Name");
        name.setCellValueFactory(new PropertyValueFactory<>("obsName"));
        name.setSortable(false);
        observationTable.getColumns().add(name);
        // Autres colones avec les observations
        for (int o = 0; o < no; o++) {
            String obs = ListsManager.getListOfObservations().get(o).getObsName();
            final int oo = o;
            TableColumn<Observation, String> col = new TableColumn<>();
            Label colHeader = new Label(obs);
            colHeader.setOnMouseClicked(event -> {
                Label labs = (Label)event.getSource();
                String obsS  = labs.getText();
                Observation observationS = ListsManager.getObservationByName(obsS);
                if(observationS != null) {
                    // Alerte(String txt, String enTete, String title, String type, double w, double h) {
                    new HelpDialog(observationS.getMeta(), "Observation","Meta information", "information", "No", 400.0, 400.0);
                }
            });
            col.setGraphic(colHeader);
            col.setSortable(false);
            col.setMinWidth(100.0);
            col.setCellValueFactory(
                    param -> {
                        SimpleStringProperty sval = new SimpleStringProperty();
                        Observation observation = param.getValue();
                        double val = observation.getValues()[oo];
                        sval.set(String.format("%.2f", val));
                        return sval;
                    }
            );
            observationTable.getColumns().add(col);
        }

        list = FXCollections.observableArrayList(observations);
        observationTable.setItems(list);
    }

    //
    public void buildTable() {
        observationTable.getColumns().removeAll(observationTable.getColumns());
        this.makeList();
        this.makeTable();
    }

}
