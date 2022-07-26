/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.GUItablesViews;

import fr.cm.ProjectManager.ProjectListsManager;
import fr.cm.canObjects.Observation;
import fr.cm.RCaNMain.Context;
import fr.cm.parameters.ColorsAndFormats;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
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
// ---------------------------------

    ObservableList<Observation> observableListOfTransposeObservations;
    List<Observation> listOfTransposedObservations;
    Observation selectedObservation = null;

    final TableView<Observation> observationTable;
    final VBox vbox = new VBox();

    int no;
    int ny;

    double width = Context.getWindowWidth();
    double height = Context.getWindowHeight();

    public ObservationTable() {
        observationTable = new TableView<>();
        observationTable.setEditable(false);
        buildTable();
        // -----------------------------------------------------------------------------------------------
        final Label title = new Label("System observations");
        title.setFont(ColorsAndFormats.titleFont);
        final VBox vbox = new VBox();
        ColorsAndFormats.setVBoxCharacteristics(vbox);
        vbox.getChildren().addAll(title, observationTable);
        this.getChildren().addAll(vbox);
    }

;

    private void transposeListOfObservations() {
        listOfTransposedObservations = new ArrayList<>();
        no = ProjectListsManager.getListOfObservations().size();
        int minYear = Context.getFirstYear();
        ny = Context.getNbYears();
        for (int y = 0; y < ny; y++) {
            String name = "" + (minYear + y);
            double[] values = new double[no];
            for (int o = 0; o < no; o++) {
                values[o] = ProjectListsManager.getListOfObservations().get(o).getValues(y);
            }
            Observation transposedObservation = new Observation(name, values);
            listOfTransposedObservations.add(transposedObservation);
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
            String obs = ProjectListsManager.getListOfObservations().get(o).getObsName();
            final int oo = o;
            TableColumn<Observation, String> col = new TableColumn<>();
            Label colHeader = new Label(obs);
            colHeader.setOnMouseClicked(event -> {
                Label labs = (Label) event.getSource();
                String obsS = labs.getText();
                selectedObservation = ProjectListsManager.getObservationByName(obsS);
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
      }


    //
    public void buildTable() {
        observationTable.setPrefWidth(0.8*width);
        observationTable.setPrefHeight(0.7*height);
        observationTable.getColumns().removeAll(observationTable.getColumns());
        transposeListOfObservations();
        makeTable();
        observableListOfTransposeObservations = FXCollections.observableArrayList(listOfTransposedObservations);
        observationTable.setItems(observableListOfTransposeObservations);
    }

}
