/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.GUItablesViews;

import fr.cm.canObjects.Flux;
import fr.cm.ProjectManager.ProjectListsManager;
import fr.cm.RCaNMain.Context;
import fr.cm.parameters.ColorsAndFormats;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.scene.control.*;
import javafx.scene.control.cell.CheckBoxTableCell;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;

/**
 *
 * @author christianmullon
 */
public class FluxTable extends Pane {
// ---------------------------------

    final TableView<Flux> table;
    ObservableList<Flux> list;

    double width = Context.getWindowWidth();
    double height =  Context.getWindowHeight();

    public FluxTable() {
        table = new TableView<>();
        table.setMinWidth(800.0);
        table.setEditable(true);

        TableColumn<Flux, String> name = new TableColumn<>("Name");
        name.setCellValueFactory(new PropertyValueFactory<>("name"));
        name.setSortable(true);
        name.setMinWidth(300.0);

        TableColumn<Flux, Boolean> trophicCol = new TableColumn<>("Trophic link");
        trophicCol.setCellValueFactory(FluxTable::call);
        trophicCol.setCellFactory(p -> new CheckBoxTableCell<>());

        TableColumn<Flux, String> in = new TableColumn<>("From");
        in.setCellValueFactory(new PropertyValueFactory<>("inName"));
        in.setSortable(true);
        in.setMinWidth(200.0);

        TableColumn<Flux, String> out = new TableColumn<>("To");
        out.setCellValueFactory(new PropertyValueFactory<>("outName"));
        out.setSortable(true);
        out.setMinWidth(200.0);

        table.getColumns().add(name);
        table.getColumns().add(trophicCol);
        table.getColumns().add(in);
        table.getColumns().add(out);

        list = FXCollections.observableArrayList(ProjectListsManager.getListOfFluxes());
        table.setItems(list);
        table.getSelectionModel().selectFirst();
        table.setPrefWidth(0.8*width);
        table.setPrefHeight(0.7*height);


        final Button deleteG = new Button("Delete");
        deleteG.setOnAction((ActionEvent e) -> {
            if (table.getSelectionModel().getSelectedItem() != null) {
                Flux flux = table.getSelectionModel().getSelectedItem();
                ProjectListsManager.removeLink(flux);
                list.removeAll(flux);
            }
        });

        final Button buttonUp = new Button("Up");
        buttonUp.setOnAction((ActionEvent e) -> {
            Flux flux = table.getSelectionModel().getSelectedItem();
            ProjectListsManager.upLink(flux);
            updateTable();
        });

        final Button buttonDown = new Button("Down");
        buttonDown.setOnAction((ActionEvent e) -> {
            Flux flux = table.getSelectionModel().getSelectedItem();
            ProjectListsManager.downLink(flux);
            updateTable();
        });

        final Label how = new Label("Change trophic type with a clic on cell");
        final HBox hBoxButton = new HBox();
        hBoxButton.getChildren().addAll(buttonUp, buttonDown, deleteG, how);
        hBoxButton.setSpacing(80.0);
        // hbox.setMinSize(500.0, 120.0);


        final Label title = new Label("System fluxes");
        title.setFont(ColorsAndFormats.titleFont);
        final VBox vbox = new VBox();
        ColorsAndFormats.setVBoxCharacteristics(vbox);
        vbox.getChildren().addAll(title, table, hBoxButton);

        this.getChildren().addAll(vbox);
     }

    private static ObservableValue<Boolean> call(TableColumn.CellDataFeatures<Flux, Boolean> param) {
        Flux flux = param.getValue();
        SimpleBooleanProperty booleanProp = new SimpleBooleanProperty(flux.isTypeTrophic());
        booleanProp.addListener((observable, oldValue, newValue) -> flux.changeTypeTrophic(newValue));
        return booleanProp;
    }

    public void updateTable() {
        Flux flux = table.getSelectionModel().getSelectedItem();
        table.getItems().removeAll(list);
        list = FXCollections.observableArrayList(ProjectListsManager.getListOfFluxes());
        table.setItems(list);
        table.getSelectionModel().select(flux);
     }

}
