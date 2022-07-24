/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.GUItablesViews;

import fr.cm.canObjects.Component;
import fr.cm.canObjects.ProjectListsManager;
import fr.cm.RCaNMain.Context;
import fr.cm.parameters.ColorsAndFormats;
import fr.cm.parameters.Strings;
import javafx.beans.property.SimpleDoubleProperty;
import javafx.beans.property.SimpleBooleanProperty;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.scene.control.*;
import javafx.scene.control.TableColumn.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.scene.control.cell.CheckBoxTableCell;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.layout.Pane;

import javafx.util.Callback;

/**
 * @author christianmullon
 */
public class ComponentTable extends Pane {

    double width = 0.9 * Context.getWindowWidth();
    double height =  0.9 * Context.getWindowHeight();

    final TableView<Component> table;
    ObservableList<Component> list;

    public ComponentTable() {
        super();
        int nbParameters = Strings.getNumberOfParameters();

        table = new TableView<>();
        table.setPrefWidth(0.9*Context.getWindowWidth());
        table.setEditable(true);

        TableColumn<Component, String> name = new TableColumn<>("Name");
        name.setCellValueFactory(new PropertyValueFactory<>("name"));
        name.setCellFactory(TextFieldTableCell.forTableColumn());
        name.setEditable(true);
        name.setMinWidth(130.0);
        name.setSortable(true);
        name.setOnEditCommit(
                event -> {
                    Component component = event.getTableView().getItems().get(event.getTablePosition().getRow());
                    component.changeName(event.getNewValue());
                }
        );
        table.getColumns().add(name);

        TableColumn<Component, Boolean> insideCol = new TableColumn<>("Inside system");
        insideCol.setCellValueFactory(param -> {
            Component component = param.getValue();
            SimpleBooleanProperty booleanProp = new SimpleBooleanProperty(component.isInside());
            booleanProp.addListener((observable, oldValue, newValue) -> component.changeInside(newValue));
            return booleanProp;
        });

        insideCol.setCellFactory(p -> new CheckBoxTableCell<>());
        table.getColumns().add(insideCol);

        for (int p = 0; p < nbParameters; p++) {
            final int q = p;
            TableColumn<Component, String> col = new TableColumn<>(Strings.getParametersNames()[p]);
            col.setMinWidth(130.0);
            col.setCellValueFactory(
                    param -> {
                        SimpleDoubleProperty sval = new SimpleDoubleProperty();
                        Component component = param.getValue();
                        sval.setValue(component.getParameters(q));
                        return sval.asString();
                    }
            );
            col.setCellFactory(TextFieldTableCell.forTableColumn());
            col.setOnEditCommit(
                    event -> {
                        Component component = event.getTableView().getItems().get(event.getTablePosition().getRow());
                        component.changeParameters(q, event.getNewValue());
                    }
            );

            table.getColumns().add(col);
        }

        list = FXCollections.observableArrayList(ProjectListsManager.getListOfComponents());
        table.setItems(list);
        table.getSelectionModel().selectFirst();

        final Button deleteG = new Button("Delete");
        deleteG.setOnAction((ActionEvent e) -> {
            if (table.getSelectionModel().getSelectedItem() != null) {
                Component component = table.getSelectionModel().getSelectedItem();
                ProjectListsManager.removeComponent(component);
                list.removeAll(component);
                updateTable();
            }
        });

        final Button buttonUp = new Button("Up");
        buttonUp.setOnAction((ActionEvent e) -> {
            Component component = table.getSelectionModel().getSelectedItem();
            ProjectListsManager.upComponent(component);
            updateTable();
        });

        final Button buttonDown = new Button("Down");
        buttonDown.setOnAction((ActionEvent e) -> {
            Component component = table.getSelectionModel().getSelectedItem();
            ProjectListsManager.downComponent(component);
            updateTable();
        });

        final HBox hboxButtons = new HBox();
        hboxButtons.getChildren().addAll(buttonUp, buttonDown, deleteG);
        hboxButtons.setSpacing(50);
        // hboxButtons.setMinSize(800.0, 120.0);

        final Label title = new Label("System components");
        title.setFont(ColorsAndFormats.titleFont);
        final VBox vbox = new VBox();
        ColorsAndFormats.setVBoxCharacteristics(vbox);
        vbox.getChildren().addAll(title, table, hboxButtons);
        // vbox.setMinSize(1000.0, 750.0);

        this.getChildren().addAll(vbox);
    }

    public void updateTable() {
        Component component = table.getSelectionModel().getSelectedItem();
        table.getItems().removeAll(list);
        list = FXCollections.observableArrayList(ProjectListsManager.getListOfComponents());
        table.setItems(list);
        table.getSelectionModel().select(component);
    }

}
