/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.GUItablesViews;

import fr.cm.canObjects.Constraint;

import fr.cm.ProjectManager.ProjectListsManager;
import fr.cm.RCaNMain.Context;
import fr.cm.GUIdialogs.ConstraintNewOrEditDialog;
import fr.cm.parameters.ColorsAndFormats;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.scene.control.*;
import javafx.scene.control.cell.CheckBoxTableCell;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;

/**
 * @author christianmullon
 */
public class ConstraintTable extends Pane {

    final TableView<Constraint> constraintTable;
    ObservableList<Constraint> list;
    final Button editC = new Button("Edit");
    final Button newC = new Button("Add");
    final Button buttonUp = new Button("Up");
    final Button buttonDown = new Button("Down");
    final Button deleteC = new Button("Delete");

    final HBox hboxButtons = new HBox();
    final VBox vbox = new VBox();

    double width = Context.getWindowWidth();
    double height = Context.getWindowHeight();

    public ConstraintTable() {
        constraintTable = new TableView<>();
        constraintTable.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
        constraintTable.setEditable(true);

        TableColumn<Constraint, String> name = new TableColumn<>("Name");
        name.setCellValueFactory(new PropertyValueFactory<>("name"));
        name.setCellFactory(TextFieldTableCell.forTableColumn());
        name.setSortable(true);
        name.setEditable(true);
        name.setOnEditCommit(
                event -> {
                    Constraint constraint = event.getTableView().getItems().get(event.getTablePosition().getRow());
                    constraint.changeName(event.getNewValue());
                }
        );

        TableColumn<Constraint, String> formula = new TableColumn<>("Formula");
        formula.setCellValueFactory(new PropertyValueFactory<>("formula"));
        formula.setSortable(true);
        formula.setEditable(false);

        TableColumn<Constraint, Boolean> activeCol = new TableColumn<>("Active y/n");
        activeCol.setCellValueFactory(param -> {
            Constraint constraint = param.getValue();
            SimpleBooleanProperty booleanProp = new SimpleBooleanProperty(constraint.isActive());
            booleanProp.addListener((observable, oldValue, newValue) -> constraint.changeActive(newValue));
            return booleanProp;
        });
        activeCol.setCellFactory(p -> new CheckBoxTableCell<>());
        activeCol.setEditable(true);

        TableColumn<Constraint, String> years = new TableColumn<>("Years");
        years.setCellValueFactory(new PropertyValueFactory<>("years"));
        years.setEditable(false);
        years.setSortable(true);

        TableColumn<Constraint, String> comment = new TableColumn<>("Annotation");
        comment.setCellValueFactory(new PropertyValueFactory<>("comment"));
        comment.setCellFactory(TextFieldTableCell.forTableColumn());

        comment.setCellFactory(EditCell.forTableColumn() );

        comment.setEditable(true);
        comment.setSortable(true);
        comment.setOnEditCommit(
                event -> {
                    Constraint constraint = event.getTableView().
                            getItems().get(event.getTablePosition().getRow());
                    constraint.setComment(event.getNewValue());
                }
        );

        constraintTable.getColumns().add(name);
        constraintTable.getColumns().add(formula);
        constraintTable.getColumns().add(activeCol);
        constraintTable.getColumns().add(years);
        constraintTable.getColumns().add(comment);
        constraintTable.setPrefHeight(0.7 * height);
        constraintTable.setPrefWidth(0.8 * width);


        list = FXCollections.observableArrayList(ProjectListsManager.getListOfConstraints());
        constraintTable.setItems(list);

        constraintTable.getSelectionModel().selectFirst();

        newC.setOnAction((ActionEvent e) -> {
            new ConstraintNewOrEditDialog();
            updateTable();
        });

        editC.setOnAction((ActionEvent e) -> {
            Constraint constraint = constraintTable.getSelectionModel().getSelectedItem();
            new ConstraintNewOrEditDialog(constraint);
            updateTable();
        });

        buttonUp.setOnAction((ActionEvent e) -> {
            Constraint constraint = constraintTable.getSelectionModel().getSelectedItem();
            ProjectListsManager.upConstraint(constraint);
            updateTable();
        });

        buttonDown.setOnAction((ActionEvent e) -> {
            Constraint constraint = constraintTable.getSelectionModel().getSelectedItem();
            ProjectListsManager.downConstraint(constraint);
            updateTable();
        });

        deleteC.setOnAction((ActionEvent e) -> {
            if (constraintTable.getSelectionModel().getSelectedItem() != null) {
                Constraint constraint = constraintTable.getSelectionModel().getSelectedItem();
                ProjectListsManager.removeConstraints(constraint);
                list.removeAll(constraint);
                updateTable();
            }
        });
        // -------------------------------------------------------------------------------------------------------------
        hboxButtons.getChildren().addAll(buttonUp, buttonDown, newC, editC, deleteC);
        hboxButtons.setSpacing(80.0);
        // -------------------------------------------------------------------------------------------------------------
        formula.setMinWidth(0.35 * width);
        years.setMinWidth(0.15 * width);
        comment.setMinWidth(0.3 * width);

        final Label title = new Label("Constraints");
        title.setFont(ColorsAndFormats.titleFont);
        ColorsAndFormats.setVBoxCharacteristics(vbox);
        vbox.getChildren().addAll(title, constraintTable, hboxButtons);

        // -------------------------------------------------------------------------------------------------------------
        this.getChildren().addAll(vbox);
    }

    public void updateTable() {
        Constraint constraint = constraintTable.getSelectionModel().getSelectedItem();
        constraintTable.getItems().removeAll(list);
        list = FXCollections.observableArrayList(ProjectListsManager.getListOfConstraints());
        constraintTable.setItems(list);
        constraintTable.getSelectionModel().select(constraint);
    }
}
