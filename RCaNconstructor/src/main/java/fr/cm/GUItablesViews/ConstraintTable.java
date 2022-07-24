/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.GUItablesViews;

import fr.cm.canObjects.Constraint;

import fr.cm.canObjects.ProjectListsManager;
import fr.cm.RCaNMain.Context;
import fr.cm.GUIdialogs.ConstraintNewOrEditDialog;
import fr.cm.parameters.ColorsAndFormats;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
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
import javafx.util.Callback;

/**
 * @author christianmullon
 */
public class ConstraintTable extends Pane {

    final TableView<Constraint> constraintTable;
    ObservableList<Constraint> list;
    final Button editC = new Button("Edit selected constraint");
    final Button newC = new Button("Add a new constraint");
    final Button buttonUp = new Button("Up");
    final Button buttonDown = new Button("Down");
    final Button deleteC = new Button("Delete");

    final HBox hboxButtons = new HBox();
    final VBox vbox = new VBox();

    double width = 0.9 * Context.getWindowWidth();
    double height = 0.9 * Context.getWindowHeight();

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

        TableColumn<Constraint, Boolean> activeCol = new TableColumn<>("Active constraint");
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

        /*
        TableColumn<Constraint, String> comment = new TableColumn<>("Comment");
        comment.setCellValueFactory(new PropertyValueFactory<>("comment"));
        comment.setCellFactory(TextFieldTableCell.forTableColumn());
        comment.setEditable(true);
        comment.setSortable(true);
        comment.setOnEditCommit(
                event -> {
                    Constraint constraint = event.getTableView().
                        getItems().get(event.getTablePosition().getRow());
                    constraint.setComment(event.getNewValue());
                }
        );
        */

        TableColumn<Constraint, String> comment = new TableColumn<>("Comment");
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
        hboxButtons.getChildren().addAll(buttonUp, buttonDown, editC, newC, deleteC);
        hboxButtons.setSpacing(10.0);
        // -------------------------------------------------------------------------------------------------------------
        constraintTable.setMinHeight(0.8 * height);
        constraintTable.setMinWidth(0.8 * width);
        formula.setMinWidth(0.35 * width);
        years.setMinWidth(0.15 * width);
        comment.setMinWidth(0.3 * width);

        final Label title = new Label("System constraints");
        title.setFont(ColorsAndFormats.titleFont);
        ColorsAndFormats.setVBoxCharacteristics(vbox);
        vbox.getChildren().addAll(title, constraintTable, hboxButtons);

        // -------------------------------------------------------------------------------------------------------------
        this.setMinWidth(width);
        this.setMinHeight(height);
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
