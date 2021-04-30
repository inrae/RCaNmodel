/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.dialogs;

import fr.cm.canObjects.*;
import fr.cm.menus.Context;
import fr.cm.menus.MainApplication;
import fr.cm.parameters.Units;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.geometry.Insets;
import javafx.scene.control.*;
import javafx.scene.control.cell.ComboBoxTableCell;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;
import javafx.stage.FileChooser;
import javafx.util.Callback;

import java.io.File;

import static javafx.collections.FXCollections.*;

/**
 * @author christianmullon
 */
public class ExternalDataFileDialog extends Pane {

    double width = Context.getWidth();
    double height =  Context.getHeight();

    TableView<ExternalDataFile> tableFiles = new TableView<>();
    TableView<String>  tableIn = new TableView<>();
    TableView<Observation> tableObs = new TableView<Observation>();

    ObservableList<ExternalDataFile> listFiles;
    ObservableList<String> listIn;
    ObservableList<Observation> listObs;

    ExternalDataFile selectedFile = null;

    Label leftTitle = new Label("Project data files");
    Label middleTitle = new Label("Meta information about selected file");
    Label rightTitle = new Label("Project observation data");
    Label labelPath = new Label("--");
    Label labelFirstYear = new Label("First year : ");
    Label labelLastYear = new Label("Last year : ");

    TextArea valueMetaInformation = new TextArea(" ");
    TextField valueFirstYear = new TextField("");
    TextField valueLastYear = new TextField("");

    Button buttonNew = new Button("Add a new data file in above list");
    Button buttonRemove = new Button("Remove selected observation");
    Button buttonSet = new Button("Confirm changes");

    VBox leftBox = new VBox();
    VBox middleBox = new VBox();
    VBox rightBox = new VBox();
    HBox hBox = new HBox();
    HBox firstYearBox = new HBox();
    HBox lastYearBox = new HBox();
    VBox yearsBox = new VBox();

    public ExternalDataFileDialog() {
        // -------------------------------------------------------------------------------------------------------------
        buttonNew.setOnAction(
                (ActionEvent e) -> {
                    // ADDING A FILE
                    String newFileName= observationFileChooser();
                    ExternalDataFile externalDataFile = new ExternalDataFile(newFileName);
                    ListsManager.addFileWithObservation(externalDataFile);
                    tableFiles.setItems(observableArrayList(ListsManager.getListOfExternalDataFiles()));
                    selectedFile = externalDataFile;
                    tableFiles.getSelectionModel().selectLast();
                    updateTables();
                }
        );
        buttonRemove.setOnAction(
                (ActionEvent e) -> {
                    // REMOVING AN OBSERVATION (?)
                    if (tableObs.getSelectionModel().getSelectedItem() != null) {
                        Observation observation = tableObs.getSelectionModel().getSelectedItem();
                        ListsManager.removeObservation(observation.getObsName());
                        updateTables();
                    }
                }
        );
        buttonSet.setOnAction(
                (ActionEvent e) -> {
                    setChanges();
                }
        );
        // -------------------------------------------------------------------------------------------------------------
        tableFiles.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
        tableFiles.setRowFactory(tv -> {
            // CLIC SUR UNE LIGNE DE LA TABLE = SELECTIONNER UN FICHIER
            TableRow<ExternalDataFile> row = new TableRow<>();
            row.setOnMouseClicked(event -> {
                if (!row.isEmpty()) {
                    selectedFile = row.getItem();
                    updateTables();
                }
            });
            return row;
        });
        TableColumn<ExternalDataFile, String> fileName = new TableColumn<>("File Name");
        fileName.setCellValueFactory(new PropertyValueFactory<>("fileName"));
        fileName.setSortable(true);
        fileName.setPrefWidth(0.30*width);
        tableFiles.getColumns().add(fileName);
        listFiles = observableArrayList(ListsManager.getListOfExternalDataFiles());
        tableFiles.setItems(listFiles);
        tableFiles.getSelectionModel().selectLast();
        // -------------------------------------------------------------------------------------------------------------
        tableIn.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
        tableIn.setRowFactory(tv -> {
            // CLIC ON A COLUMN IN THE LIST
            TableRow<String> rowIn = new TableRow<>();
            rowIn.setOnMouseClicked(event -> {
                if (!rowIn.isEmpty()) {
                    String nameCol = rowIn.getItem();
                    createObservationFromColumn(nameCol);
                }
            });
            return rowIn;
        });
        TableColumn<String, String> columnNameIn = new TableColumn<>("Select a file column to create an observation");
        columnNameIn.setCellValueFactory(cell -> new ReadOnlyStringWrapper(cell.getValue()));
        columnNameIn.setPrefWidth(0.25*width);
        tableIn.getColumns().add(columnNameIn);
        tableIn.getSelectionModel().selectFirst();
         // -------------------------------------------------------------------------------------------------------------
        TableColumn<Observation, String> obsName = new TableColumn<>("Name");
        obsName.setCellValueFactory(new PropertyValueFactory<>("obsName"));
        obsName.setCellFactory(TextFieldTableCell.<Observation> forTableColumn());
        obsName.setOnEditCommit((TableColumn.CellEditEvent<Observation, String> event) -> {
            // EDIT THE NAME OF AN OBSERVATION
            TablePosition<Observation, String> pos = event.getTablePosition();
            String newObsName = event.getNewValue();
            int row = pos.getRow();
            Observation observation = event.getTableView().getItems().get(row);
            observation.setObsName(newObsName);
            updateTables();
        });
        TableColumn<Observation, Units> obsUnit = new TableColumn<>("Unit");
        obsUnit.setCellValueFactory(new PropertyValueFactory<>("unit"));
        ObservableList<Units> unitsList = FXCollections.observableArrayList(Units.values());
        obsUnit.setCellValueFactory(new Callback<>() {
            @Override
            public ObservableValue<Units> call(TableColumn.CellDataFeatures<Observation, Units> param) {
                Observation observation = param.getValue();
                String unitCode = observation.getUnit();
                Units unit = Units.getByCode(unitCode);
                return new SimpleObjectProperty<Units>(unit);
            }
        });
        obsUnit.setCellFactory(ComboBoxTableCell.forTableColumn(unitsList));
        obsUnit.setOnEditCommit((TableColumn.CellEditEvent<Observation, Units> event) -> {
            // CHANGE UNIT
            TablePosition<Observation, Units> pos = event.getTablePosition();
            Units newUnits = event.getNewValue();
            int row = pos.getRow();
            Observation observation = event.getTableView().getItems().get(row);
            observation.setUnit(newUnits.getCode());
            updateTables();
        });
        TableColumn<Observation, String>    obsFile = new TableColumn<>("File");
        obsFile.setCellValueFactory(new PropertyValueFactory<>("originalFile"));
        TableColumn<Observation, String>    obsColumn = new TableColumn<>("Column");
        obsColumn.setCellValueFactory(new PropertyValueFactory<>("originalColumn"));
        obsName.setEditable(true);
        obsUnit.setEditable(true);
        tableObs.setEditable(true);
        tableObs.getColumns().add(obsName);
        tableObs.getColumns().add(obsUnit);
        tableObs.getColumns().add(obsFile);
        tableObs.getColumns().add(obsColumn);
        listObs =  observableArrayList(ListsManager.getListOfObservations());
        tableObs.setItems(listObs);
        tableObs.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
        // ---------------------------
        valueMetaInformation.setPrefHeight(200.0);
        tableFiles.setPrefHeight(0.3*height);
        tableObs.setPrefHeight(0.5*height);
        tableIn.setPrefHeight(0.3*height);
        tableFiles.setPrefWidth(0.3*width);
        tableObs.setPrefWidth(0.4*width);
        tableIn.setPrefWidth(0.4*width);
        // -------------------------------------------------------------------------------------------------------------
        updateTables();
        // -------------------------------------------------------------------------------------------------------------
        leftBox.setSpacing(10.0);
        middleBox.setSpacing(10.0);
        rightBox.setSpacing(10.0);
        hBox.setSpacing(10.0);

        leftBox.setPrefSize(0.30*width,0.9*height);
        middleBox.setPrefSize(0.30*width,0.9*height);
        rightBox.setPrefSize(0.40*width,0.9*height);
        
        leftBox.getChildren().addAll(leftTitle, tableFiles, buttonNew);

        firstYearBox.getChildren().addAll(labelFirstYear, valueFirstYear);
        lastYearBox.getChildren().addAll(labelLastYear, valueLastYear);
        yearsBox.getChildren().addAll(firstYearBox, lastYearBox);
        middleBox.getChildren().addAll(middleTitle, valueMetaInformation, labelPath, yearsBox, buttonSet);

        rightBox.getChildren().addAll(rightTitle, tableIn, tableObs, buttonRemove);

        hBox.getChildren().addAll(leftBox, middleBox, rightBox);
        hBox.setPrefWidth(width);
        hBox.setPrefHeight(height);
        hBox.setPadding(new Insets(10, 10, 10, 10));
        // -------------------------------------------------------------------------------------------------------------
        this.getChildren().addAll(hBox);
    }

    void updateTables() {
        selectedFile = tableFiles.getSelectionModel().getSelectedItem();
        if (selectedFile != null) {
            labelPath.setText(selectedFile.getFullFileName());
            valueFirstYear.setText(selectedFile.getFirstYear());
            valueLastYear.setText(selectedFile.getLastYear());
            valueMetaInformation.setText(selectedFile.getMetaInformationAboutDataFile());
            listObs =  observableArrayList(ListsManager.getListOfObservations());
            tableObs.setItems(listObs);
            tableObs.refresh();
            listIn = observableArrayList(selectedFile.getNamesColumnInFile());
            tableIn.setItems(listIn);
            tableIn.refresh();
        }
    }

    void createObservationFromColumn(String nameColumnInFile) {
        selectedFile = tableFiles.getSelectionModel().getSelectedItem();
        if (selectedFile != null) {
            selectedFile.setFirstYear(valueFirstYear.getText());
            selectedFile.setLastYear(valueLastYear.getText());
            selectedFile.addObservationFromColumn(nameColumnInFile);
            updateTables();
        }
    }

    void setChanges(){
        selectedFile.setFirstYear(valueFirstYear.getText());
        selectedFile.setLastYear(valueLastYear.getText());
        selectedFile.setMetaInformationAboutDatatFile(valueMetaInformation.getText());
    }

    static String observationFileChooser() {
        FileChooser fileChooser = new FileChooser();
        fileChooser.setTitle("Select file with observation");
        fileChooser.setInitialDirectory(Context.getWorkingDirectory());
        FileChooser.ExtensionFilter extFilter = new FileChooser.ExtensionFilter("Excel files", "*.csv");
        fileChooser.getExtensionFilters().add(extFilter);
        File selectedFile = fileChooser.showOpenDialog(MainApplication.stage);
        try{
            String fileName = selectedFile.getAbsolutePath();
            return(fileName);
        }
        catch(Exception e){
            return null;
        }
    }
}

