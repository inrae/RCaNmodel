package fr.cm.GUItablesViews;

import fr.cm.GUIdialogs.ObservationsFromFileDialog;
import fr.cm.GUIdialogs.CharacteristicsOfFileDialog;
import fr.cm.GUIdialogs.HelpDialog;
import fr.cm.RCaNMain.Context;
import fr.cm.RCaNMain.MainApplication;
import fr.cm.canObjects.DataFile;
import fr.cm.canObjects.ProjectListsManager;
import fr.cm.parameters.ColorsAndFormats;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.scene.control.*;
import javafx.scene.layout.*;
import javafx.stage.FileChooser;

import java.io.File;

import static javafx.collections.FXCollections.observableArrayList;

public class DataFileTable extends Pane {

    double width = Context.getWindowWidth();
    double height =  Context.getWindowHeight();
    DataFile selectedDataFile;
    // GAUCHE
    Label leftTitle = new Label("Project data files");
    TableView<DataFile> tableOfFiles = new TableView<>();
    ObservableList<DataFile> listOfFiles;
    Button buttonNewFile = new Button("Add a data file to above list");
    Button characteristicsOfFile = new Button("Characteristics of selected file");
    Button addObservationsFromFile = new Button("Create observations from selected file");
    VBox leftBox = new VBox();

    public DataFileTable(){
        selectedDataFile = ProjectListsManager.getFirstDataFile();
        buttonNewFile.setOnAction(
                (ActionEvent e) -> {
                    // ADDING A FILE
                    String newFileName = observationFileChooser();
                    DataFile dataFile = new DataFile(newFileName);
                    ProjectListsManager.addDataFile(dataFile);
                    selectedDataFile = dataFile;
                    tableOfFiles.getSelectionModel().selectLast();
                    listOfFiles = observableArrayList(ProjectListsManager.getListOfDataFiles());
                    tableOfFiles.setItems(listOfFiles);
                    tableOfFiles.getSelectionModel().selectLast();
                }
        );

        characteristicsOfFile.setOnAction(
                (ActionEvent e) -> {
                    if(selectedDataFile == null) {
                        HelpDialog.warning("No selected data file","Warning");
                    } else {
                        new CharacteristicsOfFileDialog(selectedDataFile);
                    }
                }
        );
        addObservationsFromFile.setOnAction(
                (ActionEvent e) -> {
                    if (selectedDataFile == null) {
                        HelpDialog.warning("No selected data file", "Warning");
                    } else {
                        if (selectedDataFile.getNamesColumnInFile().size()==0) {
                            HelpDialog.warning("No columns in selected data file", "Warning");
                        } else {
                            new ObservationsFromFileDialog(selectedDataFile);
                        }
                    }
                }
        );
        TableColumn<DataFile, String> fileName = new TableColumn<>("Name");
        fileName.setPrefWidth(100);
        fileName.setCellValueFactory(features -> features.getValue().getShortNameProperty());
        tableOfFiles.getColumns().add(fileName);

        TableColumn<DataFile, String> fullFileName = new TableColumn<>("File name on disk");
        fullFileName.setPrefWidth(400);
        fullFileName.setCellValueFactory(features -> features.getValue().getFullFileNameProperty());
        tableOfFiles.getColumns().add(fullFileName);

        listOfFiles = observableArrayList(ProjectListsManager.getListOfDataFiles());
        tableOfFiles.setItems(listOfFiles);
        tableOfFiles.getSelectionModel().selectFirst();
        @SuppressWarnings("rawtypes")
        ObservableList<TablePosition> selectedCells = tableOfFiles.getSelectionModel().getSelectedCells();
        selectedCells.addListener((ListChangeListener<TablePosition>) c -> {
            TablePosition tablePosition = selectedCells.get(0);
            int nl = tablePosition.getRow();
            selectedDataFile = listOfFiles.get(nl);
        });

        fileName.setPrefWidth(0.3*width);
        leftTitle.setFont(ColorsAndFormats.titleFont);
        ColorsAndFormats.setVBoxCharacteristics(leftBox);

        HBox hBox = new HBox();
        ColorsAndFormats.setHBoxCharacteristics(hBox);
        hBox.getChildren().addAll(buttonNewFile, characteristicsOfFile, addObservationsFromFile);
        leftBox.getChildren().addAll(leftTitle, tableOfFiles, hBox);
        if(listOfFiles.size()>0){
            selectedDataFile = listOfFiles.get(0);
        }
        this.getChildren().add(leftBox);
    }

    static String observationFileChooser() {
        FileChooser fileChooser = new FileChooser();
        fileChooser.setTitle("Select file with observation");
        fileChooser.setInitialDirectory(Context.getWorkingDirectory());
        FileChooser.ExtensionFilter extFilter = new FileChooser.ExtensionFilter("Excel files", "*.csv");
        fileChooser.getExtensionFilters().add(extFilter);
        File selectedFile = fileChooser.showOpenDialog(MainApplication.stage);
        try{
            return(selectedFile.getAbsolutePath());
        }
        catch(Exception e){
            return null;
        }
    }
}

