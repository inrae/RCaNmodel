package fr.cm.GUItablesViews;

import fr.cm.GUIdialogs.DataFileObservationsDialog;
import fr.cm.GUIdialogs.DataFileCharacteristicsDialog;
import fr.cm.GUIdialogs.HelpDialog;
import fr.cm.RCaNMain.Context;
import fr.cm.RCaNMain.MainApplication;
import fr.cm.canObjects.DataFile;
import fr.cm.ProjectManager.ProjectListsManager;
import fr.cm.parameters.ColorsAndFormats;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.layout.*;
import javafx.scene.text.Text;
import javafx.stage.FileChooser;

import java.io.File;

import static javafx.collections.FXCollections.observableArrayList;

public class DataFileTable extends Pane {
// ---------------------------------

    double width = Context.getWindowWidth();
    double height =  Context.getWindowHeight();
    DataFile selectedDataFile;
    // GAUCHE
    Label leftTitle = new Label("Project data files");
    ObservableList<DataFile> listOfFiles;
    Button buttonNewFile = new Button("Add");
    Button buttonObservations = new Button("Observations");
    Button buttonCharacteristics = new Button("Characteristics");
    VBox leftBox = new VBox();

    public DataFileTable(){
        TableView<DataFile> tableOfFiles = new TableView<>();

        tableOfFiles.setOnMouseClicked(click -> {
            if (click.getClickCount() == 2) {
                @SuppressWarnings("rawtypes")
                TablePosition pos = tableOfFiles.getSelectionModel().getSelectedCells().get(0);
                int row = pos.getRow();
                int col = pos.getColumn();
                selectedDataFile = ProjectListsManager.getListOfDataFiles().get(row);
                Context.setTextAreaContent("False");
                if(col==3){
                    new DataFileCharacteristicsDialog(selectedDataFile);
                    tableOfFiles.refresh();
                }
                if(col == 2) {
                    if (selectedDataFile.getNamesColumnInFile().size()==0) {
                        HelpDialog.warning("No columns in selected data file", "Warning");
                    } else {
                        new DataFileObservationsDialog(selectedDataFile);
                        tableOfFiles.refresh();
                    }
                }
            }
        });

        TableColumn<DataFile, String> idCol = new TableColumn<>("Id");
        idCol.setPrefWidth(0.05 * width);
        idCol.setCellValueFactory(new PropertyValueFactory<>("id"));
        tableOfFiles.getColumns().add(idCol);

        TableColumn<DataFile, String> fileNameCol = new TableColumn<>("Name");
        fileNameCol.setPrefWidth(0.2 * width);
        fileNameCol.setCellValueFactory(new PropertyValueFactory<>("shortName"));
        tableOfFiles.getColumns().add(fileNameCol);

        TableColumn<DataFile, String> observationsCol = new TableColumn<>("Observations");
        observationsCol.setPrefWidth(0.2 * width);
        observationsCol.setCellValueFactory(new PropertyValueFactory<>("stringAddedObservations"));
        observationsCol.setCellFactory(tc -> {
            TableCell<DataFile, String> cell = new TableCell<>();
            Text text = new Text();
            cell.setGraphic(text);
            text.wrappingWidthProperty().bind(observationsCol.widthProperty());
            text.textProperty().bind(cell.itemProperty());
            return cell ;
        });
        observationsCol.setEditable(false);
        tableOfFiles.getColumns().add(observationsCol);

        TableColumn<DataFile, String> commentsCol = new TableColumn<>("Annotations");
        commentsCol.setPrefWidth(0.4*width);
        commentsCol.setCellValueFactory(new PropertyValueFactory<>("metaInformation"));
        commentsCol.setCellFactory(tc -> {
            TableCell<DataFile, String> cell = new TableCell<>();
            Text text = new Text();
            cell.setGraphic(text);
            text.wrappingWidthProperty().bind(observationsCol.widthProperty());
            text.textProperty().bind(cell.itemProperty());
            return cell ;
        });
        commentsCol.setEditable(false);
        tableOfFiles.getColumns().add(commentsCol);


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

        tableOfFiles.setPrefWidth(0.8*width);
        tableOfFiles.setPrefHeight(0.7*height);

        leftTitle.setFont(ColorsAndFormats.titleFont);

        buttonNewFile.setOnAction(
                (ActionEvent e) -> {
                    // ADDING A FILE
                    String newFileName = observationFileChooser();
                    DataFile dataFile = new DataFile(newFileName);
                    ProjectListsManager.addDataFile(dataFile, true);
                    selectedDataFile = dataFile;
                    tableOfFiles.getSelectionModel().selectLast();
                    listOfFiles = observableArrayList(ProjectListsManager.getListOfDataFiles());
                    tableOfFiles.setItems(listOfFiles);
                    tableOfFiles.getSelectionModel().selectLast();
                }
        );

        buttonCharacteristics.setOnAction(
                (ActionEvent e) -> {
                    if (selectedDataFile != null) {
                        new DataFileCharacteristicsDialog(selectedDataFile);
                        listOfFiles = observableArrayList(ProjectListsManager.getListOfDataFiles());
                        tableOfFiles.setItems(listOfFiles);
                        tableOfFiles.getSelectionModel().selectLast();
                    }
                }
        );

        buttonObservations.setOnAction(
                (ActionEvent e) -> {
                    if (selectedDataFile != null) {
                        new DataFileObservationsDialog(selectedDataFile);
                        listOfFiles = observableArrayList(ProjectListsManager.getListOfDataFiles());
                        tableOfFiles.setItems(listOfFiles);
                        tableOfFiles.getSelectionModel().selectLast();
                    }
                }
        );

        final Label how = new Label("Include observations or edit annotations with a double clic on cell");
        HBox hBoxButton = new HBox(50);
        hBoxButton.getChildren().addAll(buttonNewFile, how);
        hBoxButton.setSpacing(80);
        leftBox.getChildren().addAll(leftTitle, tableOfFiles, hBoxButton);
        if(listOfFiles.size()>0){
            selectedDataFile = listOfFiles.get(0);
        }
        ColorsAndFormats.setVBoxCharacteristics(leftBox);
        this.getChildren().add(leftBox);
    }

    static String observationFileChooser() {
        FileChooser fileChooser = new FileChooser();
        fileChooser.setTitle("Select a data file with observation");
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

