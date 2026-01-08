package fr.cm.objects;

import fr.cm.Main.Context;
import fr.cm.Main.MainApplication;
import fr.cm.Main.ObjectsManager;
import fr.cm.preferences.ColorsAndFormats;
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

    double width = Context.getWindowWidth(),
            height =  Context.getWindowHeight();
    DataFile selectedDataFile;
    // GAUCHE
    Label leftTitle = new Label("Project data files");
    ObservableList<DataFile> listOfFiles;
    Button buttonNewFile = new Button("Add a datafile to above list"),
            buttonObservations = new Button("Add or remove observations in selected datafile"),
            buttonCharacteristics = new Button("Edit annotations of selected datafile");
    VBox leftBox = new VBox();

    public DataFileTable(){
        TableView<DataFile> tableOfFiles = new TableView<>();

        TableColumn<DataFile, String> idCol = new TableColumn<>("Id");
        idCol.setPrefWidth(0.05 * width);
        idCol.setCellValueFactory(new PropertyValueFactory<>("id"));
        tableOfFiles.getColumns().add(idCol);

        TableColumn<DataFile, String> fileNameCol = new TableColumn<>("Name");
        fileNameCol.setPrefWidth(0.2 * width);
        fileNameCol.setCellValueFactory(new PropertyValueFactory<>("shortName"));
        tableOfFiles.getColumns().add(fileNameCol);

        TableColumn<DataFile, String> observationsCol = new TableColumn<>("Observations used from this files");
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

        listOfFiles = observableArrayList(ObjectsManager.getListOfDataFiles());
        tableOfFiles.setItems(listOfFiles);
        tableOfFiles.getSelectionModel().selectFirst();
        @SuppressWarnings("rawtypes")
        ObservableList<TablePosition> selectedCells = tableOfFiles.getSelectionModel().getSelectedCells();
        selectedCells.addListener((ListChangeListener<TablePosition>) c -> {
            int nl = 0;
            if(selectedCells.size()>0) {
                TablePosition tablePosition = selectedCells.get(0);
                nl = tablePosition.getRow();
            }
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
                    ObjectsManager.addDataFile(dataFile, true);
                    selectedDataFile = dataFile;
                    tableOfFiles.getSelectionModel().selectLast();
                    listOfFiles = observableArrayList(ObjectsManager.getListOfDataFiles());
                    tableOfFiles.setItems(listOfFiles);
                    tableOfFiles.getSelectionModel().selectLast();
                }
        );

        buttonCharacteristics.setOnAction(
                (ActionEvent e) -> {
                    if (selectedDataFile != null) {
                        new DataFileSetCharacteristicsDialog(selectedDataFile);
                        listOfFiles = observableArrayList(ObjectsManager.getListOfDataFiles());
                        tableOfFiles.setItems(listOfFiles);
                        tableOfFiles.getSelectionModel().selectLast();
                        tableOfFiles.refresh();
                    }
                }
        );

        buttonObservations.setOnAction(
                (ActionEvent e) -> {
                    if (selectedDataFile != null) {
                        new DataFileAddObservationDialog(selectedDataFile);
                        listOfFiles = observableArrayList(ObjectsManager.getListOfDataFiles());
                        tableOfFiles.setItems(listOfFiles);
                        tableOfFiles.getSelectionModel().selectLast();
                        tableOfFiles.refresh();
                    }
                }
        );

        HBox hBoxButton = new HBox(50);
        hBoxButton.getChildren().addAll(buttonNewFile, buttonObservations, buttonCharacteristics);
        hBoxButton.setSpacing(20);
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
        fileChooser.setInitialDirectory(new File(Context.getDirName()));
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

