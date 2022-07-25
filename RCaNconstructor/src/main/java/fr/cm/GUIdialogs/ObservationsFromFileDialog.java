package fr.cm.GUIdialogs;

import fr.cm.RCaNMain.Context;
import fr.cm.canObjects.DataFile;
import fr.cm.canObjects.Observation;
import fr.cm.ProjectManager.ProjectListsManager;
import fr.cm.parameters.ColorsAndFormats;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.layout.*;
import javafx.scene.text.Text;
import javafx.util.Callback;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static javafx.collections.FXCollections.observableArrayList;

public class ObservationsFromFileDialog extends Dialog <ButtonType> {


    double width = Context.getWindowWidth();
    double height =  Context.getWindowHeight();
    DataFile selectedDataFile = null;
    // DROITE
    Label rightTitle;
    Label systemTitle;
    TableView<String[]> tableOfDataValues = new TableView<>();
    TableView<Observation> tableOfObservationsInSystem = new TableView<Observation>();
    ObservableList<String[]> listOfDataValues;
    ObservableList<Observation> listOfObservationsInSystem;
    List<Observation> listOfAddedObservations;
    List<Observation> projectObservations;
    List<Observation> listORemovedObservations;
    Text textAddObservation, textRemoveObservation;
    Button buttonRemove = new Button("Remove selected observation");

    int rightWidth = (int)(0.3 * width);

    public ObservationsFromFileDialog(DataFile selectedDataFile){
        projectObservations = new ArrayList<>(ProjectListsManager.getListOfObservations());
        listOfAddedObservations = new ArrayList<>();
        listORemovedObservations = new ArrayList<>();
        this.selectedDataFile = selectedDataFile;
        buttonRemove.setOnAction(
                (ActionEvent e) -> {
                    // REMOVING AN OBSERVATION (?)
                    if (tableOfObservationsInSystem.getSelectionModel().getSelectedItem() != null) {
                        Observation observation = tableOfObservationsInSystem.getSelectionModel().getSelectedItem();
                        ProjectListsManager.removeObservation(observation.getObsName());
                        updateBottom();
                        listORemovedObservations.add(observation);
                    }
                }
                );

        textAddObservation = new Text("Click on the header of a column to add it as an observation of the system");
        textAddObservation.wrappingWidthProperty().set(rightWidth);
        textRemoveObservation = new Text("To remove an observation in the above list, select it and then click Button Remove");
        textRemoveObservation.wrappingWidthProperty().set(rightWidth);

        rightTitle = new Label("Columns of selected file");
        systemTitle = new Label("Observations in the system");
        rightTitle.setFont(ColorsAndFormats.titleFont);
        systemTitle.setFont(ColorsAndFormats.titleFont);
        VBox vBoxLeft = new VBox();
        vBoxLeft.getChildren().addAll(rightTitle, tableOfDataValues, textAddObservation);
        VBox vBoxRight = new VBox();
        vBoxRight.getChildren().addAll(
                systemTitle, tableOfObservationsInSystem,
                textRemoveObservation, buttonRemove);
        HBox hBox = new HBox();
        hBox.getChildren().addAll(vBoxLeft,vBoxRight);
        updateTop();
        updateBottom();
        vBoxLeft.setPrefWidth(0.3 * width);
        vBoxRight.setPrefWidth(0.3 * width);
        ColorsAndFormats.setVBoxCharacteristics(vBoxLeft);
        ColorsAndFormats.setVBoxCharacteristics(vBoxRight);
        ColorsAndFormats.setHBoxCharacteristics(hBox);
        this.setTitle("Add and remove observation from "+ selectedDataFile.getShortName());
        this.getDialogPane().setContent(hBox);
        this.getDialogPane().getButtonTypes().addAll(ButtonType.OK, ButtonType.CANCEL);
        this.getDialogPane().setStyle(ColorsAndFormats.font);

        Optional<ButtonType> result = this.showAndWait();
        if (result.isPresent()) {
            if (result.get() == ButtonType.OK) {
                selectedDataFile.updateObservations(listOfAddedObservations, listORemovedObservations);
            }
            if (result.get() == ButtonType.CANCEL) {
                ProjectListsManager.setListOfObservations(projectObservations);
            }
        }
    }


    public void updateTop() {
        tableOfDataValues.getColumns().clear();
        if (selectedDataFile != null) {
            int nc = selectedDataFile.getNumberOfColumns();
            if (nc > 0) {
                String[][] dataValues = selectedDataFile.getFormattedDataInFile();
                List<String> namesOfColumns = selectedDataFile.getNamesColumnInFile();
                for (int col = 0; col < nc; col++) {
                    TableColumn tableColumn = new TableColumn();
                    Label colHeader = new Label(namesOfColumns.get(col));
                    colHeader.setOnMouseClicked(event -> {
                        Label labs = (Label) event.getSource();
                        String colInDataFile = labs.getText();
                        Observation newObservation = selectedDataFile.newObservationFromColumn(colInDataFile);
                        updateBottom();
                        if(newObservation != null) {
                            listOfAddedObservations.add(newObservation);
                        }
                    });
                    tableColumn.setGraphic(colHeader);
                    tableColumn.setSortable(false);
                    final int colNo = col;
                    tableColumn.setCellValueFactory(new Callback<TableColumn.CellDataFeatures<String[], String>, ObservableValue<String>>() {
                        @Override
                        public ObservableValue<String> call(TableColumn.CellDataFeatures<String[], String> p) {
                            return new SimpleStringProperty((p.getValue()[colNo]));
                        }
                    });
                    tableColumn.setPrefWidth(100);
                    tableOfDataValues.getColumns().add(tableColumn);
                }
                listOfDataValues = observableArrayList(Arrays.asList(dataValues));
                tableOfDataValues.setItems(listOfDataValues);
            }
        }
    }

    public void updateBottom() {
        tableOfObservationsInSystem.getColumns().clear();
        TableColumn<Observation, String>    obsName = new TableColumn<>("Name");
        obsName.setCellValueFactory(new PropertyValueFactory<>("obsName"));
        obsName.setEditable(false);
        obsName.setPrefWidth(100);
        TableColumn<Observation, String>    dataFileName = new TableColumn<>("Data File");
        dataFileName.setCellValueFactory(new PropertyValueFactory<>("dataFileName"));
        dataFileName.setEditable(false);
        dataFileName.setPrefWidth(300);
        tableOfObservationsInSystem.getColumns().add(obsName);
        tableOfObservationsInSystem.getColumns().add(dataFileName);
        listOfObservationsInSystem = observableArrayList(ProjectListsManager.getListOfObservations());
        tableOfObservationsInSystem.setItems(listOfObservationsInSystem);
        tableOfObservationsInSystem.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
    }

}

