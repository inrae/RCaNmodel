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

public class DataFileObservationsDialog extends Dialog <ButtonType> {

    double width = Context.getWindowWidth();
    double height =  Context.getWindowHeight();
    DataFile selectedDataFile = null;
    // DROITE
    Label rightTitle,  systemTitle;
    TableView<String[]> tableOfDataValues ;
    TableView<Observation> tableOfObservationsFromDataFile ;
    ObservableList<String[]> listOfDataValues;
    Text textAddObservation, textRemoveObservation;
    Button buttonRemove = new Button("Remove selected observation");

    int rightWidth = (int)(0.3 * width);
    int leftWidth = (int)(0.7 * width);

    public DataFileObservationsDialog(DataFile selectedDataFile){
        this.selectedDataFile = selectedDataFile;

        makeTableOfDataInFile();
        makeTableExtractedObservations();

        textAddObservation = new Text("Click on the header of a column to add it as an observation of the system");
        textAddObservation.wrappingWidthProperty().set(rightWidth);
        textRemoveObservation = new Text("To remove an observation in the above list, select it and then click Button Remove");
        textRemoveObservation.wrappingWidthProperty().set(rightWidth);

        rightTitle = new Label("Data in file");
        systemTitle = new Label("Extracted observations");
        rightTitle.setFont(ColorsAndFormats.titleFont);
        systemTitle.setFont(ColorsAndFormats.titleFont);
        VBox vBoxLeft = new VBox();
        vBoxLeft.getChildren().addAll(rightTitle, tableOfDataValues, textAddObservation);
        VBox vBoxRight = new VBox();
        vBoxRight.getChildren().addAll(systemTitle, tableOfObservationsFromDataFile, textRemoveObservation, buttonRemove);
        HBox hBox = new HBox();
        hBox.getChildren().addAll(vBoxLeft,vBoxRight);
        ColorsAndFormats.setVBoxCharacteristics(vBoxLeft);
        ColorsAndFormats.setHBoxCharacteristics(hBox);
        this.setTitle("Add and remove observation from "+ selectedDataFile.getShortName());
        this.getDialogPane().setContent(hBox);
        this.getDialogPane().getButtonTypes().addAll(ButtonType.OK, ButtonType.CANCEL);
        this.getDialogPane().setStyle(ColorsAndFormats.font);

        Optional<ButtonType> result = this.showAndWait();
        if (result.isPresent()) {
            if (result.get() == ButtonType.OK) {
             }
        }
    }

    public void makeTableOfDataInFile() {
        tableOfDataValues = new TableView<>();
        tableOfDataValues.setMinWidth(leftWidth);
        if (selectedDataFile != null) {
            int nc = selectedDataFile.getNumberOfColumns();
            if (nc > 0) {
                String[][] dataValues = selectedDataFile.getFormattedDataInFile();
                List<String> namesOfColumns = selectedDataFile.getNamesColumnInFile();
                for (int col = 0; col < nc; col++) {
                    TableColumn tableColumn = new TableColumn();
                    Label colHeader = new Label(namesOfColumns.get(col));
                    colHeader.setOnMouseClicked(event -> {
                        System.out.println("Clic dans names of columns");
                        Label labs = (Label) event.getSource();
                        String colInDataFile = labs.getText();
                        System.out.println("Clic dans names  column " + colInDataFile);
                        Observation observation = selectedDataFile.makeObservationFromColumn(colInDataFile);
                        System.out.println("Added");
                        if(observation != null) {
                            tableOfObservationsFromDataFile.getItems().add(observation);
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
                    tableOfDataValues.getColumns().add(tableColumn);
                }
                listOfDataValues = observableArrayList(Arrays.asList(dataValues));
                tableOfDataValues.setItems(listOfDataValues);
            }
        }
    }

    public void makeTableExtractedObservations() {
        tableOfObservationsFromDataFile = new TableView<Observation>();
        TableColumn<Observation, String>    obsNameCol = new TableColumn<>("Name");
        obsNameCol.setCellValueFactory(new PropertyValueFactory<>("obsName"));
        obsNameCol.setEditable(false);
        obsNameCol.setPrefWidth(rightWidth);
        tableOfObservationsFromDataFile.setItems(observableArrayList(selectedDataFile.getAddedAsObservations()));
        tableOfObservationsFromDataFile.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
        tableOfObservationsFromDataFile.getColumns().add(obsNameCol);

        buttonRemove.setOnAction(
                (ActionEvent e) -> {
                    Observation observation = tableOfObservationsFromDataFile.getSelectionModel().getSelectedItem();
                    if (observation != null) {
                        selectedDataFile.removeObservationFromColumn(observation);
                        tableOfObservationsFromDataFile.getItems().remove(observation);
                        Context.setTextAreaContent("True");
                    }
                }
        );

    }

}

