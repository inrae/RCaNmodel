package fr.cm.objects;

import fr.cm.Main.Context;
import fr.cm.preferences.ColorsAndFormats;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.layout.*;
import javafx.scene.text.Text;
import javafx.util.Callback;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;

import static javafx.collections.FXCollections.observableArrayList;

public class DataFileAddObservationDialog extends Dialog <ButtonType> {

    double width = Context.getWindowWidth();
    double height =  Context.getWindowHeight();
    DataFile selectedDataFile = null;
    // DROITE
    Label rightTitle,  systemTitle;
    TableView<String[]> tableOfDataValues ;
    TableView<Observation> tableOfOfAlreadyExtractedObservations;
    ObservableList<String[]> listOfDataValues;
    Text textAddObservation, textRemoveObservation;
    Button buttonRemove = new Button("Remove selected observation");

    int rightWidth = (int)(0.3 * width);
    int leftWidth = (int)(0.7 * width);

    public DataFileAddObservationDialog(DataFile selectedDataFile){
        this.selectedDataFile = selectedDataFile;

        makeTableOfDataInFile();
        makeTableOfAlreadyExtractedObservations();

        textAddObservation = new Text("Click on the header of a column to add it as an observation of the system");
        textAddObservation.wrappingWidthProperty().set(rightWidth);
        textRemoveObservation = new Text("To remove an observation in the above list, select it and then click button Remove");
        textRemoveObservation.wrappingWidthProperty().set(rightWidth);

        rightTitle = new Label("Data in file");
        systemTitle = new Label("Extracted observations");
        rightTitle.setFont(ColorsAndFormats.titleFont);
        systemTitle.setFont(ColorsAndFormats.titleFont);
        VBox vBoxLeft = new VBox();
        vBoxLeft.getChildren().addAll(rightTitle, tableOfDataValues, textAddObservation);
        vBoxLeft.setSpacing(20);
        VBox vBoxRight = new VBox();
        vBoxRight.getChildren().addAll(systemTitle, tableOfOfAlreadyExtractedObservations, textRemoveObservation, buttonRemove);
        vBoxRight.setSpacing(20);
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
                    tableColumn.setMinWidth(100);
                    Label colHeader = new Label(namesOfColumns.get(col));
                    if(col >0){
                        colHeader.setOnMouseClicked(event -> {
                            Label labs = (Label) event.getSource();
                            String colInDataFile = labs.getText();
                            Observation observation = selectedDataFile.makeObservationFromColumn(colInDataFile);
                            if(observation != null) {
                                if( ! tableOfOfAlreadyExtractedObservations.getItems().contains(observation)) {
                                    tableOfOfAlreadyExtractedObservations.getItems().add(observation);
                                }
                            }
                        });
                    }
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

    public void makeTableOfAlreadyExtractedObservations() {
        tableOfOfAlreadyExtractedObservations = new TableView<Observation>();
        TableColumn<Observation, String>    obsNameCol = new TableColumn<>("Name");
        obsNameCol.setCellValueFactory(new PropertyValueFactory<>("obsName"));
        obsNameCol.setEditable(false);
        obsNameCol.setPrefWidth(rightWidth);
        tableOfOfAlreadyExtractedObservations.setItems(observableArrayList(selectedDataFile.getAddedAsObservations()));
        tableOfOfAlreadyExtractedObservations.getSelectionModel().setSelectionMode(SelectionMode.SINGLE);
        tableOfOfAlreadyExtractedObservations.getColumns().add(obsNameCol);

        buttonRemove.setOnAction(
                (ActionEvent e) -> {
                    Observation observation = tableOfOfAlreadyExtractedObservations.getSelectionModel().getSelectedItem();
                    if (observation != null) {
                        selectedDataFile.removeAddedObservation(observation);
                        tableOfOfAlreadyExtractedObservations.getItems().remove(observation);
                        Context.setTextAreaContent("True");
                    }
                }
        );

    }

}

