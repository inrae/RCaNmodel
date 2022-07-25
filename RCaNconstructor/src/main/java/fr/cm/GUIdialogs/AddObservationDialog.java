package fr.cm.GUIdialogs;

import fr.cm.RCaNMain.Context;
import fr.cm.canObjects.DataFile;
import fr.cm.canObjects.Observation;
import fr.cm.ProjectManager.ProjectListsManager;
import fr.cm.parameters.ColorsAndFormats;
import javafx.geometry.HPos;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Dialog;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.GridPane;
import java.util.Optional;

public class AddObservationDialog extends Dialog<ButtonType> {

    public AddObservationDialog(Observation observation, DataFile dataFile) {

        GridPane grid  = new GridPane();

        Label labelDataFile = new Label("Origin datafile");
        Label labelDataFileName = new Label(observation.getDataFileName());
        GridPane.setHalignment(labelDataFile, HPos.RIGHT);
        grid.addRow(0,labelDataFile, labelDataFileName);

        Label labelDataColumn = new Label("Column in origin datafile");
        Label labelDataColumnName = new Label(observation.getOriginalColumn());
        GridPane.setHalignment(labelDataColumn, HPos.RIGHT);
        grid.addRow(1,labelDataColumn, labelDataColumnName);

        Label labelObsName = new Label("Observation Name");
        TextField fieldObsName = new TextField();
        fieldObsName.setText(observation.getObsName());
        GridPane.setHalignment(labelObsName, HPos.RIGHT);
        grid.addRow(2,labelObsName, fieldObsName);
        
        Label labelFirstYear = new Label("First year in table");
        TextField fieldFirstYear = new TextField();
        fieldFirstYear.setText(Integer.toString(observation.getFirstYear()));
        GridPane.setHalignment(labelFirstYear, HPos.RIGHT);
        grid.addRow(3,labelFirstYear, fieldFirstYear);

        Label labelFirstYearCtxt = new Label("First year in system");
        Label labelFirstYearCtxtValue = new Label(Integer.toString(Context.getFirstYear()));
        GridPane.setHalignment(labelFirstYearCtxt, HPos.RIGHT);
        grid.addRow(4,labelFirstYearCtxt, labelFirstYearCtxtValue);

        Label labelLastYear = new Label("Last year in table");
        TextField fieldLastYear = new TextField();
        fieldLastYear.setText(Integer.toString(observation.getLastYear()));
        GridPane.setHalignment(labelLastYear, HPos.RIGHT);
        grid.addRow(5,labelLastYear, fieldLastYear);

        Label labelLastYearCtxt = new Label("Last year in system");
        Label labelLastYearCtxtValue = new Label(Integer.toString(Context.getLastYear()));
        GridPane.setHalignment(labelLastYearCtxt, HPos.RIGHT);
        grid.addRow(6,labelLastYearCtxt, labelLastYearCtxtValue);

        ColorsAndFormats.setGridCharacteristics(grid);

        this.setTitle("Observation");
        this.getDialogPane().setContent(grid);
        this.getDialogPane().getButtonTypes().addAll(ButtonType.OK, ButtonType.CANCEL);
        this.getDialogPane().setStyle(ColorsAndFormats.font);

        Optional<ButtonType> result = this.showAndWait();
        if (result.isPresent()) {
            if (result.get() == ButtonType.OK) {
                try{
                    observation.setObsName(fieldObsName.getText());
                    int fY = Integer.parseInt(fieldFirstYear.getText());
                    int lY = Integer.parseInt(fieldLastYear.getText());
                    observation.redimValues(fY, lY);
                    ProjectListsManager.addObservation(observation, true);
                    dataFile.addObservationFromColumn(observation);
                }
                catch(Exception ex){

                }
            }
        }
    }
    // --------------------------------------------
}
