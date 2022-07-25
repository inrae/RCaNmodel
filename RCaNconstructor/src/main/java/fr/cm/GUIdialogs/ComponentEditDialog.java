package fr.cm.GUIdialogs;

import fr.cm.canObjects.Component;
import fr.cm.ProjectManager.ProjectListsManager;
import fr.cm.parameters.Strings;
import javafx.geometry.Insets;
import javafx.scene.control.*;
import javafx.scene.input.KeyCode;
import javafx.scene.layout.GridPane;
import javafx.stage.Window;

import java.util.Optional;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 * @author christianmullon
 */

public class ComponentEditDialog extends Dialog<ButtonType> {

    final TextField[] fieldCaracteristiques;
    final Window window;

    public ComponentEditDialog(Component component) {
        window = this.getDialogPane().getScene().getWindow();
        window.setOnCloseRequest(event -> window.hide());
        this.setTitle(Strings.component);
        this.setHeaderText(component.getName());

        ToggleGroup toggleGroup = new ToggleGroup();
        ToggleButton buttonIn = new ToggleButton(Strings.getTypesOfComponents()[0]);
        ToggleButton buttonOut =  new ToggleButton(Strings.getTypesOfComponents()[1]);
        buttonIn.setToggleGroup(toggleGroup);
        buttonOut.setToggleGroup(toggleGroup);
        boolean in = component.isInside();
        buttonIn.setSelected(in);
        buttonOut.setSelected( ! in);
        buttonIn.setMinWidth(100);
        buttonOut.setMinWidth(100);

        String[] charactersNames = Strings.getParametersNames();
        int nc = charactersNames.length;
        Label[] labelCaracteristiques = new Label[nc];
        fieldCaracteristiques = new TextField[nc];

        GridPane gridPane = new GridPane();
        gridPane.setHgap(10);
        gridPane.setVgap(10);
        gridPane.setPadding(new Insets(10, 10, 10, 10));
        gridPane.add(buttonIn, 1, 1);
        gridPane.add(buttonOut, 2, 1);
        for (int c = 0; c < nc; c++) {
            final int cc = c;
            labelCaracteristiques[c] = new Label(Strings.getParametersNames(c));
            fieldCaracteristiques[c] = new TextField();
            fieldCaracteristiques[c].setOnKeyReleased(event -> {
                if (event.getCode() == KeyCode.ENTER) {
                    double vmax = Strings.getParametersMaxValues()[cc];
                    double val = 0.0f;
                    try {
                        val = Double.parseDouble(fieldCaracteristiques[cc].getText());
                    } catch (NumberFormatException e) {
                        HelpDialog.warning("numberFormatIssue","Warning", e);
                        fieldCaracteristiques[cc].setText(Double.toString(component.getParameters()[cc]));
                    }
                    if ((vmax > 0.0 && val >= 0.0 && val <= vmax) || (vmax < 0.0 && val >= 0.0)) {
                        String sval = String.format("%.4f", val);
                        fieldCaracteristiques[cc].setText(sval);
                    } else {
                        HelpDialog.warning("valueOutOfRange","Warning");
                        fieldCaracteristiques[cc].setText(Double.toString(component.getParameters()[cc]));

                    }
                }
            });
            fieldCaracteristiques[c].setText(Double.toString(component.getParameters()[c]));
            gridPane.add(labelCaracteristiques[c], 1, c + 2);
            gridPane.add(fieldCaracteristiques[c], 2, c + 2);
        }

        this.getDialogPane().setContent(gridPane);
        ButtonType buttonTypeOk = new ButtonType("OK", ButtonBar.ButtonData.OK_DONE);
        ButtonType buttonTypeDelete = new ButtonType("Delete", ButtonBar.ButtonData.OK_DONE);
        ButtonType buttonTypeCancel = new ButtonType("Cancel", ButtonBar.ButtonData.OK_DONE);
        this.getDialogPane().getButtonTypes().add(buttonTypeOk);
        this.getDialogPane().getButtonTypes().add(buttonTypeDelete);
        this.getDialogPane().getButtonTypes().add(buttonTypeCancel);

        Optional<ButtonType> result = this.showAndWait();
        if (result.isPresent()) {
            if (result.get() == buttonTypeOk) {
                component.setInside(buttonIn.isSelected());
                double[] parameters = new double[nc];
                for (int c = 0; c < nc; c++) {
                    labelCaracteristiques[c] = new Label(Strings.getParametersNames(c));
                    parameters[c] = Double.parseDouble(fieldCaracteristiques[c].getText());
                    component.setParameters(parameters);
                }
            }
            if (result.get() == buttonTypeDelete) {
                ProjectListsManager.removeComponent(component);
            }
        }
    }


}
