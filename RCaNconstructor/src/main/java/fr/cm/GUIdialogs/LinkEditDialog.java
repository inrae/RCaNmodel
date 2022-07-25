/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.GUIdialogs;

import fr.cm.canObjects.Flux;
import fr.cm.ProjectManager.ProjectListsManager;
import fr.cm.parameters.Strings;
import javafx.geometry.Insets;
import javafx.scene.control.*;
import javafx.scene.layout.GridPane;
import javafx.stage.Window;

import java.util.Optional;

/**
 * @author christianmullon
 */
public class LinkEditDialog extends Dialog<ButtonType> {
    final Window window;

    public LinkEditDialog(Flux flux) {
        window = this.getDialogPane().getScene().getWindow();
        window.setOnCloseRequest(event -> window.hide());
        setTitle("Link");
        setHeaderText(flux.getName());

        ToggleGroup toggleGroup = new ToggleGroup();
        ToggleButton buttonTrophic = new ToggleButton(Strings.getTypesOfLinks()[0]);
        ToggleButton buttonNonTrophic =  new ToggleButton(Strings.getTypesOfLinks()[1]);
        buttonTrophic.setToggleGroup(toggleGroup);
        buttonNonTrophic.setToggleGroup(toggleGroup);
        buttonTrophic.setSelected(flux.isTypeTrophic());
        buttonNonTrophic.setSelected( ! flux.isTypeTrophic());
        buttonTrophic.setMinWidth(100);
        buttonNonTrophic.setMinWidth(100);

        GridPane gridPane = new GridPane();
        gridPane.setHgap(10);
        gridPane.setVgap(10);
        gridPane.setPadding(new Insets(10, 10, 10, 10));
        gridPane.add(buttonTrophic, 1, 1);
        gridPane.add(buttonNonTrophic, 2, 1);

        getDialogPane().setContent(gridPane);
        ButtonType buttonTypeOk = new ButtonType("OK", ButtonBar.ButtonData.OK_DONE);
        ButtonType buttonTypeDelete = new ButtonType("Delete", ButtonBar.ButtonData.OK_DONE);
        ButtonType buttonTypeCancel = new ButtonType("Cancel", ButtonBar.ButtonData.OK_DONE);
        getDialogPane().getButtonTypes().add(buttonTypeOk);
        getDialogPane().getButtonTypes().add(buttonTypeDelete);
        getDialogPane().getButtonTypes().add(buttonTypeCancel);

        Optional<ButtonType> result = this.showAndWait();
        if (result.isPresent()) {
            if (result.get() == buttonTypeOk) {
                 flux.setTypeTrophic(buttonTrophic.isSelected());
            }
            if (result.get() == buttonTypeDelete) {
                ProjectListsManager.removeLink(flux);
            }
        }
    }
}
