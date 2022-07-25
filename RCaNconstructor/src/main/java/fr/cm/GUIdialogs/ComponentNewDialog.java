package fr.cm.GUIdialogs;

import fr.cm.canObjects.Component;
import java.util.Optional;

import fr.cm.ProjectManager.ProjectListsManager;
import fr.cm.parameters.Strings;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.geometry.Insets;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.ButtonType;
import javafx.scene.control.ChoiceBox;
import javafx.scene.control.Dialog;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.GridPane;
import javafx.stage.Window;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
/**
 *
 * @author christianmullon
 */
public class ComponentNewDialog extends Dialog<ButtonType>{

    final Window window;

    public ComponentNewDialog(double wx, double hy) {
        window = this.getDialogPane().getScene().getWindow();
        window.setOnCloseRequest(event -> window.hide());
        setTitle(Strings.addingAComponent);
        setHeaderText(Strings.nameAndTypeOfNewComponent);

        Label labelName = new Label(Strings.nameOfNewComponent);
        Label labelType = new Label(Strings.typeOfNewComponent);
        TextField fieldName = new TextField();
        ObservableList<String> items = FXCollections.observableArrayList(Strings.getTypesOfComponents());
        final ChoiceBox<String> choiceType;
        choiceType = new ChoiceBox<>(items);
        choiceType.setValue(Strings.getTypesOfComponents()[0]);
        
        GridPane gridPane = new GridPane();
        gridPane.setHgap(10);
        gridPane.setVgap(15);
        gridPane.setPadding(new Insets(10, 10, 10, 10));
        gridPane.add(labelName, 1, 1);
        gridPane.add(fieldName, 2, 1);
        gridPane.add(labelType, 1, 2);
        gridPane.add(choiceType, 2, 2);
        getDialogPane().setContent(gridPane);

        ButtonType buttonTypeOk = new ButtonType("OK", ButtonBar.ButtonData.OK_DONE);
        getDialogPane().getButtonTypes().add(buttonTypeOk);

        Optional<ButtonType> result = this.showAndWait();
        if (result.isPresent()) {
            String name = fieldName.getText();
            String sType = choiceType.getValue();
            boolean type = false;
            if(sType.equals(Strings.getTypesOfComponents()[0])){
                type = true;
            }
            if (name.length() > 0) {
                Component component = new Component();
                component.setName(name);
                component.setInside(type);
                component.setText();
                component.setWH(wx, hy);
                ProjectListsManager.addComponent(component, true);
            }
         }
    }

}
