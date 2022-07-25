package fr.cm.GUIdialogs;

import fr.cm.GUInetwork.NetworkView;
import fr.cm.canObjects.Component;
import fr.cm.ProjectManager.ProjectListsManager;

import java.util.Optional;

import fr.cm.parameters.Strings;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Dialog;
import javafx.stage.Window;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
/**
 * @author christianmullon
 */
public class ComponentClicDialog extends Dialog<ButtonType> {

    final Window window;

    public ComponentClicDialog(Component component, NetworkView networkView) {
        window = this.getDialogPane().getScene().getWindow();
        window.setOnCloseRequest(event -> window.hide());
        setTitle(component.getName());
        setHeaderText(Strings.addLinkHint);

        ButtonType buttonTypeEdit = new ButtonType(Strings.editComponent, ButtonBar.ButtonData.OK_DONE);
        ButtonType buttonTypeDelete = new ButtonType(Strings.deleteComponent, ButtonBar.ButtonData.OK_DONE);
        ButtonType buttonTypeNewTrophicLink = new ButtonType(Strings.newTrophicLink, ButtonBar.ButtonData.OK_DONE);
        ButtonType buttonTypeNewNonTrophicLink = new ButtonType(Strings.newNonTrophicLink, ButtonBar.ButtonData.OK_DONE);
        ButtonType buttonTypeCancel = new ButtonType(Strings.cancel, ButtonBar.ButtonData.OK_DONE);

        getDialogPane().getButtonTypes().addAll(buttonTypeEdit,buttonTypeDelete,buttonTypeNewTrophicLink,buttonTypeNewNonTrophicLink,buttonTypeCancel);

        Optional<ButtonType> result = this.showAndWait();
        if (result.isPresent()) {
            if (result.get() == buttonTypeEdit) {
                new ComponentEditDialog(component);
            }
            if (result.get() == buttonTypeDelete) {
                ProjectListsManager.removeComponent(component);
            }
            if (result.get() == buttonTypeNewTrophicLink) {
                networkView.setAddingTrophicLink(true);
            }
            if (result.get() == buttonTypeNewNonTrophicLink) {
                networkView.setAddingNonTrophicLink(true);
            }
        }
    }

}
