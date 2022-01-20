package fr.cm.GUIdialogs;

import fr.cm.canObjects.ProjectListsManager;
import fr.cm.parameters.ColorsAndFormats;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Dialog;

import java.util.Optional;

public class ProjectSaveBeforeClosing extends Dialog<ButtonType> {

    // --------------------------------------------
    public ProjectSaveBeforeClosing() {
        this.setTitle("Changes have been done. Do you want to save the project?");
        this.getDialogPane().getButtonTypes().addAll(ButtonType.YES, ButtonType.NO);
        this.getDialogPane().setMinWidth(500);
        this.getDialogPane().setStyle(ColorsAndFormats.font);
        Optional<ButtonType> result = this.showAndWait();
        if (result.isPresent()) {
            if (result.get() == ButtonType.YES) {
                ProjectListsManager.saveExcel();
            }
        }
    }
    // --------------------------------------------

}
