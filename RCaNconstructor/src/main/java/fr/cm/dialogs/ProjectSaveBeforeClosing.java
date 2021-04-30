package fr.cm.dialogs;

import fr.cm.canObjects.ListsManager;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Dialog;

import java.util.Optional;

public class ProjectSaveBeforeClosing extends Dialog<ButtonType> {

    public boolean answer;
    public ProjectSaveBeforeClosing() {
        answer = false;
        this.setTitle("Changes have been done. Do you want to save the project?");
        this.getDialogPane().getButtonTypes().addAll(ButtonType.YES, ButtonType.NO, ButtonType.CANCEL);
        this.getDialogPane().setMinWidth(500);
        Optional<ButtonType> result = this.showAndWait();
        if (result.isPresent()) {
            if (result.get() == ButtonType.YES) {
                ListsManager.saveExcel();
                answer = true;
            }
            if (result.get() == ButtonType.NO) {
                answer = true;
            }
        }
    }
}
