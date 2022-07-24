package fr.cm.GUIdialogs;

import fr.cm.RCaNMain.Context;
import fr.cm.parameters.ColorsAndFormats;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Dialog;
import javafx.scene.control.TextArea;
import javafx.scene.layout.GridPane;

import java.util.Optional;

public class TextAreaDialog  extends Dialog<ButtonType> {

    // --------------------------------------------
    String content;
    public TextAreaDialog(String title, String content) {
        this.content = content;
        TextArea textFileName = new TextArea(content);
        textFileName.setMinWidth(500);
        textFileName.setMinHeight(300);

        GridPane grid = new GridPane();
        grid.setHgap(20);
        grid.setVgap(20);
        grid.add(textFileName, 1, 1);

        this.setTitle(title);
        this.getDialogPane().setContent(grid);
        this.getDialogPane().getButtonTypes().addAll(ButtonType.OK, ButtonType.CANCEL);
        this.getDialogPane().setStyle(ColorsAndFormats.font);

        Optional<ButtonType> result = this.showAndWait();
        if (result.isPresent()) {
            if (result.get() == ButtonType.OK) {
                content = textFileName.getText();
                Context.setTextAreaContent(content);
            }
        }
    }
    // --------------------------------------------
}