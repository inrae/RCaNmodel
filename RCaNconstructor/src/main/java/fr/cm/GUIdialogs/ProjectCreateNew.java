/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.GUIdialogs;

import java.util.Optional;

import fr.cm.RCaNMain.Context;
import fr.cm.parameters.ColorsAndFormats;
import javafx.scene.control.*;
import javafx.scene.layout.GridPane;

/**
 * @author christianmullon
 */
public class ProjectCreateNew extends Dialog<ButtonType> {

    // --------------------------------------------
    public ProjectCreateNew() {

        TextField textFileName = new TextField();
        textFileName.setMinWidth(300);

        GridPane grid = new GridPane();
        grid.setHgap(20);
        grid.setVgap(20);
        grid.add(textFileName, 1, 1);

        this.setTitle("Name of new project");
        this.getDialogPane().setContent(grid);
        this.getDialogPane().getButtonTypes().addAll(ButtonType.OK, ButtonType.CANCEL);
        this.getDialogPane().setStyle(ColorsAndFormats.font);

        Optional<ButtonType> result = this.showAndWait();
        if (result.isPresent()) {
            if (result.get() == ButtonType.OK) {
                String fileName = textFileName.getText();
                if(fileName.length()>0) {
                    if (!fileName.contains(".xlsx")) {
                        fileName = fileName + ".xlsx";
                    }
                }
                Context.init();
                Context.setFileName(fileName);
                Context.setDirName("");
                Context.setStarted(true);
            }
        }
    }
    // --------------------------------------------
}
