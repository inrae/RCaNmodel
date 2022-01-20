/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.GUIdialogs;

import fr.cm.RCaNMain.Context;
import fr.cm.RCaNMain.MainApplication;

import java.io.File;

import javafx.scene.control.*;
import javafx.stage.FileChooser;

/**
 * @author christianmullon
 */
public class ProjectOpenExisting extends Dialog<ButtonType> {

    // --------------------------------------------
    public ProjectOpenExisting() {
        FileChooser fileChooser = new FileChooser();
        fileChooser.setTitle("Select file of existing project");
        fileChooser.setInitialDirectory(Context.getWorkingDirectory());
        FileChooser.ExtensionFilter extFilter = new FileChooser.ExtensionFilter("Excel files", "*.xlsx");
        fileChooser.getExtensionFilters().add(extFilter);
        File selectedFile = fileChooser.showOpenDialog(MainApplication.stage);
        if (selectedFile != null) {
            String fileName = selectedFile.getName();
            if ((fileName.length() > 0) && ! fileName.equals("...")) {
                String dirName = selectedFile.getParent();
                if (!fileName.contains(".xlsx")) fileName = fileName + ".xlsx";
                Context.init();
                Context.setFileName(fileName);
                Context.setDirName(dirName);
                Context.setStarted(true);
            }
        }
    }
    // --------------------------------------------

}

