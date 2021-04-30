/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.dialogs;

import fr.cm.menus.Context;

import java.io.File;

import fr.cm.menus.MainApplication;
import javafx.scene.control.*;
import javafx.stage.FileChooser;

/**
 * @author christianmullon
 */
public class ProjectChangeFileName extends Dialog<ButtonType> {

    public ProjectChangeFileName() {
        String prevFileName = Context.getFileName();
        FileChooser fileChooser = new FileChooser();
        fileChooser.setTitle("Name of project");
        fileChooser.setInitialDirectory(Context.getWorkingDirectory());
        fileChooser.setInitialFileName(prevFileName);
        FileChooser.ExtensionFilter extFilter = new FileChooser.ExtensionFilter("Excel files", "*.xlsx");
        fileChooser.getExtensionFilters().add(extFilter);
        File selectedFile = fileChooser.showSaveDialog(MainApplication.stage);
        if (selectedFile != null) {
            String fileName = selectedFile.getName();
            if ((fileName.length() > 0) && ! fileName.equals("...")) {
                String dirName = selectedFile.getParent();
                if (!fileName.contains(".xlsx")) fileName = fileName + ".xlsx";
                Context.setFileName(fileName);
                Context.setDirName(dirName);
                Context.setStarted(true);
            }
        }
    }
}

