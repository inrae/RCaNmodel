/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.GUIdialogs;

import fr.cm.RCaNMain.Context;

import java.io.File;

import fr.cm.RCaNMain.MainApplication;
import javafx.scene.control.*;
import javafx.stage.FileChooser;

/**
 * @author christianmullon
 */
public class ProjectSaveAs extends Dialog<ButtonType> {

    // --------------------------------------------
    public ProjectSaveAs() {
        FileChooser fileChooser = new FileChooser();
        fileChooser.setTitle("Name of project");
        fileChooser.setInitialDirectory(new File(Context.getDirName()));
        fileChooser.setInitialFileName(Context.getFileName());
        File selectedFile = fileChooser.showSaveDialog(MainApplication.stage);
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

