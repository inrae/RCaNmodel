package fr.cm.dialogs;

import fr.cm.canObjects.*;
import fr.cm.menus.Context;
import fr.cm.menus.MainApplication;
import javafx.stage.FileChooser;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

public class MetaInformationSave {

    public MetaInformationSave() {

        StringBuilder sB = new StringBuilder("PROJECT");
        sB.append("\n");

        for (MetaElement metaElement : MetaInformation.getElements()) {
            sB.append(metaElement.getMetaName().toUpperCase());
            sB.append("\n      ");
            sB.append(metaElement.getMetaContentProperty());
            sB.append("\n");
        }

        sB.append("\n");
        sB.append("\n");
        sB.append("DATA FILES");
        sB.append("\n");

        List<ExternalDataFile> listOfExternalDataFiles = ListsManager.getListOfExternalDataFiles() ;
        for (ExternalDataFile externalDataFile : listOfExternalDataFiles) {
            sB.append(externalDataFile.getFileName().toUpperCase());
            sB.append("\n    ");
            sB.append(externalDataFile.getMetaInformationAboutDataFile());
            sB.append("\n");
        }

        sB.append("\n");
        sB.append("\n");
        sB.append("CONSTRAINTS");
        sB.append("\n");

        List<Constraint> listOfConstraint = ListsManager.getListOfConstraints() ;
        for ( Constraint constraint : listOfConstraint) {
            if(constraint.isActive()) {
                sB.append(constraint.getName().toUpperCase());
                sB.append("\n    ");
                sB.append(constraint.getFormula());
                sB.append("\n    ");
                sB.append("Active: ");
                sB.append(constraint.getComment());
                sB.append("\n");
            }
        }

        String text =  sB.toString();

        FileChooser fileChooser = new FileChooser();
        fileChooser.setTitle("Name of text file");
        fileChooser.setInitialDirectory(Context.getWorkingDirectory());
        String fileNameTxt = Context.getFileName().replace(".xlsx",".txt");
        fileChooser.setInitialFileName(fileNameTxt);
        File selectedFile = fileChooser.showSaveDialog(MainApplication.stage);
        if (selectedFile != null) {
            try {
                FileWriter fileWriter = new FileWriter(selectedFile);
                fileWriter.write(text);
                fileWriter.close() ;
            } catch (IOException ioException) {
                ioException.printStackTrace();
            }
        }
    }
}
