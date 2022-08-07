package fr.cm.GUIdialogs;

import fr.cm.canObjects.*;
import fr.cm.RCaNMain.Context;
import fr.cm.RCaNMain.MainApplication;
import javafx.stage.FileChooser;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

public class MetaInformationSaveDialog {

    // --------------------------------------------
    public MetaInformationSaveDialog() {

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

        List<DataFile> listOfDataFiles = ProjectListsManager.getListOfDataFiles() ;
        for (DataFile dataFile : listOfDataFiles) {
            sB.append(dataFile.getShortName().toUpperCase());
            sB.append("\n    ");
            sB.append(dataFile.getMetaInformationAboutDataFile());
            sB.append("\n");
        }

        sB.append("\n");
        sB.append("\n");
        sB.append("CONSTRAINTS");
        sB.append("\n");

        List<Constraint> listOfConstraint = ProjectListsManager.getListOfConstraints() ;
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
                HelpDialog.warning("Problem in writing file","Warning", ioException);
            }
        }
    }
    // --------------------------------------------
}