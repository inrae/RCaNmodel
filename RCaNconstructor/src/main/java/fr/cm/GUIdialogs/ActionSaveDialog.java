
package fr.cm.GUIdialogs;

import fr.cm.canObjects.*;
import fr.cm.RCaNMain.Context;
import fr.cm.RCaNMain.MainApplication;
import javafx.stage.FileChooser;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

public class ActionSaveDialog {

    // --------------------------------------------
    public ActionSaveDialog() {

        StringBuilder sB = new StringBuilder("PROJECT TRACKING");
        sB.append("\n");
        List<Action> listOfActions = ProjectListsManager.getListOfActions() ;


        for (Action action : listOfActions) {
            sB.append(action.getDate());
            sB.append("\t");
            sB.append(action.getWhichAction());
            sB.append("\t");
            sB.append(action.getCommentAuthor());
            sB.append("\n");
        }

        String text =  sB.toString();

        FileChooser fileChooser = new FileChooser();
        fileChooser.setTitle("Name of text file");
        fileChooser.setInitialDirectory(new File(Context.getDirName()));
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
