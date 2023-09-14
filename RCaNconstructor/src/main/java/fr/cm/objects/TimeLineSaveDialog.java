
package fr.cm.objects;

import fr.cm.dialogs.HelpDialog;
import fr.cm.Main.ObjectsManager;
import fr.cm.Main.Context;
import fr.cm.Main.MainApplication;
import javafx.stage.FileChooser;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

public class TimeLineSaveDialog {
    // --------------------------------------------
    public TimeLineSaveDialog() {

        StringBuilder sB = new StringBuilder("PROJECT TRACKING");
        sB.append("\n");
        List<TimeLine> listOfTimeLines = ObjectsManager.getListOfTimeLines() ;


        for (TimeLine timeLine : listOfTimeLines) {
            sB.append(timeLine.getDate());
            sB.append("\t");
            sB.append(timeLine.getWhichTimeLines());
            sB.append("\t");
            sB.append(timeLine.getCommentAuthor());
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
