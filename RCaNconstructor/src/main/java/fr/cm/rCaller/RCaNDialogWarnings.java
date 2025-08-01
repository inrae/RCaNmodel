package fr.cm.rCaller;

import fr.cm.preferences.ColorsAndFormats;
import javafx.scene.control.Alert;
import javafx.scene.control.TextArea;

import java.io.PrintWriter;
import java.io.StringWriter;

public class RCaNDialogWarnings {

    // --------------------------------------------
    public RCaNDialogWarnings(String title, String header, Exception ex) {
        Alert alert = new Alert(Alert.AlertType.INFORMATION);
        alert.getDialogPane().setStyle(ColorsAndFormats.font);
        String content = "No error";
        if(ex != null) {
            StringWriter sw = new StringWriter();
            PrintWriter pw = new PrintWriter(sw);
            ex.printStackTrace(pw);
            content = sw.toString();
        }
        TextArea area = new TextArea(content);
        area.setWrapText(true);
        area.setEditable(false);
        alert.setTitle(title);
        alert.setHeaderText(header);
        alert.getDialogPane().setContent(area);
        alert.setResizable(true);
        alert.getDialogPane().setPrefSize(800.0, 400.0);
        alert.showAndWait();
    }
    // --------------------------------------------

}
