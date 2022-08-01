package fr.cm.objects;

import fr.cm.dialogs.HelpDialog;
import fr.cm.project.ProjectListsManager;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;

import java.util.Iterator;
import java.util.List;

public class ActionExcel {

    public static void saveExcelFileActions(Workbook workbook) {
        Sheet sheet;
        Row row;
        Cell cell;
        sheet = workbook.createSheet("Actions");

        row = sheet.createRow(0);
        cell = row.createCell(0);
        cell.setCellValue("Date");
        cell = row.createCell(1);
        cell.setCellValue("Task");
        cell = row.createCell(2);
        cell.setCellValue("Annotation");

        List<Action> listOfActions = ProjectListsManager.getListOfActions();
        int c = 0;
        for (Action action : listOfActions) {
            c++;
            row = sheet.createRow(c + 1);
            cell = row.createCell(0);
            cell.setCellValue(action.getDate());
            cell = row.createCell(1);
            cell.setCellValue(action.getWhichAction());
            cell = row.createCell(2);
            cell.setCellValue(action.getCommentAuthor());
        }
    }

    // -------------------------------------------------------------------------
    public static void getExcelFileActions(Workbook workbook) {
        try {
            ProjectListsManager.initListOfActions();
            Sheet sheet = workbook.getSheet("Actions");
            Row row;
            Cell cell;
            Iterator<Row> iterator = sheet.iterator();
            iterator.next();
            while (iterator.hasNext()) {
                row = iterator.next();
                cell = row.getCell(0);
                String date = cell.getStringCellValue();
                cell = row.getCell(1);
                String comment = cell.getStringCellValue();
                cell = row.getCell(2);
                String commentAuthor = cell.getStringCellValue();
                Action newAction = new Action(date, comment, commentAuthor);
                ProjectListsManager.addAction(newAction, false);
            }
        } catch (Exception ex) {
            HelpDialog.warning("Sheet Action does not exist", "Warning ", ex);
        }
    }
}
