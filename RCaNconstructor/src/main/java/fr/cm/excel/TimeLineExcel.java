package fr.cm.excel;

import fr.cm.dialogs.HelpDialog;
import fr.cm.objects.TimeLine;
import fr.cm.project.ProjectListsManager;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;

import java.util.Iterator;
import java.util.List;

public class TimeLineExcel {

    public static void saveExcelFileTimeLines(Workbook workbook) {
        Sheet sheet;
        Row row;
        Cell cell;
        sheet = workbook.createSheet("TimeLines");

        row = sheet.createRow(0);
        cell = row.createCell(0);
        cell.setCellValue("Date");
        cell = row.createCell(1);
        cell.setCellValue("Task");
        cell = row.createCell(2);
        cell.setCellValue("Annotation");

        List<TimeLine> listOfTimeLines = ProjectListsManager.getListOfTimeLines();
        int c = 0;
        for (TimeLine timeLine : listOfTimeLines) {
            c++;
            row = sheet.createRow(c + 1);
            cell = row.createCell(0);
            cell.setCellValue(timeLine.getDate());
            cell = row.createCell(1);
            cell.setCellValue(timeLine.getWhichTimeLines());
            cell = row.createCell(2);
            cell.setCellValue(timeLine.getCommentAuthor());
        }
    }

    // -------------------------------------------------------------------------
    public static void getExcelFileTimeLines(Workbook workbook) {
        try {
            ProjectListsManager.initListOfTimeLines();
            Sheet sheet = workbook.getSheet("TimeLines");
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
                TimeLine newTimeLine = new TimeLine(date, comment, commentAuthor);
                ProjectListsManager.addTimeLine(newTimeLine, false);
            }
        } catch (Exception ex) {
            HelpDialog.warning("Sheet TimeLine does not exist", "Warning ", ex);
        }
    }
}
