package fr.cm.objects;

import fr.cm.Main.Context;
import fr.cm.project.ProjectListsManager;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;

import java.util.List;

public class MetaExcel {
    public static void saveExcelMetaInformation(Workbook workbook) {
        Sheet sheet;
        Row row;
        Cell cell;
        sheet = workbook.createSheet("INFO");
        row = sheet.createRow(0);
        cell = row.createCell(0);
        cell.setCellValue("PathName");
        cell = row.createCell(1);
        cell.setCellValue(Context.getFullFileName());
        row = sheet.createRow(1);
        cell = row.createCell(0);
        cell.setCellValue("Caracteristic");
        cell = row.createCell(1);
        cell.setCellValue("Value");

        List<MetaElement> elements = ProjectListsManager.getListOfMetaElements();
        for (int c = 0; c < elements.size(); c++) {
            MetaElement element = elements.get(c);
            row = sheet.createRow(c + 2);
            cell = row.createCell(0);
            cell.setCellValue(element.getMetaName());
            cell = row.createCell(1);
            cell.setCellValue(element.getMetaContent());
            cell = row.createCell(2);
            cell.setCellValue(element.getMetaHint());
        }
    }

    // -------------------------------------------------------------------------
    public static void getExcelMetaInformation(Workbook workbook) {
        Sheet sheet = workbook.getSheet("INFO");
        Row row;
        Cell cell;
        ProjectListsManager.makeMetaElementsList();
        List<MetaElement> elements = ProjectListsManager.getListOfMetaElements();
        try {
            for (int r = 0; r < elements.size(); r++) {
                row = sheet.getRow(r + 2);
                cell = row.getCell(1);
                String content = cell.getStringCellValue();
                elements.get(r).setMetaContent(content);
            }
            ProjectListsManager.setListOfMetaElements(elements);
        } catch (Exception e) {
            ProjectListsManager.makeMetaElementsList();
        }
    }




}
