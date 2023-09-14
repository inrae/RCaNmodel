package fr.cm.excel;

import fr.cm.Main.Context;
import fr.cm.objects.MetaElement;
import fr.cm.Main.ObjectsManager;
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

        List<MetaElement> elements = ObjectsManager.getListOfMetaElements();
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
        ObjectsManager.makeMetaElementsList();
        List<MetaElement> elements = ObjectsManager.getListOfMetaElements();
        try {
            for (int r = 0; r < elements.size(); r++) {
                row = sheet.getRow(r + 2);
                cell = row.getCell(1);
                String content = cell.getStringCellValue();
                elements.get(r).setMetaContent(content);
            }
            ObjectsManager.setListOfMetaElements(elements);
        } catch (Exception e) {
            ObjectsManager.makeMetaElementsList();
        }
    }




}
