package fr.cm.excel;

import fr.cm.dialogs.HelpDialog;
import fr.cm.Main.Context;

import org.apache.poi.ss.usermodel.*;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

/**
 * @author christianmullon
 */
public class ExcelManager {
    // ---------------------------------
    public static void saveExcel() {
        String fileName = Context.getFullFileName();
        try {
            Workbook workbook = WorkbookFactory.create(true);
            MetaExcel.saveExcelMetaInformation(workbook);
            ComponentExcel.saveExcelComponents(workbook);
            FluxExcel.saveExcelLinks(workbook);
            ConstraintExcel.saveExcelConstraints(workbook);
            ObservationExcel.saveExcelObservations(workbook);
            DataFileExcel.saveExcelFileWithObservation(workbook);
            TimeLineExcel.saveExcelFileTimeLines(workbook);
            FileOutputStream outputStream = new FileOutputStream(fileName);
            workbook.write(outputStream);
        } catch (FileNotFoundException ex) {
            HelpDialog.warning("File not found","Warning", ex);
        } catch (IOException ex) {
            HelpDialog.warning("IO problem","Warning", ex);
        }
    }

    // -------------------------------------------------------------------------
    public static void getExcel() {
        String fileName = Context.getFullFileName();
        try {
            FileInputStream inputStream = new FileInputStream(fileName);
            Workbook workbook = WorkbookFactory.create(inputStream);
            MetaExcel.getExcelMetaInformation(workbook);
            ComponentExcel.getExcelComponents(workbook);
            FluxExcel.getExcelLinks(workbook);
            ConstraintExcel.getExcelConstraints(workbook);
            ObservationExcel.getExcelObservations(workbook);
            DataFileExcel.getExcelFileWithObservation(workbook);
            TimeLineExcel.getExcelFileTimeLines(workbook);
        } catch (FileNotFoundException ex) {
            HelpDialog.warning("File not found","Warning", ex);
        } catch (IOException ex) {
            HelpDialog.warning("IO problem","Warning", ex);
        }
    }

    // -------------------------------------------------------------------------


}
