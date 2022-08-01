package fr.cm.excel;

import fr.cm.dialogs.HelpDialog;
import fr.cm.Main.Context;
import fr.cm.objects.ActionExcel;
import fr.cm.objects.ComponentExcel;
import fr.cm.objects.ConstraintExcel;
import fr.cm.objects.DataFileExcel;
import fr.cm.objects.FluxExcel;
import fr.cm.objects.MetaExcel;
import fr.cm.objects.ObservationExcel;
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
            ActionExcel.saveExcelFileActions(workbook);
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
            ActionExcel.getExcelFileActions(workbook);
        } catch (FileNotFoundException ex) {
            HelpDialog.warning("File not found","Warning", ex);
        } catch (IOException ex) {
            HelpDialog.warning("IO problem","Warning", ex);
        }
    }

    // -------------------------------------------------------------------------


    // -------------------------------------------------------------------------
     // ---------------------------------------------------------------------

    // -------------------------------------------------------------------------

    // -------------------------------------------------------------------------


}
