package fr.cm.objects;

import fr.cm.dialogs.HelpDialog;
import fr.cm.project.ProjectListsManager;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;

import java.util.Iterator;

public class DataFileExcel {

    public static void saveExcelFileWithObservation(Workbook workbook) {
        Sheet sheet;
        Row row;
        Cell cell;
        sheet = workbook.createSheet("FileWithObservation");
        int lig = 0;
        row = sheet.createRow(lig);
        cell = row.createCell(0);
        cell.setCellValue("Id");
        cell = row.createCell(1);
        cell.setCellValue("File name");
        cell = row.createCell(2);
        cell.setCellValue("Full file name");
        cell = row.createCell(3);
        cell.setCellValue("Meta information");
        cell = row.createCell(4);
        cell.setCellValue("Owner");
        cell = row.createCell(5);
        cell.setCellValue("Selected columns");
        lig++;
        for (int c = 0; c < ProjectListsManager.getListOfDataFiles().size(); c++) {
            DataFile dataFile = ProjectListsManager.getListOfDataFiles().get(c);
            row = sheet.createRow(lig);
            cell = row.createCell(0);
            cell.setCellValue(dataFile.getId());
            cell = row.createCell(1);
            cell.setCellValue(dataFile.getShortName());
            cell = row.createCell(2);
            cell.setCellValue(dataFile.getFullFileName());
            cell = row.createCell(3);
            cell.setCellValue(dataFile.getMetaInformation());
            cell = row.createCell(4);
            cell.setCellValue(dataFile.getOwner());
            cell = row.createCell(5);
            cell.setCellValue(dataFile.codeAddedObservations());
            lig++;
        }
    }

    // -------------------------------------------------------------------------
    public static void getExcelFileWithObservation(Workbook workbook) {
        try {
            Sheet sheet = workbook.getSheet("FileWithObservation");
            Row row;
            Cell cell;
            Iterator<Row> iterator = sheet.iterator();
            iterator.next();
            while (iterator.hasNext()) {
                row = iterator.next();
                cell = row.getCell(0);
                String id = cell.getStringCellValue();
                cell = row.getCell(1);
                String shortName = cell.getStringCellValue();
                cell = row.getCell(2);
                String fullFileName = cell.getStringCellValue();
                cell = row.getCell(3);
                String metaInformation = cell.getStringCellValue();
                cell = row.getCell(4);
                String owner = cell.getStringCellValue();
                cell = row.getCell(5);
                String listOfObservations = cell.getStringCellValue();
                DataFile dataFile = new DataFile(id, shortName, fullFileName, metaInformation, owner, listOfObservations);
                if(dataFile.isStillExisting()){
                    ProjectListsManager.addDataFile(dataFile, false);
                }
            }
        }
    catch (Exception ex) {
            HelpDialog.warning("IO problem : Trying previous format","Warning", ex);
            oldGetExcelFileWithObservation(workbook);
        }
    }

    private static void oldGetExcelFileWithObservation(Workbook workbook) {
        try {
            String id = ProjectListsManager.getDataFileId();
            Sheet sheet = workbook.getSheet("FileWithObservation");
            Row row;
            Cell cell;
            Iterator<Row> iterator = sheet.iterator();
            iterator.next();
            while (iterator.hasNext()) {
                row = iterator.next();
                cell = row.getCell(0);
                String shortName = cell.getStringCellValue();
                cell = row.getCell(1);
                String fullFileName = cell.getStringCellValue();
                cell = row.getCell(2);
                String metaInformation = cell.getStringCellValue();
                cell = row.getCell(3);
                String owner = cell.getStringCellValue();
                cell = row.getCell(4);
                String listOfObservations = cell.getStringCellValue();
                DataFile dataFile = new DataFile(id, shortName, fullFileName, metaInformation, owner, listOfObservations);
                if(dataFile.isStillExisting()){
                    ProjectListsManager.addDataFile(dataFile, false);
                }
            }
        }
        catch (Exception ex) {
            HelpDialog.warning("IO problem","Warning", ex);
        }
    }
}
