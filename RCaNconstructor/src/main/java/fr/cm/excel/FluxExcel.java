package fr.cm.excel;

import fr.cm.objects.Flux;
import fr.cm.project.ProjectListsManager;
import org.apache.poi.ss.usermodel.*;

import java.util.Iterator;

public class FluxExcel {

    // -------------------------------------------------------------------------
    public static void saveExcelLinks(Workbook workbook) {
        Sheet sheet;
        Row row;
        Cell cell;
        sheet = workbook.createSheet("Fluxes");
        int liL = 0;
        row = sheet.createRow(liL);
        liL++;
        cell = row.createCell(0);
        cell.setCellValue("Flux");
        cell = row.createCell(1);
        cell.setCellValue("From");
        cell = row.createCell(2);
        cell.setCellValue("To");
        cell = row.createCell(3);
        cell.setCellValue("Trophic");
        for (Flux flux : ProjectListsManager.getListOfFluxes()) {
            row = sheet.createRow(liL);
            liL++;
            cell = row.createCell(0);
            cell.setCellValue(flux.getName());
            cell = row.createCell(1);
            cell.setCellValue(flux.getIn().getName());
            cell = row.createCell(2);
            cell.setCellValue(flux.getOut().getName());
            cell = row.createCell(3);
            cell.setCellValue(transformBoolean(flux.isTypeTrophic()));
        }
    }

    // -------------------------------------------------------------------------
    public static void getExcelLinks(Workbook workbook) {
        Sheet sheet = workbook.getSheet("Fluxes");
        Row row;
        Cell cell;
        Iterator<Row> iterator = sheet.iterator();
        iterator.next();
        while (iterator.hasNext()) {
            row = iterator.next();
            cell = row.getCell(0);
            String name = cell.getStringCellValue();
            cell = row.getCell(1);
            String in = cell.getStringCellValue();
            cell = row.getCell(2);
            String out = cell.getStringCellValue();
            cell = row.getCell(3);
            boolean stype = getBoolean(cell);
            Flux newFlux = new Flux(in, out, stype);
            ProjectListsManager.addLink(newFlux, false);
        }
    }

    static boolean getBoolean(Cell cell){
        CellType type = cell.getCellType();
        boolean active = false;
        if (type == CellType.STRING) {
            if (cell.getStringCellValue().equals("TRUE")
                    || cell.getStringCellValue().equals("1")) {
                active = true;
            }
        } else if (type == CellType.BOOLEAN) {
            active = cell.getBooleanCellValue();
        } else if (type == CellType.NUMERIC){
            if(cell.getNumericCellValue()==1){
                active = true;
            }
        }
        return(active);
    }

    // -------------------------------------------------------------------------
    static int transformBoolean(boolean active){
        if(active) return(1);
        return(0);
    }

}
