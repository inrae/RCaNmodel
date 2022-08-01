package fr.cm.objects;

import fr.cm.project.ProjectListsManager;
import org.apache.poi.ss.usermodel.*;

import java.util.Iterator;

public class ConstraintExcel {

    public static void saveExcelConstraints(Workbook workbook) {
        Sheet sheet;
        Row row;
        Cell cell;
        sheet = workbook.createSheet("Constraints");
        int lig = 0;
        row = sheet.createRow(lig);
        cell = row.createCell(0);
        cell.setCellValue("Id");
        cell = row.createCell(1);
        cell.setCellValue("Constraint");
        cell = row.createCell(2);
        cell.setCellValue("Time-range");
        cell = row.createCell(3);
        cell.setCellValue("Active");
        cell = row.createCell(4);
        cell.setCellValue("Comment");
        lig++;
        for (int c = 0; c < ProjectListsManager.getListOfConstraints().size(); c++) {
            Constraint constraint = ProjectListsManager.getListOfConstraints().get(c);
            row = sheet.createRow(lig);
            cell = row.createCell(0);
            cell.setCellValue(constraint.getName());
            cell = row.createCell(1);
            cell.setCellValue(constraint.getFormula());
            cell = row.createCell(2);
            cell.setCellValue(constraint.getYears());
            cell = row.createCell(3);
            cell.setCellValue(transformBoolean(constraint.isActive()));
            cell = row.createCell(4);
            cell.setCellValue(constraint.getComment());
            lig++;
        }
    }

    // -------------------------------------------------------------------------
    public static void getExcelConstraints(Workbook workbook) {
        Sheet sheet = workbook.getSheet("Constraints");
        Row row;
        Cell cell;
        Iterator<Row> iterator = sheet.iterator();
        iterator.next();
        String name;
        String formula ;
        String years;
        boolean active;
        String comment;
        while (iterator.hasNext()) {
            try {
                row = iterator.next();
                cell = row.getCell(0);
                name = cell.getStringCellValue();
                cell = row.getCell(1);
                formula = cell.getStringCellValue();
                cell = row.getCell(2);
                years = cell.getStringCellValue();
                cell = row.getCell(3);
                active = getBoolean(cell);
                cell = row.getCell(4);
                if(cell == null) {
                    comment = name;
                } else {
                    comment = cell.getStringCellValue();
                }
                Constraint constraint = new Constraint(name, formula, years, active, comment);
                ProjectListsManager.addConstraint(constraint, false);
            } catch (Exception e) {
                // le champ active n'existe pas encore
            }
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
