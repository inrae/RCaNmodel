package fr.cm.objects;

import fr.cm.preferences.Strings;
import fr.cm.project.ProjectListsManager;
import org.apache.poi.ss.usermodel.*;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class ComponentExcel {

    // -------------------------------------------------------------------------
    public static void saveExcelComponents(Workbook workbook) {
        String[] parametersNames = Strings.getParametersNames();
        int np = parametersNames.length;
        Sheet sheet;
        Row row;
        Cell cell;
        sheet = workbook.createSheet("Components & input parameter");
        int liP = 0;
        row = sheet.createRow(liP);
        liP++;
        cell = row.createCell(0);
        cell.setCellValue("Component");
        cell = row.createCell(1);
        cell.setCellValue("Inside");
        for (int p = 0; p < np; p++) {
            cell = row.createCell(p + 2);
            cell.setCellValue(parametersNames[p]);
        }
        cell = row.createCell(np + 2);
        cell.setCellValue("X");
        cell = row.createCell(np + 3);
        cell.setCellValue("Y");

        for (Component component : ProjectListsManager.getListOfComponents()) {
            row = sheet.createRow(liP);
            liP++;
            cell = row.createCell(0);
            cell.setCellValue(component.getName());
            cell = row.createCell(1);
            cell.setCellValue(  transformBoolean(component.isInside()));
            for (int p = 0; p < np; p++) {
                double val = component.getParameters()[p];
                if (val >= 0) {
                    cell = row.createCell(p + 2);
                    cell.setCellValue(val);
                }
            }
            cell = row.createCell(np + 2);
            cell.setCellValue(component.getX());
            cell = row.createCell(np + 3);
            cell.setCellValue(component.getY());
        }
    }

    // -------------------------------------------------------------------------
    public static void getExcelComponents(Workbook workbook) {
        int np = Strings.getNumberOfParameters();
        double[] parameters = new double[np];
        Sheet sheet;
        Row row;
        Cell cell;
        sheet = workbook.getSheet("Components & input parameter");
        Iterator<Row> iterator = sheet.iterator();
        row = iterator.next();
        Iterator<Cell> iteratorR = row.cellIterator();
        List<String> colNames  = new ArrayList<>();
        while (iteratorR.hasNext()) {
            cell = iteratorR.next();
            String var = cell.getStringCellValue();
            colNames.add(var);
        }
        while (iterator.hasNext()) {
            row = iterator.next();
            cell = row.getCell(0);
            String nameComponent = cell.getStringCellValue();
            cell = row.getCell(1);
            boolean typeComponent = getBoolean(cell);
            for (int p = 0; p < np; p++) {
                String varName = Strings.getParametersNames(p);
                int ip = colNames.indexOf(varName);
                cell = row.getCell(ip);
                if (cell == null) {
                    parameters[p] = -1.0;
                } else {
                    parameters[p] = cell.getNumericCellValue();
                }
            }
            double x;
            double y;
            try {
                int ix = colNames.indexOf("X");
                cell = row.getCell(ix);
                x = cell.getNumericCellValue();
                int iy = colNames.indexOf("Y");
                cell = row.getCell(iy);
                y = cell.getNumericCellValue();
            } catch (Exception e) {
                x = 0.2 + 0.6 * Math.random();
                y = 0.2 + 0.4 * Math.random();
            }
            Component component = new Component(nameComponent, parameters, typeComponent, x, y);
            ProjectListsManager.addComponent(component, false);
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
