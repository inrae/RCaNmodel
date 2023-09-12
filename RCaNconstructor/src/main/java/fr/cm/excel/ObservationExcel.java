package fr.cm.excel;

import fr.cm.Main.Context;
import fr.cm.objects.Observation;
import fr.cm.project.ProjectListsManager;
import org.apache.poi.ss.usermodel.*;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class ObservationExcel {
    // ---------------------------------------------------------------------
    public static void saveExcelObservations(Workbook workbook) {
        int first = Context.getFirstYear();
        int last = Context.getLastYear();
        int ny = last - first + 1;

        Sheet sheet;
        Cell cell;
        Row row;
        int o;
        sheet = workbook.createSheet("Input time-series");
        row = sheet.createRow(0);
        cell = row.createCell(0);
        cell.setCellValue("Year");
        o = 0;
        for (Observation observation : ProjectListsManager.getListOfObservations()) {
            o++;
            cell = row.createCell(o);
            cell.setCellValue(observation.getObsName());
        }

        for (int y = 0; y < ny; y++) {
            o = 0;
            row = sheet.createRow(y + 1);
            cell = row.createCell(0);
            cell.setCellValue(y + first);
            for (Observation observation : ProjectListsManager.getListOfObservations()) {
                o++;
                double val = observation.getValues()[y];
                if (val >= 0.0) {
                    cell = row.createCell(o);
                    cell.setCellValue(observation.getValues()[y]);
                }
            }
        }
    }

    static int getIntFromCell(Cell cell){
        CellType cellType = cell.getCellType();
        try {
            if (cellType == CellType.NUMERIC) {
                return (int) cell.getNumericCellValue();
            } else if (cellType == CellType.STRING) {
                return Integer.parseInt(cell.getStringCellValue());
            }
        } catch(Exception ex){
            return(-1);
        }
        return(-1);
    }

    static double getDoubleFromCell(Cell cell){
        try {
            CellType cellType = cell.getCellType();
            if (cellType == CellType.NUMERIC) {
                return cell.getNumericCellValue();
            } else if (cellType == CellType.STRING) {
                return Double.parseDouble(cell.getStringCellValue());
            }
        } catch(Exception ex){
            return(-1.0);
        }
        return(-1.0);
    }

    // -------------------------------------------------------------------------
    public static void getExcelObservations(Workbook workbook) {
        int first = 5000;
        int last = -5000;
        Sheet sheet;
        Cell cell;
        Row row;

        sheet = workbook.getSheet("Input time-series");
        Iterator<Row> iteratorRow = sheet.iterator();
        // Identifiants des colonnes dans la première ligne
        List<String> idObs = new ArrayList<>();
        row = iteratorRow.next();
        Iterator<Cell> iteratorCell = row.iterator();
        iteratorCell.next(); // on saute la premiere colonne
        while (iteratorCell.hasNext()) {
            cell = iteratorCell.next();
            String scell = cell.toString();
            idObs.add(scell);
        }
        //  Identifiants des années dans la premiere colonne a partir de la deuxieme ligne
        while (iteratorRow.hasNext()) {
            row = iteratorRow.next();
            cell = row.getCell(0);
            if(cell != null) {
                int val = getIntFromCell(cell);
                first = Math.min(first, val);
                last = Math.max(last, val);
            }
        }
        Context.setLastYear(last);
        Context.setFirstYear(first);
        int nbo = idObs.size();
        if(nbo>0) {
            int nby = last - first + 1;
            double[][] values = new double[nbo][nby];
            for (int y = 0; y < nby; y++) {
                row = sheet.getRow(y + 1);
                for (int o = 0; o < nbo; o++) {
                    cell = row.getCell(o + 1);
                    double val = getDoubleFromCell(cell);
                    values[o][y] = val;
                }
            }
            for (int o = 0; o < nbo; o++) {
                Observation observation = new Observation(idObs.get(o), values[o]);
                ProjectListsManager.addObservation(observation, false);
            }
        }
    }
}
