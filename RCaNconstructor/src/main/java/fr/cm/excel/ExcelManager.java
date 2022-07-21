package fr.cm.excel;

import fr.cm.canObjects.*;
import fr.cm.GUIdialogs.HelpDialog;
import fr.cm.parameters.Strings;
import org.apache.poi.ss.usermodel.*;

import fr.cm.RCaNMain.Context;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * @author christianmullon
 */
public class ExcelManager {

    // -------------------------------------------------------------------------
    public static void saveExcel() {
        String fileName = Context.getFullFileName();
        try {
            Workbook workbook = WorkbookFactory.create(true);
            saveExcelMetaInformation(workbook);
            saveExcelComponents(workbook);
            saveExcelLinks(workbook);
            saveExcelConstraints(workbook);
            saveExcelObservations(workbook);
            saveExcelFileWithObservation(workbook);
            saveExcelFileActions(workbook);
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
            FileInputStream inputStream = new FileInputStream(new File(fileName));
            Workbook workbook = WorkbookFactory.create(inputStream);
            getExcelMetaInformation(workbook);
            getExcelComponents(workbook);
            getExcelLinks(workbook);
            getExcelConstraints(workbook);
            getExcelObservations(workbook);
            getExcelFileWithObservation(workbook);
            getExcelFileActions(workbook);
        } catch (FileNotFoundException ex) {
            HelpDialog.warning("File not found","Warning", ex);
        } catch (IOException ex) {
            HelpDialog.warning("IO problem","Warning", ex);
        }
    }

    // -------------------------------------------------------------------------
    private static void saveExcelFileActions(Workbook workbook) {
        Sheet sheet;
        Row row;
        Cell cell;
        sheet = workbook.createSheet("Actions");

        row = sheet.createRow(0);
        cell = row.createCell(0);
        cell.setCellValue("Date");
        cell = row.createCell(1);
        cell.setCellValue("Comment");

        List<Action> listOfActions = ProjectListsManager.getListOfActions();
        int c = 0;
        for (Action action : listOfActions) {
            c++;
            row = sheet.createRow(c + 1);
            cell = row.createCell(0);
            cell.setCellValue(action.getDate());
            cell = row.createCell(1);
            cell.setCellValue(action.getComment());
            cell = row.createCell(2);
            cell.setCellValue(action.getCommentAuthor());
        }
    }

    // -------------------------------------------------------------------------
    public static void getExcelFileActions(Workbook workbook) {
        ProjectListsManager.initListOfActions();
        Sheet sheet = workbook.getSheet("Actions");
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
            String commentAuthor ="";
            try{
                cell = row.getCell(2);
                commentAuthor = cell.getStringCellValue();
            }
            catch(Exception e){};
            Action newAction = new Action(date, comment,commentAuthor);
            ProjectListsManager.addAction(newAction);
        }
    }

    // -------------------------------------------------------------------------
    private static void saveExcelMetaInformation(Workbook workbook) {
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

        MetaInformation metaInformation = ProjectListsManager.getMetaInformation();
        List<MetaElement> elements = metaInformation.getElements();
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
        new MetaInformation();
        Sheet sheet = workbook.getSheet("INFO");
        Row row;
        Cell cell;
        MetaInformation metaInformation = ProjectListsManager.getMetaInformation();
        List<MetaElement> elements = metaInformation.getElements();
        try {
            for (int c = 0; c < elements.size(); c++) {
                row = sheet.getRow(c + 2);
                cell = row.getCell(1);
                String content = cell.getStringCellValue();
                elements.get(c).setMetaContent(content);
            }
        } catch (Exception e) {
            new MetaInformation();
        }
    }

    // -------------------------------------------------------------------------
    private static void saveExcelComponents(Workbook workbook) {
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
    private static void getExcelComponents(Workbook workbook) {
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
            ProjectListsManager.addComponent(component);
        }
    }

    // -------------------------------------------------------------------------
    private static void saveExcelLinks(Workbook workbook) {
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
    private static void getExcelLinks(Workbook workbook) {
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
            ProjectListsManager.addLink(newFlux);
        }
    }

    // ---------------------------------------------------------------------
    private static void saveExcelObservations(Workbook workbook) {
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
                int stringCellValue = (int) cell.getNumericCellValue();
                return stringCellValue;
            } else if (cellType == CellType.STRING) {
                int stringCellValue = Integer.parseInt(cell.getStringCellValue());
                return stringCellValue;
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
                double stringCellValue = cell.getNumericCellValue();
                return stringCellValue;
            } else if (cellType == CellType.STRING) {
                double stringCellValue = Double.parseDouble(cell.getStringCellValue());
                return stringCellValue;
            }
        } catch(Exception ex){
            return(-1.0);
        }
        return(-1.0);
    }

    // -------------------------------------------------------------------------
    private static void getExcelObservations(Workbook workbook) {
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
                ProjectListsManager.addObservation(observation);
            }
        }
    }

    // ---------------------------------------------------------------------
    private static void saveExcelConstraints(Workbook workbook) {
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
    private static void getExcelConstraints(Workbook workbook) {
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
                ProjectListsManager.addConstraint(constraint);
            } catch (Exception e) {
                // le champ active n'existe pas encore
            }
        }
    }

    // -------------------------------------------------------------------------
    private static void saveExcelFileWithObservation(Workbook workbook) {
        Sheet sheet;
        Row row;
        Cell cell;
        sheet = workbook.createSheet("FileWithObservation");
        int lig = 0;
        row = sheet.createRow(lig);
        cell = row.createCell(0);
        cell.setCellValue("File name");
        cell = row.createCell(1);
        cell.setCellValue("Full file name");
        cell = row.createCell(2);
        cell.setCellValue("Meta information");
        cell = row.createCell(3);
        cell.setCellValue("Owner");
        cell = row.createCell(4);
        cell.setCellValue("First year");
        cell = row.createCell(5);
        cell.setCellValue("Last year");
        cell = row.createCell(6);
        cell.setCellValue("Selected columns");
        lig++;
        for (int c = 0; c < ProjectListsManager.getListOfDataFiles().size(); c++) {
            DataFile dataFile = ProjectListsManager.getListOfDataFiles().get(c);
            row = sheet.createRow(lig);
            cell = row.createCell(0);
            cell.setCellValue(dataFile.getShortName());
            cell = row.createCell(1);
            cell.setCellValue(dataFile.getFullFileName());
            cell = row.createCell(2);
            cell.setCellValue(dataFile.getMetaInformationAboutDataFile());
            cell = row.createCell(3);
            cell.setCellValue(dataFile.getOwner());
            cell = row.createCell(4);
            cell.setCellValue(dataFile.codeAddedObservations());
            lig++;
        }
    }


    // -------------------------------------------------------------------------
    private static void getExcelFileWithObservation(Workbook workbook) {
        try {
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
                DataFile dataFile = new DataFile(
                        shortName,
                        fullFileName,
                        metaInformation,
                        owner,
                        listOfObservations);
                if(dataFile.isStillExisting()){
                    ProjectListsManager.addDataFile(dataFile);
                }
            }
        } catch (Exception e) {
            System.out.println("Probleme pour lire la liste des fichiers avec observations");
            e.printStackTrace();
        }
    }

    // -------------------------------------------------------------------------
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
