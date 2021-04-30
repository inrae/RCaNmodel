package fr.cm.canObjects;

import fr.cm.excel.ExcelManagerCSV;
import org.apache.commons.io.FilenameUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class ExternalDataFile {
    String fileName;
    String fullFileName;
    String metaInformationAboutDataFile;
    String[][] dataInFile;
    int firstYear; // in file
    int lastYear;  // in file
    int fYear; // selected by user
    int lYear; // selected by user

    private List<String> namesColumnInFile;
    private List<Observation> addedObservations;

    public ExternalDataFile(String fullFileName,
                            String metaInformationAboutDataFile,
                            String codedObservations,
                            int firstYear, int lastYear,
                            int fYear, int lYear){
        this.fileName = FilenameUtils.removeExtension(FilenameUtils.getName(fullFileName));
        this.fullFileName = fullFileName;
        this.metaInformationAboutDataFile = metaInformationAboutDataFile;
        this.firstYear = firstYear;
        this.lastYear = lastYear;
        this.fYear = fYear;
        this.lYear = lYear;
        decodeAddedObservations(codedObservations);
        readData();
     }

    void readData(){
         dataInFile = ExcelManagerCSV.importCSV(fullFileName);
         int nv = dataInFile[0].length;
         namesColumnInFile = new ArrayList<>();
         namesColumnInFile.addAll(Arrays.asList(dataInFile[0]).subList(1, nv));
     }

    public ExternalDataFile(String fullFileName){
        this.fileName = FilenameUtils.removeExtension(FilenameUtils.getName(fullFileName));
        this.fullFileName = fullFileName;
        this.metaInformationAboutDataFile = "";
        addedObservations = new ArrayList<>();
        readData();
        int ny = dataInFile.length-1;
        firstYear = Integer.parseInt(dataInFile[1][0]);
        lastYear = Integer.parseInt(dataInFile[ny][0]);
        fYear  = firstYear;
        lYear = lastYear;
    }

    public double correct(String str){
        double parseDouble = Double.parseDouble(str.replace(",","."));
        return parseDouble;
    }

    public void addObservationFromColumn(String nameColumn){
        // APRES UN CLIC DANS LA LISTE DES COLONNES DU FICHIER
        int numObs = namesColumnInFile.indexOf(nameColumn);
        int ny = lYear+1-fYear;
        double[] values = new double[ny];
        for(int y = 0; y < ny; y++){
            values[y] = correct(dataInFile[y+(fYear-firstYear)+1][numObs+1]);
        }
        Observation observation = new Observation(fileName, nameColumn, values, fYear, lYear);
        if (! ListsManager.containsObservation(fileName+"."+nameColumn))  {
            if( ! addedObservations.contains(observation) ){
                addedObservations.add(observation);  // RAJOUT DANS LA LISTE DES OBSERVATIONS AJOUTEES
                ListsManager.addObservation(observation);  // RAJOUT DANS LA LISTE DES OBSERVATIONS DU RESEAU
            }
        }
    }

    public String codeAddedObservations() {
        StringBuilder bo = new StringBuilder("");
        for(Observation observation : addedObservations){
            bo.append(observation.codeInExternalFile());
            bo.append(";");
        }
        return bo.toString();
    }

    public void decodeAddedObservations(String codeObservations) {
        addedObservations = new ArrayList<>();
        String[] stObservations = codeObservations.split(";");
        for(String stObservation : stObservations){
            if(! stObservation.equals("")){
                Observation observation = Observation.codeFromExternalFile(stObservation);
                addedObservations.add(observation);
            }
        }
    }

    public String getFileName(){ return fileName; }

    public String getFullFileName() { return fullFileName; }

    public String getMetaInformationAboutDataFile() { return metaInformationAboutDataFile; }

    public void setMetaInformationAboutDatatFile(String metaInformationAboutDataFile) { this.metaInformationAboutDataFile = metaInformationAboutDataFile; }

    public void setFileName(String fileName) { this.fileName = fileName; }

    public String getFirstYear() { return String.valueOf(firstYear); }

    public void setFirstYear(String firstYear) {
        this.firstYear = Integer.parseInt(firstYear);
    }

    public String getLastYear() {
        return String.valueOf(lastYear);
    }

    public void setLastYear(String lastYear) {
        this.lastYear = Integer.parseInt(lastYear);
    }

    public int getfYear() {
        return fYear;
    }

    public int getlYear() { return lYear; }
    /*
    public void setfYear(int fYear) {
        this.fYear = Math.max(fYear,firstYear);
    }

    public void setlYear(int lYear) {
        this.lYear = Math.min(lYear,lastYear);
    }

    public void setNamesColumnInFile(List<String> namesColumnInFile) {
        this.namesColumnInFile = namesColumnInFile;
    }
    public List<Observation> getAddedObservations() {
        return addedObservations;
    }

    public void setAddedObservations(List<Observation> addedObservations) { this.addedObservations = addedObservations; }

*/
    public List<String> getNamesColumnInFile() { return namesColumnInFile; }

}
