/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.excel;

import fr.cm.dialogs.HelpDialog;

import java.io.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * @author christianmullon
 */
public class ExcelManagerCSV {

    // -------------------------------------------------------------------------
    private static String checkSeparatorCSV(String fileName) {
        String fileSeparator = ",";
        try (BufferedReader br = new BufferedReader(new FileReader(fileName))) {
            String line = br.readLine();
            String[] values = line.split(fileSeparator);
            if (values.length < 2) {
                fileSeparator = ";";
            }
        } catch (Exception ex) {
            HelpDialog.warning("Separator issue with" + fileName,"Warning", ex);
        }
        return fileSeparator;
    }
    private static boolean existsCSV(String fileName) {
        try (BufferedReader br = new BufferedReader(new FileReader(fileName))) {
            br.readLine();
             return(true);
          } catch (Exception ex) {
         }
        return (false);
    }

    // -------------------------------------------------------------------------
    public static String[][] importCSV(String fileName) {
        if (existsCSV(fileName)) {
            String fileSeparator = checkSeparatorCSV(fileName);
            List<List<String>> records = new ArrayList<>();
            try (BufferedReader br = new BufferedReader(new FileReader(fileName))) {
                String line;
                while ((line = br.readLine()) != null) {
                    String[] values = line.split(fileSeparator);
                    records.add(Arrays.asList(values));
                }
            } catch (FileNotFoundException ex) {
                HelpDialog.warning("Import CSV : File not found " + fileName,"Warning", ex);
            } catch (IOException ex) {
                HelpDialog.warning("Format issue with " + fileName,"Warning", ex);
            }
            int nl = records.size();
            int nc = 0;
            for (List<String> record : records) {
                nc = Math.max(nc, record.size());
            }
            String[][] array = new String[nl][nc];
            for (int l = 0; l < nl; l++) {
                for (int c = 0; c < nc; c++) {
                    array[l][c] = "-";
                }
            }
            for (int l = 0; l < nl; l++) {
                for (int c = 0; c < records.get(l).size(); c++) {
                    array[l][c] = records.get(l).get(c);
                }
            }
            return (array);
        }
        HelpDialog.warning("Import CSV : File not found " + fileName, "Warning");
        return null;
    }

}
