
/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.canObjects;

import org.apache.commons.io.IOUtils;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author christianmullon
 */
public class MetaInformation {

    static List<MetaElement> elements;

    // --------------------------------------------
    public MetaInformation() {
        elements = new ArrayList<>();
        String fileName = "project/Project.txt";
        InputStream inst = MetaInformation.class.getClassLoader().getResourceAsStream(fileName);
        List<String> elementsResource = new ArrayList<>();
        try {
            elementsResource = IOUtils.readLines(inst, StandardCharsets.UTF_8.name());
        } catch (IOException e) {
            e.printStackTrace();
        }
        for(String elementR : elementsResource){
            MetaElement metaElement = new MetaElement(elementR);
            elements.add(metaElement);
        }
    }

    public  int getNc() {
        return elements.size();
    }

    public  List<MetaElement> getElements() {
        return elements;
    }

}
