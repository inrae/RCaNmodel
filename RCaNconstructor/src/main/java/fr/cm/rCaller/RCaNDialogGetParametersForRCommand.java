package fr.cm.rCaller;

import fr.cm.ProjectManager.ProjectListsManager;
import fr.cm.RCaNMain.Context;
import fr.cm.xmlFiles.RCommandXML;
import javafx.collections.FXCollections;
import javafx.geometry.Insets;
import javafx.scene.control.*;
import javafx.scene.layout.GridPane;
import javafx.stage.Window;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class RCaNDialogGetParametersForRCommand extends Dialog<ButtonType>  {

    final Window window;

    int ncol;
    ListView cYear = null;
    ListView cFlux = null;
    ListView cYear2 = null;
    ListView cFlux2 = null;
    ListView cComponent = null;
    GridPane gridPane = null;

    public RCaNDialogGetParametersForRCommand(RCommandXML rCommandXML) {
        window = this.getDialogPane().getScene().getWindow();
        window.setOnCloseRequest(event -> window.hide());
        String typeParameter = rCommandXML.getTypeParameter();
        String parameter = "";

        if (typeParameter.equals("ALLCOMPONENTS")) {
            List<String> components = ProjectListsManager.getNamesOfComponentsIn();
            StringBuilder sb = new StringBuilder("c(");
            for (Object sC : components) {
                sb.append("'");
                sb.append((String) sC);
                sb.append("'");
                sb.append(", ");
            }
            sb.append(")");
            parameter = sb.toString();
        }
        else{
            if (typeParameter.length() > 0) {
                parameter = getParameterFromRCommand(typeParameter);
            }
        }
        rCommandXML.setParameter(clean(parameter));
    }

    private void addYear(SelectionMode sm){
        List<String> years = new ArrayList<>();
        for (int i = Context.getFirstYear(); i <= Context.getLastYear(); i++) {
            years.add("" + i);
        }
        cYear = new ListView(FXCollections.observableArrayList(years));
        cYear.setPrefHeight(0.4 * Context.getWindowHeight());
        cYear.getSelectionModel().setSelectionMode(sm);
        cYear.getSelectionModel().select(0);
        Label lYear = new Label("Year");
        if(sm.equals(SelectionMode.MULTIPLE))
            lYear.setText("Years");
        gridPane.add(lYear, ncol, 1);
        gridPane.add(cYear, ncol, 2);
        ncol ++;
    }
    private void addYear2(SelectionMode sm){
        List<String> years = new ArrayList<>();
        for (int i = Context.getFirstYear(); i <= Context.getLastYear(); i++) {
            years.add("" + i);
        }
        cYear2 = new ListView(FXCollections.observableArrayList(years));
        cYear2.setPrefHeight(0.4 * Context.getWindowHeight());
        cYear2.getSelectionModel().setSelectionMode(sm);
        cYear2.getSelectionModel().select(0);
        Label lYear2 = new Label("Year");
        if(sm.equals(SelectionMode.MULTIPLE))
            lYear2.setText("Years");
        gridPane.add(lYear2, ncol, 1);
        gridPane.add(cYear2, ncol, 2);
        ncol ++;
    }

    private void addFlux(SelectionMode sm) {
        List<String> flux = ProjectListsManager.getNamesOfLinks();
        cFlux = new ListView(FXCollections.observableArrayList(flux));
        cFlux.setPrefHeight(0.4 * Context.getWindowHeight());
        cFlux.getSelectionModel().select(0);
        cFlux.getSelectionModel().setSelectionMode(sm);
        cFlux.getSelectionModel().select(0);
        Label lFlux = new Label("Flux");
        if(sm.equals(SelectionMode.MULTIPLE))
            lFlux.setText("Fluxes");
        gridPane.add(lFlux, ncol, 1);
        gridPane.add(cFlux, ncol, 2);
        ncol++;
    }

    private void addFlux2(SelectionMode sm) {
        List<String> flux = ProjectListsManager.getNamesOfLinks();
        cFlux2 = new ListView(FXCollections.observableArrayList(flux));
        cFlux2.setPrefHeight(0.4 * Context.getWindowHeight());
        cFlux2.getSelectionModel().setSelectionMode(sm);
        cFlux2.getSelectionModel().select(0);
        Label lFlux2 = new Label("Flux");
        if(sm.equals(SelectionMode.MULTIPLE))
            lFlux2.setText("Fluxes");
        gridPane.add(lFlux2, ncol, 1);
        gridPane.add(cFlux2, ncol, 2);
        ncol++;
    }

    private void addComponent(SelectionMode sm) {
        List<String> components = ProjectListsManager.getNamesOfComponentsIn();
        cComponent = new ListView(FXCollections.observableArrayList(components));
        cComponent.setPrefHeight(0.4 * Context.getWindowHeight());
        cComponent.getSelectionModel().setSelectionMode(sm);
        cComponent.getSelectionModel().select(0);
        Label lComponent = new Label("Flux");
        if(sm.equals(SelectionMode.MULTIPLE))
            lComponent.setText("Components");
        gridPane.add(lComponent, ncol, 1);
        gridPane.add(cComponent, ncol, 2);
        ncol++;
    }

    private String clean(String st){
        String nst = st.replace(",)",")")
                .replace(", )",")")
                .replace(", ,'",",")
                .replace(",,'",",");
        return(nst);
    }



    String getParameterFromRCommand(String typeParameter) {
        String parameter ="";

        gridPane = new GridPane();
        gridPane.setPrefSize(0.4 * Context.getWindowWidth(), 0.6 * Context.getWindowHeight());
        gridPane.setHgap(10);
        gridPane.setVgap(10);
        gridPane.setPadding(new Insets(10, 10, 10, 10));
        ncol = 0;

        switch (typeParameter) {
            case "FLUX[YEAR]":
                addYear(SelectionMode.SINGLE);
                addFlux(SelectionMode.SINGLE);
                break;
            case "COMPONENT[YEAR]":
                addYear(SelectionMode.SINGLE);
                addComponent(SelectionMode.SINGLE);
                break;
            case "FLUXES":
                addFlux(SelectionMode.MULTIPLE);
                break;
            case "COMPONENTS":
                addComponent(SelectionMode.MULTIPLE);
                break;
            case "FLUXESYEAR":
                addFlux(SelectionMode.MULTIPLE);
                addYear(SelectionMode.SINGLE);
                break;
            case "COMPONENTSYEAR":
                addComponent(SelectionMode.MULTIPLE);
                addYear(SelectionMode.SINGLE);
                break;
            case "FLUXESCOMPONENTS":
                addFlux(SelectionMode.MULTIPLE);
                addComponent(SelectionMode.MULTIPLE);
                break;
            case "FLUX[YEAR]FLUX[YEAR]":
                addYear(SelectionMode.SINGLE);
                addFlux(SelectionMode.SINGLE);
                addYear2(SelectionMode.SINGLE);
                addFlux2(SelectionMode.SINGLE);
                break;
            case "FLUX":
                addFlux(SelectionMode.SINGLE);
                break;
            case "COMPONENT":
                addComponent(SelectionMode.SINGLE);
                break;
        }

        if (ncol > 0) {
            ButtonType buttonTypeOk = new ButtonType("OK", ButtonBar.ButtonData.OK_DONE);
            ButtonType buttonTypeCancel = new ButtonType("Cancel", ButtonBar.ButtonData.OK_DONE);
            this.getDialogPane().setContent(gridPane);
            this.getDialogPane().getButtonTypes().add(buttonTypeOk);
            this.getDialogPane().getButtonTypes().add(buttonTypeCancel);

            Optional<ButtonType> result = this.showAndWait();
            String year, flux, component;
            StringBuilder sb;
            List<Object> sFlux, sComponent;
            if (result.isPresent()) {
                if (result.get() == buttonTypeOk) {
                    switch (typeParameter) {
                        case "FLUX[YEAR]":
                            year = (String) cYear.getSelectionModel().getSelectedItems().get(0);
                            flux = (String) cFlux.getSelectionModel().getSelectedItems().get(0);
                            parameter = "'" + flux + "[" + year + "]'";
                            break;
                        case "COMPONENT[YEAR]":
                            year = (String) cYear.getSelectionModel().getSelectedItems().get(0);
                            component = (String) cComponent.getSelectionModel().getSelectedItems().get(0);
                            parameter = "'" + component + "[" + year + "]'";
                            break;
                        case "FLUXES":
                            sFlux = cFlux.getSelectionModel().getSelectedItems();
                            sb = new StringBuilder("c(");
                            for (Object sF : sFlux) {
                                sb.append("'");
                                sb.append((String) sF);
                                sb.append("'");
                                sb.append(", ");
                            }
                            sb.append(")");
                            parameter = sb.toString();
                            break;
                        case "COMPONENTS":
                            sComponent = cComponent.getSelectionModel().getSelectedItems();
                            sb = new StringBuilder("c(");
                            for (Object sC : sComponent) {
                                sb.append("'");
                                sb.append((String) sC);
                                sb.append("'");
                                sb.append(", ");
                            }
                            sb.append(")");
                            parameter = sb.toString();
                            break;
                        case "FLUXESYEAR":
                            year = (String) cYear.getSelectionModel().getSelectedItems().get(0);
                            sFlux = cFlux.getSelectionModel().getSelectedItems();
                            sb = new StringBuilder("c(");
                            for (Object sF : sFlux) {
                                sb.append("'");
                                sb.append((String) sF);
                                sb.append("'");
                                sb.append(", ");
                            }
                            sb.append("), year = " + year);
                            parameter = sb.toString();
                            break;
                        case "COMPONENTSYEAR":
                            year = (String) cYear.getSelectionModel().getSelectedItems().get(0);
                            sComponent = cComponent.getSelectionModel().getSelectedItems();
                            sb = new StringBuilder("c(");
                            for (Object sC : sComponent) {
                                sb.append("'");
                                sb.append((String) sC);
                                sb.append("'");
                                sb.append(", ");
                            }
                            sb.append("), year = " + year);
                            parameter = sb.toString();
                            break;
                        case "FLUXESCOMPONENTS":
                            sFlux = cFlux.getSelectionModel().getSelectedItems();
                            sb = new StringBuilder("c(");
                            for (Object sF : sFlux) {
                                sb.append("'");
                                sb.append((String) sF);
                                sb.append("'");
                                sb.append(", ");
                            }
                            sComponent = cComponent.getSelectionModel().getSelectedItems();
                            if (sComponent.size() > 0) {
                                for (Object sC : sComponent) {
                                    sb.append("'");
                                    sb.append((String) sC);
                                    sb.append("'");
                                    sb.append(", ");
                                }
                            }
                            sb.append(")");
                            parameter = sb.toString();
                            break;
                        case "FLUX[YEAR]FLUX[YEAR]":
                            year = (String) cYear.getSelectionModel().getSelectedItems().get(0);
                            flux = (String) cFlux.getSelectionModel().getSelectedItems().get(0);
                            String year2 = (String) cYear2.getSelectionModel().getSelectedItems().get(0);
                            String flux2 = (String) cFlux2.getSelectionModel().getSelectedItems().get(0);
                            parameter = "c('" + flux + "[" + year + "]'" + ","
                                    + "'" + flux2 + "[" + year2 + "]'" + ")";
                            break;
                        case "FLUX":
                            flux = (String) cFlux.getSelectionModel().getSelectedItems().get(0);
                            parameter = "'" + flux + "'";
                            break;
                        case "COMPONENT":
                            component = (String) cComponent.getSelectionModel().getSelectedItems().get(0);
                            parameter = "'" + component + "'";
                            break;
                    }
                }
            }
        }
        return (parameter);
    }
}
