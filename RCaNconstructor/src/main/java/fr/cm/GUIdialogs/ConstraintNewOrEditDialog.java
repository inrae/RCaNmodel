/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.GUIdialogs;

import fr.cm.canObjects.Constraint;
import fr.cm.canObjects.Formula;
import fr.cm.ProjectManager.ProjectListsManager;
import fr.cm.RCaNMain.Context;
import fr.cm.parameters.ColorsAndFormats;
import fr.cm.parameters.Strings;
import javafx.event.ActionEvent;
import javafx.geometry.Insets;
import javafx.scene.control.*;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.stage.Window;

import java.util.ArrayList;
import java.util.List;

/**
 * @author christianmullon
 */
public class ConstraintNewOrEditDialog extends Dialog<ButtonType> {

    Formula formula ;
    List<Label> tokensOfFormula = new ArrayList<>();

    double width = 0.9 * Context.getWindowWidth();
    double height =  Context.getWindowHeight();

    boolean editCst;

    final Window window;

    Constraint previousConstraint;

    Button buttonClear = new Button(Strings.clearFormula);
    Button buttonAddEditCst = new Button(Strings.addConstraint);
    Button buttonCancelCst = new Button(Strings.cancelConstraint);

    final ListView<String> listViewYears = new ListView<>();
    final ListView<String> listViewComponents = new ListView<>();
    final ListView<String> listViewLinks = new ListView<>();
    final ListView<String> listViewObservations = new ListView<>();

    boolean selectAllYears;

    final Label labelTokenYears = new Label("");
    final Label labelTokenValidityOfFormula = new Label("");
    final Label helpFormula = new Label(Strings.helpFormula);

    final Label cstNameLabel = new Label("Name of constraint");
    final TextArea cstNameTextArea = new TextArea("");

    final Label componentLabel = new Label("Components");
    final Label linkLabel = new Label("Links");
    final Label observationLabel = new Label("Observations");

    final GridPane boxSymbols = new GridPane();
    ToggleGroup toggleGroup = new ToggleGroup();
    ToggleButton toggleButtonAllYears = new ToggleButton("All years");
    ToggleButton toggleButtonSelectedYears = new ToggleButton("Select years");

    // ----------------------------------------------------------------------------------
    final VBox vboxVariables = new VBox();
    final VBox vboxYears = new VBox();
    final HBox hboxTables = new HBox();
    final HBox hboxToggle = new HBox();
    final HBox hboxTokensLabels = new HBox();
    final HBox hboxFormulaStuff = new HBox();
    final HBox hboxButtons = new HBox();
    final VBox vboxContent = new VBox();

    String cstName;

    public ConstraintNewOrEditDialog() {
        editCst = false;
        buttonAddEditCst.setText(Strings.addConstraint);
        window = this.getDialogPane().getScene().getWindow();
        window.setOnCloseRequest(event -> window.hide());
        selectAllYears = true;
        // ----------------------------------------------------------------------------------
        createTokens();
        formula = new Formula();
        updateTokens();
        // ----------------------------------------------------------------------------------
        cstName = "C" + (int)(Math.random()*100000.0f);
        labelTokenValidityOfFormula.setText(Strings.validityOfFormula + "?");
        initDialog();
     }

    // --------------------------------------------
    public ConstraintNewOrEditDialog(Constraint constraint) {
        this.previousConstraint = constraint;
        editCst = true;
        buttonAddEditCst.setText(Strings.editConstraint);
        window = this.getDialogPane().getScene().getWindow();
        window.setOnCloseRequest(event -> window.hide());
        selectAllYears = true;
        // ----------------------------------------------------------------------------------
        createTokens();
        formula = new Formula(constraint);
        updateTokens();
        // ----------------------------------------------------------------------------------
        cstName = constraint.getName();
        String fOk = formula.check();
        labelTokenValidityOfFormula.setText(Strings.validityOfFormula + fOk);
        initDialog();
    }

    // --------------------------------------------
    void initDialog(){
        cstNameTextArea.setText(cstName);
        helpFormula.setWrapText(true);
        // ----------------------------------------------------------------------------------
        labelTokenYears.setFont(new Font("Arial", 15));
        labelTokenYears.setBorder(new Border(new BorderStroke(Color.BLACK, BorderStrokeStyle.SOLID, CornerRadii.EMPTY, new BorderWidths(2))));
        labelTokenYears.setBackground(new Background(new BackgroundFill(Color.LIGHTGREY, CornerRadii.EMPTY, Insets.EMPTY)));
        setLabelYears();
        // ----------------------------------------------------------------------------------
        List<String> allSymbols = new ArrayList<>();
        allSymbols.addAll(Formula.nombres);
        allSymbols.addAll(Formula.symbolsAndOperators);

        for (int s = 0; s < allSymbols.size(); s++) {
            Button button = new Button(allSymbols.get(s));
            button.setOnAction((ActionEvent e) -> {
                String token = button.getText();
                addToken(token);
            });
            button.setMinWidth(70);
            int col = s / 5;
            int lig = s % 5;
            boxSymbols.add(button, lig, col);
        }
        // ----------------------------------------------------------------------------------
        List<String> namesComponents = new ArrayList<>();
        namesComponents.addAll(ProjectListsManager.getNamesOfComponents());
        listViewComponents.getSelectionModel().selectFirst();
        listViewComponents.setOnMouseClicked(evt -> {
            String token = listViewComponents.getSelectionModel().getSelectedItem();
            formula.append(token);
            String fOk = formula.check();
            labelTokenValidityOfFormula.setText(Strings.validityOfFormula + fOk);
            updateTokens();
        });
        listViewComponents.getItems().addAll(namesComponents);
        // ----------------------------------------------------------------------------------
        List<String> namesLinks = new ArrayList<>();
        namesLinks.addAll(ProjectListsManager.getNamesOfLinks());
        listViewLinks.getSelectionModel().selectFirst();
        listViewLinks.setOnMouseClicked(evt -> {
            String token = listViewLinks.getSelectionModel().getSelectedItem();
            formula.append(token);
            String fOk = formula.check();
            labelTokenValidityOfFormula.setText(Strings.validityOfFormula + fOk);
            updateTokens();
        });
        listViewLinks.getItems().addAll(namesLinks);
        // ----------------------------------------------------------------------------------
        List<String> namesObservations = new ArrayList<>();
        namesObservations.addAll(ProjectListsManager.getNamesOfObservations());
        listViewObservations.getSelectionModel().selectFirst();
        listViewObservations.setOnMouseClicked(evt -> {
            String token = listViewObservations.getSelectionModel().getSelectedItem();
            formula.append(token);
            String fOk = formula.check();
            labelTokenValidityOfFormula.setText(Strings.validityOfFormula + fOk);
            updateTokens();
        });
        listViewObservations.getItems().addAll(namesObservations);
        // ----------------------------------------------------------------------------------
        labelTokenValidityOfFormula.setMinWidth(0.45 * width);
        // ----------------------------------------------------------------------------------
        buttonClear.setOnAction((ActionEvent e) -> {
            formula.clear();
            labelTokenValidityOfFormula.setText(Strings.validityOfFormula + false);
            updateTokens();
        });
        buttonAddEditCst.setOnAction((ActionEvent e) -> addEditConstraint());
        buttonCancelCst.setOnAction((ActionEvent e) -> cancelConstraint());
        // ----------------------------------------------------------------------------------
        Insets insets = new Insets(10, 0, 0, 10);
        // ----------------------------------------------------------------------------------
        listViewYears.setDisable(selectAllYears);
        listViewYears.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
        listViewYears.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) -> setLabelYears());
        listViewYears.getItems().addAll(Context.getObservationsYears());
        // ----------------------------------------------------------------------------------
        toggleButtonAllYears.setToggleGroup(toggleGroup);
        toggleButtonSelectedYears.setToggleGroup(toggleGroup);
        toggleButtonAllYears.setSelected(true);
        toggleButtonSelectedYears.setSelected(false);

        toggleButtonAllYears.selectedProperty().addListener(
                (obs, wasPreviouslySelected, isNowSelected) -> {
                    listViewYears.setDisable(isNowSelected);
                    selectAllYears = isNowSelected;
                    toggleButtonSelectedYears.setSelected(!isNowSelected);
                    setLabelYears();
                }
        );
        toggleButtonSelectedYears.selectedProperty().addListener(
                (obs, wasPreviouslySelected, isNowSelected) -> {
                    listViewYears.setDisable(!isNowSelected);
                    selectAllYears = !isNowSelected;
                    toggleButtonAllYears.setSelected(!isNowSelected);
                    setLabelYears();
                }
        );
        // ----------------------------------------------------------------------------------
        vboxVariables.setSpacing(10);
        vboxVariables.setPadding(insets);
        vboxVariables.getChildren().addAll(
                cstNameLabel,
                cstNameTextArea,
                componentLabel,
                listViewComponents,
                linkLabel,
                listViewLinks,
                observationLabel,
                listViewObservations);
        // ----------------------------------------------------------------------------------
        hboxToggle.setSpacing(10);
        hboxToggle.setPadding(insets);
        hboxToggle.getChildren().addAll(
                toggleButtonAllYears,
                toggleButtonSelectedYears);
        // ----------------------------------------------------------------------------------
        vboxYears.setSpacing(10);
        vboxYears.setPadding(insets);
        vboxYears.getChildren().addAll(
                boxSymbols,
                hboxToggle,
                listViewYears,
                labelTokenYears,
                helpFormula
        );
        // ----------------------------------------------------------------------------------
        hboxTables.setSpacing(30);
        hboxTables.setPadding(insets);
        hboxTables.getChildren().addAll(vboxVariables, vboxYears);
        // ----------------------------------------------------------------------------------
        hboxTokensLabels.setSpacing(5);
        hboxTokensLabels.setPadding(insets);
        hboxTokensLabels.getChildren().addAll(tokensOfFormula);
        // ----------------------------------------------------------------------------------
        hboxFormulaStuff.setSpacing(5);
        hboxFormulaStuff.setPadding(insets);
        hboxFormulaStuff.getChildren().addAll(labelTokenValidityOfFormula);
        // ----------------------------------------------------------------------------------
        hboxButtons.setSpacing(5);
        hboxButtons.setPadding(insets);
        hboxButtons.getChildren().addAll(
                buttonClear,
                buttonAddEditCst,
                buttonCancelCst);
        // ----------------------------------------------------------------------------------
        vboxContent.setSpacing(10);
        vboxContent.getChildren().addAll(
                hboxTables,
                hboxTokensLabels,
                hboxFormulaStuff,
                hboxButtons);
        vboxContent.setPadding(insets);
        // ----------------------------------------------------------------------------------
        labelTokenYears.setMinWidth(0.4 * width);
        listViewYears.setMinWidth(0.4 * width);
        boxSymbols.setMinWidth(0.4 * width);
        listViewComponents.setMinWidth(0.4 * width);
        listViewLinks.setMinWidth(0.4 * width);
        listViewObservations.setMinWidth(0.4 * width);
        buttonClear.setMinWidth(200);
        buttonAddEditCst.setMinWidth(200);
        toggleButtonAllYears.setMinWidth(100);
        toggleButtonSelectedYears.setMinWidth(100);
        hboxButtons.setMinWidth(0.8 * width);

        cstNameTextArea.setMaxHeight(80);
        helpFormula.setMinHeight(100);
        boxSymbols.setMinHeight(0.15 * height);
        listViewComponents.setMinHeight(0.15 * height);
        listViewLinks.setMinHeight(0.15 * height);
        listViewObservations.setMinHeight(0.15 * height);
        listViewYears.setPrefHeight(0.15 * height);
        vboxContent.setMinHeight(height);
        // ----------------------------------------------------------------------------------
        this.setTitle("Constraint");
        this.getDialogPane().getChildren().addAll(vboxContent);
        this.getDialogPane().setMinWidth(width);
        this.getDialogPane().setMinHeight(height);
        this.getDialogPane().setStyle(ColorsAndFormats.font);
        // ----------------------------------------------------------------------------------
        this.showAndWait();
    }

    // --------------------------------------------
    final void addEditConstraint() {
        if (formula.check().equals("Ok")) {
            if(editCst){
                Constraint constraint =  new Constraint(cstNameTextArea.getText(),formula.fromTokensToString(), labelTokenYears.getText(), true, "");
                ProjectListsManager.updateConstraint(previousConstraint, constraint);
            }
            else {
                Constraint constraint = new Constraint(cstNameTextArea.getText(),formula.fromTokensToString(), labelTokenYears.getText(), true, "");
                ProjectListsManager.addConstraint(constraint, true);
            }
            window.hide();
        }
    }

    // --------------------------------------------
    final void cancelConstraint() {
           window.hide();
    }

    // ----------------------------------------------------------------------------------
    final void setLabelYears() {
        if (selectAllYears) {
            List<String> years = Context.getObservationsYears();
            labelTokenYears.setText(codeYears(years));
        } else {
            List<String> years = listViewYears.getSelectionModel().getSelectedItems();
            labelTokenYears.setText(codeYears(years));
        }
    }

    // ----------------------------------------------------------------------------------
    public static String codeYears(List<String> lYears) {
        int lY = lYears.size();
        if (lY > 0) {
            String fi = lYears.get(0);
            String la = lYears.get(lY - 1);
            int ifi = Integer.parseInt(fi);
            int ila = Integer.parseInt(la);
            if ((ila - ifi) == (lY - 1)) {
                return ("" + fi + ":" + la + "");
            }
            StringBuilder sb = new StringBuilder("c(");
            for (int y = 0; y < lYears.size(); y++) {
                sb.append(lYears.get(y));
                if (y < (lYears.size() - 1)) {
                    sb.append(", ");
                }
            }
            sb.append(")");
            return (sb.toString());
        }
        return ("");
    }

    // ----------------------------------------------------------------------------------
    public void addToken(String token) {
        formula.append(token);
        String fOk = formula.check();
        labelTokenValidityOfFormula.setText(Strings.validityOfFormula + fOk);
        updateTokens();
    }

    // --------------------------------------------
    public void updateTokens() {
        int nt = formula.getNbTokens();
        for (int t = 0; t < nt; t++) {
            Label labelToken = tokensOfFormula.get(t);
            String token = formula.getToken(t);
            labelToken.setText(token);
            labelToken.setMinWidth(5 + 10 * token.length());
            labelToken.setTextFill(Color.BLUE);
            if (t == (formula.getPos())) {
                labelToken.setTextFill(Color.RED);
            }
            labelToken.setVisible(true);
        }
        for (int t = nt; t < tokensOfFormula.size(); t++) {
            tokensOfFormula.get(t).setVisible(false);
        }
    }

    // --------------------------------------------
    public void createTokens() {
        for (int l = 0; l < 50; l++) {
            Label labelToken = new Label("");
            labelToken.setOnMouseClicked(event -> {
                Label source = (Label) event.getSource();
                int po = tokensOfFormula.indexOf(source);
                if (event.getClickCount() == 2) {
                    formula.removeAt(po);
                } else {
                    formula.setPos(po);
                 }
                String fOk = formula.check();
                labelTokenValidityOfFormula.setText(Strings.validityOfFormula + fOk);
                updateTokens();
            });
            labelToken.setBorder(new Border(new BorderStroke(Color.BLACK, BorderStrokeStyle.SOLID, CornerRadii.EMPTY, new BorderWidths(2))));
            labelToken.setBackground(new Background(new BackgroundFill(Color.LIGHTGREY, CornerRadii.EMPTY, Insets.EMPTY)));
            tokensOfFormula.add(labelToken);
        }
    }
    // --------------------------------------------
}
