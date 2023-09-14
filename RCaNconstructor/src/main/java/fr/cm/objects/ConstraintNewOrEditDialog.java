/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package fr.cm.objects;

import fr.cm.Main.ObjectsManager;
import fr.cm.Main.Context;
import fr.cm.preferences.ColorsAndFormats;
import fr.cm.preferences.Strings;
import javafx.event.ActionEvent;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
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

    ConstraintFormula formula ;
    List<Label> tokensOfFormula = new ArrayList<>();
    double width = 0.7 * Context.getWindowWidth(), height =  Context.getWindowHeight();
    boolean editCst;
    final Window window;
    Constraint previousConstraint;
    Button buttonClear = new Button(Strings.clearFormula), buttonAddEditCst = new Button(Strings.addConstraint), buttonCancelCst = new Button(Strings.cancelConstraint);
    final ListView<String> listViewYears = new ListView<>(), listViewComponents = new ListView<>(),  listViewLinks = new ListView<>(),  listViewObservations = new ListView<>();
    boolean selectAllYears;
    final Label labelTokenYears = new Label(""),  labelTokenValidityOfFormula = new Label("");
    final TextArea cstNameTextArea = new TextArea("");
    // ----------------------------------------------------------------------------------
     final HBox boxToggle = new HBox();
    String cstName;
    Insets insets = new Insets(10, 0, 0, 10);
    public ConstraintNewOrEditDialog() {
        editCst = false;
        buttonAddEditCst.setText(Strings.addConstraint);
        window = this.getDialogPane().getScene().getWindow();
        window.setOnCloseRequest(event -> window.hide());
        selectAllYears = true;
        // ----------------------------------------------------------------------------------
        createTokens();
        formula = new ConstraintFormula();
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
        formula = new ConstraintFormula(constraint);
        updateTokens();
        // ----------------------------------------------------------------------------------
        cstName = constraint.getName();
        String fOk = formula.check();
        labelTokenValidityOfFormula.setText(Strings.validityOfFormula + fOk);
        initDialog();
    }

    VBox initLeft() {
        // ----------------------------------------------------------------------------------
        Label componentLabel = new Label("Components");
        Label linkLabel = new Label("Links");
        Label observationLabel = new Label("Observations");
        Label cstNameLabel = new Label("Name of constraint");
        // COMPONENTS ----------------------------------------------------------------------------------
        VBox boxTableLeft = new VBox();
        List<String> namesComponents = new ArrayList<>();
        namesComponents.addAll(ObjectsManager.getNamesOfComponents());
        listViewComponents.getSelectionModel().selectFirst();
        listViewComponents.setOnMouseClicked(evt -> {
            String token = listViewComponents.getSelectionModel().getSelectedItem();
            formula.append(token);
            String fOk = formula.check();
            labelTokenValidityOfFormula.setText(Strings.validityOfFormula + fOk);
            updateTokens();
        });
        listViewComponents.getItems().addAll(namesComponents);
        // LINKS ----------------------------------------------------------------------------------
        List<String> namesLinks = new ArrayList<>();
        namesLinks.addAll(ObjectsManager.getNamesOfLinks());
        listViewLinks.getSelectionModel().selectFirst();
        listViewLinks.setOnMouseClicked(evt -> {
            String token = listViewLinks.getSelectionModel().getSelectedItem();
            formula.append(token);
            String fOk = formula.check();
            labelTokenValidityOfFormula.setText(Strings.validityOfFormula + fOk);
            updateTokens();
        });
        listViewLinks.getItems().addAll(namesLinks);
        // OBSERVATIONS ----------------------------------------------------------------------------------
        List<String> namesObservations = new ArrayList<>();
        namesObservations.addAll(ObjectsManager.getNamesOfObservations());
        listViewObservations.getSelectionModel().selectFirst();
        listViewObservations.setOnMouseClicked(evt -> {
            String token = listViewObservations.getSelectionModel().getSelectedItem();
            formula.append(token);
            String fOk = formula.check();
            labelTokenValidityOfFormula.setText(Strings.validityOfFormula + fOk);
            updateTokens();
        });
        listViewObservations.getItems().addAll(namesObservations);
        boxTableLeft.setSpacing(10);
        boxTableLeft.setPadding(insets);
        boxTableLeft.setMinWidth((0.6 * width));
        boxTableLeft.setMinHeight((0.7 * height));
        boxTableLeft.getChildren().addAll(
                cstNameLabel,
                cstNameTextArea,
                componentLabel,
                listViewComponents,
                linkLabel,
                listViewLinks,
                observationLabel,
                listViewObservations);
        return(boxTableLeft);
    }
    VBox initRight(){
        // ----------------------------------------------------------------------------------
        VBox boxTableRight = new VBox();
        GridPane boxSymbols = new GridPane();
        GridPane boxShortcuts = new GridPane();
        // SYMBOLES ----------------------------------------------------------------------------------
        List<String> allSymbols = new ArrayList<>();
        allSymbols.addAll(ConstraintFormula.nombres);
        allSymbols.addAll(ConstraintFormula.symbolsAndOperators);

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
        // SHORTCUTS ----------------------------------------------------------------------------------
        List<String> allShortcuts = new ArrayList<>();
        allShortcuts.addAll(ConstraintFormula.shortcuts);

        for (int s = 0; s < allShortcuts.size(); s++) {
            Button button = new Button(allShortcuts.get(s));
            button.setOnAction((ActionEvent e) -> {
                String token = button.getText();
                addToken(token);
            });
            button.setMinWidth(180);
            int col = s % 2 ;
            int lig = s / 2;
            boxShortcuts.add(button, col, lig);
        }
        // YEARS ----------------------------------------------------------------------------------
        labelTokenYears.setFont(new Font("Arial", 15));
        labelTokenYears.setBorder(new Border(new BorderStroke(Color.BLACK, BorderStrokeStyle.SOLID, CornerRadii.EMPTY, new BorderWidths(2))));
        labelTokenYears.setBackground(new Background(new BackgroundFill(Color.LIGHTGREY, CornerRadii.EMPTY, Insets.EMPTY)));

        setLabelYears();
        listViewYears.setDisable(selectAllYears);
        listViewYears.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
        listViewYears.getSelectionModel().selectedItemProperty().addListener((observable, oldValue, newValue) -> setLabelYears());
        listViewYears.getItems().addAll(Context.getObservationsYears());

        ToggleGroup toggleGroup = new ToggleGroup();
        ToggleButton toggleButtonAllYears = new ToggleButton("All years");
        ToggleButton toggleButtonSelectedYears = new ToggleButton("Select years");
        toggleButtonAllYears.setToggleGroup(toggleGroup);
        toggleButtonSelectedYears.setToggleGroup(toggleGroup);
        toggleButtonAllYears.setSelected(true);
        toggleButtonSelectedYears.setSelected(false);

        boxToggle.setSpacing(10);
        boxToggle.setPadding(insets);
        boxToggle.getChildren().addAll(
                toggleButtonAllYears,
                toggleButtonSelectedYears);

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
        // TOGETHER ----------------------------------------------------------------------------------
        boxTableRight.setSpacing(10);
        boxTableRight.setPadding(insets);
        boxTableRight.setMinWidth((0.4 * width));
        boxTableRight.setMinHeight((0.7 * height));
        boxTableRight.getChildren().addAll(
                boxSymbols,
                // boxShortcuts,
                boxToggle,
                listViewYears,
                labelTokenYears
        );
        return(boxTableRight);

    }
    // --------------------------------------------
    void initDialog(){
        // ----------------------------------------------------------------------------------
        VBox boxTableLeft = initLeft();
        VBox boxTableRight = initRight();
        HBox boxTables = new HBox();
        HBox boxTokens = new HBox();
        HBox boxButtons = new HBox();
        VBox boxContent = new VBox();
        // ----------------------------------------------------------------------------------
        boxTables.setSpacing(30);
        boxTables.setPadding(insets);
        boxTables.setAlignment(Pos.CENTER);
        boxTables.getChildren().addAll(boxTableLeft, boxTableRight);
        // ----------------------------------------------------------------------------------
        cstNameTextArea.setText(cstName);
        cstNameTextArea.setMaxHeight(80);
        // ----------------------------------------------------------------------------------
        Label helpFormula = new Label(Strings.helpFormula);
        helpFormula.setWrapText(true);
        helpFormula.setMinWidth((0.7 * width));
        helpFormula.setMinHeight(150);
        helpFormula.setAlignment(Pos.CENTER);
        // ----------------------------------------------------------------------------------
        boxTokens.setSpacing(5);
        boxTokens.setPadding(insets);
        boxTokens.setMinWidth((0.7 * width));
        boxTokens.setAlignment(Pos.CENTER);
        boxTokens.getChildren().addAll(tokensOfFormula);
        // ----------------------------------------------------------------------------------
        labelTokenValidityOfFormula.setMinWidth(0.7 * width);
        labelTokenValidityOfFormula.setPadding(insets);
        labelTokenValidityOfFormula.setAlignment(Pos.CENTER);
        // ----------------------------------------------------------------------------------
        // ----------------------------------------------------------------------------------
        buttonClear.setOnAction((ActionEvent e) -> {
            formula.clear();
            labelTokenValidityOfFormula.setText(Strings.validityOfFormula + false);
            updateTokens();
        });
        buttonAddEditCst.setOnAction((ActionEvent e) -> addEditConstraint());
        buttonCancelCst.setOnAction((ActionEvent e) -> cancelConstraint());
        boxButtons.setSpacing(5);
        boxButtons.setPadding(insets);
        boxButtons.setMinWidth((0.7 * width));
        boxButtons.setAlignment(Pos.CENTER);
        boxButtons.getChildren().addAll(
                buttonClear,
                buttonAddEditCst,
                buttonCancelCst);
        // ----------------------------------------------------------------------------------
        boxContent.setSpacing(10);
        boxContent.getChildren().addAll(
                boxTables,
                helpFormula,
                boxTokens,
                labelTokenValidityOfFormula,
                boxButtons);
        boxContent.setPadding(insets);
        // ----------------------------------------------------------------------------------
        this.setTitle("Constraint");
        this.getDialogPane().getChildren().addAll(boxContent);
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
                ObjectsManager.updateConstraint(previousConstraint, constraint);
            }
            else {
                Constraint constraint = new Constraint(cstNameTextArea.getText(),formula.fromTokensToString(), labelTokenYears.getText(), true, "");
                ObjectsManager.addConstraint(constraint, true);
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
