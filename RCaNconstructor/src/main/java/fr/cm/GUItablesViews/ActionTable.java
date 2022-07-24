package fr.cm.GUItablesViews;


import fr.cm.GUIdialogs.ActionSaveDialog;
import fr.cm.GUIdialogs.TextAreaDialog;
import fr.cm.RCaNMain.Context;
import fr.cm.canObjects.Action;
import fr.cm.canObjects.ProjectListsManager;
import fr.cm.parameters.ColorsAndFormats;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;

public class ActionTable extends Pane {
    final TableView<Action> table;
    ObservableList<Action> list;
    double width = 0.9 * Context.getWindowWidth();
    double height =  0.8 * Context.getWindowHeight();

    public ActionTable() {
        table = new TableView<>();
        table.setMinWidth(width);
        table.setMinHeight(height);
        table.setEditable(true);
        table.getSelectionModel().setCellSelectionEnabled(true);  // selects cell only, not the whole row
        table.setOnMouseClicked(click -> {
            if (click.getClickCount() == 2) {
                @SuppressWarnings("rawtypes")
                TablePosition pos = table.getSelectionModel().getSelectedCells().get(0);
                int row = pos.getRow();
                int col = pos.getColumn();
                @SuppressWarnings("rawtypes")
                TableColumn column = pos.getTableColumn();
                String oComment = column.getCellData(row).toString();
                if(col==2){
                    new TextAreaDialog("Edit annotation",oComment);
                    String nComment = Context.getTextAreaContent();
                    if(!nComment.equals(oComment)) {
                        ProjectListsManager.updateAction(row, nComment);
                        table.refresh();
                    }
                }
            }
        });
         // ------------------------------------------------------------------------
        TableColumn<Action, String> dateCol = new TableColumn<>("Date");
        dateCol.setCellValueFactory(new PropertyValueFactory<>("date"));
        dateCol.setMinWidth(0.16 *width);
        // ------------------------------------------------------------------------
        TableColumn<Action, String> whichActionCol = new TableColumn<>("Task");
        whichActionCol.setCellValueFactory(new PropertyValueFactory<>("whichAction"));
        // whichActionCol.setCellFactory(TextFieldTableCell.forTableColumn());
        whichActionCol.setCellFactory(tc -> {
            TableCell<Action, String> cell = new TableCell<>();
            Text text = new Text();
            cell.setGraphic(text);
            // cell.setPrefHeight(Control.USE_COMPUTED_SIZE);
            text.wrappingWidthProperty().bind(whichActionCol.widthProperty());
            text.textProperty().bind(cell.itemProperty());
            return cell ;
        });
        whichActionCol.setEditable(false);
        whichActionCol.setMinWidth(0.4 *width);
        // ------------------------------------------------------------------------
        TableColumn<Action, String> commentAuthorCol = new TableColumn<>("Annotation");
        // commentAuthorCol.setCellFactory(cellFactory);
        commentAuthorCol.setCellValueFactory(new PropertyValueFactory<>("commentAuthor"));
        commentAuthorCol.setMinWidth((0.4 *width));
        commentAuthorCol.setEditable(false);
        commentAuthorCol.setCellFactory(tc -> {
            TableCell<Action, String> cell = new TableCell<>();
            Text text = new Text();
            cell.setGraphic(text);
            text.wrappingWidthProperty().bind(whichActionCol.widthProperty());
            text.textProperty().bind(cell.itemProperty());
            return cell ;
        });
        whichActionCol.setEditable(false);
        // ------------------------------------------------------------------------
        table.getColumns().add(dateCol);
        table.getColumns().add(whichActionCol);
        table.getColumns().add(commentAuthorCol);

        list = FXCollections.observableArrayList(ProjectListsManager.getListOfActions());
        table.setItems(list);
        table.getSelectionModel().selectFirst();

        final Label title = new Label("RCaN Tasks");
        title.setFont(ColorsAndFormats.titleFont);
        final Button button = new Button("Save as text file");
        button.setOnAction((ActionEvent e) -> new ActionSaveDialog());
        final HBox hbox = new HBox(50);
        hbox.getChildren().addAll(title, button);

        final VBox vbox = new VBox();
        ColorsAndFormats.setVBoxCharacteristics(vbox);
        vbox.getChildren().addAll(hbox, table);
        this.getChildren().addAll(vbox);
    }

}
