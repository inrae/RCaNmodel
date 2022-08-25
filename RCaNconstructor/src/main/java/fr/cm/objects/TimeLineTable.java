package fr.cm.objects;


import fr.cm.dialogs.TextAreaDialog;
import fr.cm.Main.Context;
import fr.cm.project.ProjectListsManager;
import fr.cm.preferences.ColorsAndFormats;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;
import javafx.scene.text.Text;

public class TimeLineTable extends Pane {
    // ---------------------------------

    final TableView<TimeLine> table;
    ObservableList<TimeLine> list;
    double width = Context.getWindowWidth();
    double height =  Context.getWindowHeight();

    public TimeLineTable() {
        table = new TableView<>();
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
        TableColumn<TimeLine, String> dateCol = new TableColumn<>("Date");
        dateCol.setSortable(true);
        dateCol.setCellValueFactory(new PropertyValueFactory<>("date"));
        dateCol.setMinWidth(0.12 *width);
        // ------------------------------------------------------------------------
        TableColumn<TimeLine, String> whichTimeLineCol = new TableColumn<>("Task");
        whichTimeLineCol.setCellValueFactory(new PropertyValueFactory<>("whichTimeLine"));
        whichTimeLineCol.setSortable(true);
        whichTimeLineCol.setCellFactory(tc -> {
            TableCell<TimeLine, String> cell = new TableCell<>();
            Text text = new Text();
            cell.setGraphic(text);
            text.wrappingWidthProperty().bind(whichTimeLineCol.widthProperty());
            text.textProperty().bind(cell.itemProperty());
            return cell ;
        });
        whichTimeLineCol.setEditable(false);
        whichTimeLineCol.setMinWidth(0.34 *width);
        // ------------------------------------------------------------------------
        TableColumn<TimeLine, String> commentAuthorCol = new TableColumn<>("Annotation");
        commentAuthorCol.setCellValueFactory(new PropertyValueFactory<>("commentAuthor"));
        commentAuthorCol.setMinWidth((0.34 *width));
        commentAuthorCol.setEditable(false);
        commentAuthorCol.setCellFactory(tc -> {
            TableCell<TimeLine, String> cell = new TableCell<>();
            Text text = new Text();
            cell.setGraphic(text);
            text.wrappingWidthProperty().bind(whichTimeLineCol.widthProperty());
            text.textProperty().bind(cell.itemProperty());
            return cell ;
        });
        whichTimeLineCol.setEditable(false);
        // ------------------------------------------------------------------------
        table.getColumns().add(dateCol);
        table.getColumns().add(whichTimeLineCol);
        table.getColumns().add(commentAuthorCol);

        list = FXCollections.observableArrayList(ProjectListsManager.getListOfTimeLines());
        table.setItems(list);
        table.getSelectionModel().selectFirst();
        table.setPrefWidth(0.8*width);
        table.setPrefHeight(0.7*height);

        final Label title = new Label("RCaN Tasks");
        title.setFont(ColorsAndFormats.titleFont);
        final Button button = new Button("Save as text file");
        final Label hint = new Label("Double clic on an annotation to edit it");
        button.setOnAction((ActionEvent e) -> new TimeLineSaveDialog());
        final Button buttonNewAnnotation = new Button("Add time line");
        buttonNewAnnotation.setOnAction((ActionEvent e) -> {
            new TextAreaDialog("Add annotation","");
            String nComment = Context.getTextAreaContent();
            TimeLine timeLine = new TimeLine("Added by author",nComment);
            timeLine.print();
            ProjectListsManager.addTimeLine(timeLine,false);
            list = FXCollections.observableArrayList(ProjectListsManager.getListOfTimeLines());
            table.setItems(list);
            table.refresh();
        });

        final HBox hboxButtons = new HBox();
        hboxButtons.getChildren().addAll( button, buttonNewAnnotation, hint);
        hboxButtons.setSpacing(80);

        final VBox vbox = new VBox();
        ColorsAndFormats.setVBoxCharacteristics(vbox);
        vbox.getChildren().addAll(title, table, hboxButtons);
        ColorsAndFormats.setVBoxCharacteristics(vbox);

        this.getChildren().addAll(vbox);
    }

}
