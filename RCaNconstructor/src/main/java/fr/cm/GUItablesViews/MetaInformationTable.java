package fr.cm.GUItablesViews;


import fr.cm.GUIdialogs.MetaInformationSaveDialog;
import fr.cm.GUIdialogs.TextAreaDialog;
import fr.cm.RCaNMain.Context;
import fr.cm.canObjects.Action;
import fr.cm.canObjects.Constraint;
import fr.cm.canObjects.MetaElement;
import fr.cm.canObjects.ProjectListsManager;
import fr.cm.parameters.ColorsAndFormats;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;
import javafx.scene.text.Text;
import javafx.util.Callback;
import javafx.scene.control.Tooltip;

import javafx.scene.control.TableCell;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableRow;
import javafx.scene.control.TableView;


public class MetaInformationTable extends Pane {

    double width = 0.9 * Context.getWindowWidth();
    double height =  0.8 * Context.getWindowHeight();

    final TableView<MetaElement> table;
    ObservableList<MetaElement> list;

    public MetaInformationTable() {
        super();

        table = new TableView<>();
        table.setPrefWidth(width);
        table.setPrefHeight(height);
        table.setEditable(true);

        table.setOnMouseClicked(click -> {
            if (click.getClickCount() == 2) {
                @SuppressWarnings("rawtypes")
                TablePosition pos = table.getSelectionModel().getSelectedCells().get(0);
                int row = pos.getRow();
                int col = pos.getColumn();
                @SuppressWarnings("rawtypes")
                TableColumn column = pos.getTableColumn();
                String oComment = column.getCellData(row).toString();
                if(col==1){
                    new TextAreaDialog("Edit answer",oComment);
                    String nComment = Context.getTextAreaContent();
                    System.out.println("Clic on: "+ row
                            + " " + oComment
                            + " " + nComment
                            + " " + nComment.equals(oComment)
                    );
                    if(!nComment.equals(oComment)) {
                        System.out.println("Clic");
                        ProjectListsManager.updateMetaElement(row, nComment);
                        table.refresh();
                    }
                }
            }
        });

        table.setRowFactory(tv -> new TableRow<>() {
            private final Tooltip tooltip = new Tooltip();
            @Override
            public void updateItem(MetaElement metaElement, boolean empty) {
                super.updateItem(metaElement, empty);
                if (metaElement == null) {
                    setTooltip(null);
                } else {
                    tooltip.setPrefWidth(0.3 * width);
                    tooltip.setWrapText(true);
                    tooltip.setText(metaElement.getMetaHint());
                    setTooltip(tooltip);
                }
            }
        });
        // -------------------------------------------------------------------------
        TableColumn<MetaElement, String> metaNameCol = new TableColumn<>("Field");
        metaNameCol.setCellValueFactory(new PropertyValueFactory<>("metaName"));
        metaNameCol.setEditable(false);
        metaNameCol.setMinWidth(0.35*width);
        @SuppressWarnings("rawtypes")
        Callback cellFactoryMeta = new Callback<TableColumn, TableCell>() {
            @Override
            public TableCell call(TableColumn param) {
                @SuppressWarnings("rawtypes")
                TableCell tableCell = new TableCell() {
                    public void updateItem(Object item, boolean empty) {
                        super.updateItem(item, empty);
                        if (!isEmpty()) {
                            String string = item.toString();
                            Font font;
                            if (string.startsWith("<")) {
                                font = Font.font("Verdana", FontWeight.BOLD, 15);
                                string = string.
                                        replace("<", "").
                                        replace(">", "");
                            } else {
                                font = Font.font("Verdana", FontWeight.NORMAL, 15);
                            }
                            Text text = new Text(string);
                            text.setFont(font);
                            text.wrappingWidthProperty().bind(getTableColumn().widthProperty().subtract(35));
                            setGraphic(text);
                        }
                    }
                };
                return tableCell;
            }
        };
        metaNameCol.setCellFactory(cellFactoryMeta);
        table.getColumns().add(metaNameCol);
        // -------------------------------------------------------------------------
        TableColumn<MetaElement, String> metaContentCol = new TableColumn<>("Your answers");
        metaContentCol.setCellValueFactory(new PropertyValueFactory<>("metaContent"));
        metaContentCol.setMinWidth(0.5*width);
        metaContentCol.setCellFactory(tc -> {
            TableCell<MetaElement, String> cell = new TableCell<>();
            Text text = new Text();
            cell.setGraphic(text);
            text.wrappingWidthProperty().bind(metaContentCol.widthProperty());
            text.textProperty().bind(cell.itemProperty());
            return cell ;
        });
        metaContentCol.setEditable(false);
        table.getColumns().add(metaContentCol);
        // ------------------------------------------------------------------------
        // -------------------------------------------------------------------------
        list = FXCollections.observableArrayList(ProjectListsManager.getMetaInformation().getElements());
        table.setItems(list);
        table.getSelectionModel().selectFirst();

        final Label title = new Label("Meta Information");
        title.setFont(ColorsAndFormats.titleFont);
        final Button button = new Button("Save as text file");
        button.setOnAction((ActionEvent e) -> new MetaInformationSaveDialog());
        final HBox hbox = new HBox(50);
        hbox.getChildren().addAll(title, button);

        final VBox vbox = new VBox();
        ColorsAndFormats.setVBoxCharacteristics(vbox);
        vbox.getChildren().addAll(hbox, table);
        this.getChildren().addAll(vbox);

    }
}
