package fr.cm.GUItablesViews;


import fr.cm.GUIdialogs.MetaInformationSaveTextDialog;
import fr.cm.GUIdialogs.TextAreaDialog;
import fr.cm.RCaNMain.Context;
import fr.cm.canObjects.MetaElement;
import fr.cm.ProjectManager.ProjectListsManager;
import fr.cm.parameters.ColorsAndFormats;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.scene.control.*;
import javafx.scene.control.cell.PropertyValueFactory;
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

    double width = Context.getWindowWidth();
    double height =  Context.getWindowHeight();

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
                if(col==1 && ProjectListsManager.metaInformationType(row)){
                    new TextAreaDialog("Edit annotation",oComment);
                    String nComment = Context.getTextAreaContent();
                    if(!nComment.equals(oComment)) {
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
        metaNameCol.setSortable(false);
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
        TableColumn<MetaElement, String> metaContentCol = new TableColumn<>("Annotations");
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
        metaContentCol.setSortable(false);
        table.getColumns().add(metaContentCol);
        // ------------------------------------------------------------------------
        // -------------------------------------------------------------------------
        list = FXCollections.observableArrayList(ProjectListsManager.getListOfMetaElements());
        table.setItems(list);
        table.getSelectionModel().selectFirst();
        table.setPrefWidth(0.8*width);
        table.setPrefHeight(0.7*height);

        final Label title = new Label("Project Information");
        final Label hint = new Label("A tooltip gives advices about what is expected. Double clic on an annotation to edit it");
        title.setFont(ColorsAndFormats.titleFont);
        final Button button = new Button("Save as text file");
        button.setOnAction((ActionEvent e) -> new MetaInformationSaveTextDialog());
        final HBox hboxBottom = new HBox(50);
        hboxBottom.getChildren().addAll(title, button, hint);

        final VBox vbox = new VBox();
        vbox.getChildren().addAll(title, table,hboxBottom);
        ColorsAndFormats.setVBoxCharacteristics(vbox);
        this.getChildren().addAll(vbox);
    }
}
